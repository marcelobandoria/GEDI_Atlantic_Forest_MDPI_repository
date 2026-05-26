# -----------------------------------------------------------------------------
# Script 04 — Run supplementary QC and acquisition-sensitivity analyses
#
# Project: GEDI-derived forest structure in Atlantic Forest physiognomies
# Repository workflow: manuscript and supplementary-data reproducibility
# Author: Marcelo C. S. Bandoria
#
# Purpose
# This script reproduces the supplementary robustness workflow reported in the
# Supplementary Information. It assigns GEDI footprints to sampling polygons by
# spatial join, applies the 60 m internal edge filter under alternative QC and
# acquisition scenarios, and exports scenario-wise Kruskal-Wallis, Dunn,
# correlation, LMM, Moran's I, and effect-size delta outputs.
#
# Main outputs
#   results/supplementary/supplement_qc_sensitivity.xlsx
#   results/supplementary/*.csv used by Scripts 05 and 06
#
# Run after Script 02. Use GEDI_FAST_TEST=true only to check execution quickly.
# -----------------------------------------------------------------------------

# --------------------------- REPOSITORY ROOT ---------------------------------
# The scripts are stored in scripts/. This block makes paths work whether you run
# them from the repository root or from inside the scripts/ folder.
.detect_repo_root <- function() {
  candidates <- character(0)

  cwd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  candidates <- c(candidates, cwd, dirname(cwd), dirname(dirname(cwd)))

  # When sourced, R often stores the script path in sys.frames()[[i]]$ofile.
  ofiles <- vapply(sys.frames(), function(x) {
    if (!is.null(x$ofile)) as.character(x$ofile)[1] else NA_character_
  }, character(1))
  ofiles <- stats::na.omit(ofiles)
  if (length(ofiles) > 0) {
    script_dir <- dirname(normalizePath(tail(ofiles, 1), winslash = "/", mustWork = FALSE))
    candidates <- c(candidates, script_dir, dirname(script_dir))
  }

  # When executed with Rscript --file=...
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    script_file <- sub("^--file=", "", file_arg[1])
    script_dir <- dirname(normalizePath(script_file, winslash = "/", mustWork = FALSE))
    candidates <- c(candidates, script_dir, dirname(script_dir))
  }

  candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))

  valid <- candidates[
    dir.exists(file.path(candidates, "scripts")) &
      (dir.exists(file.path(candidates, "data")) | dir.exists(file.path(candidates, "spatial")) | dir.exists(file.path(candidates, "results")))
  ]
  if (length(valid) > 0) return(valid[1])

  # Fallback: if current working directory is scripts/, use its parent.
  if (basename(cwd) == "scripts") return(dirname(cwd))

  cwd
}

repo_root <- .detect_repo_root()
repo_path <- function(...) file.path(repo_root, ...)
fast_test <- tolower(Sys.getenv("GEDI_FAST_TEST", "false")) %in% c("true", "1", "yes", "sim")
message("Repository root detected: ", repo_root)

# --------------------------- 0) OPTIONS --------------------------------------
opt <- list(
  path_csv          = repo_path("data", "gedi_footprints_filtered.csv"),
  path_csv_fallback = NULL,
  poly_path          = repo_path("spatial", "old_growth_candidate_polygons.gpkg"),
  poly_path_fallback = NULL,
  poly_layer         = "atual__samples",

  id_col_csv  = "id_amostra",
  fito_col    = "fitofisionomia",
  lat_col     = "latitude_bin0",
  lon_col     = "longitude_bin0",
  poly_id_col = "id",
  join_method = "within",

  buffer_m    = 60,
  crs_metrico = 3857,
  vars_key = c("altura_dossel", "agbd", "pai", "cover", "fhd_normal"),

  qc_cols = list(
    qf_2b   = "l2b_quality_flag",
    dg_2b   = "degrade_flag_2b",
    sens_2b = "sensitivity_2b",
    beam_2b = "beam_2b",
    sol_2b  = "solar_elevation_2b",
    qf_4a   = "l4_quality_flag",
    dg_4a   = "degrade_flag_4a",
    sens_4a = "sensitivity_4a",
    beam_4a = "beam_4a",
    sol_4a  = "solar_elevation_4a"
  ),
  qc_values = list(quality_ok = 1, degrade_ok = 0),

  sensitivity_thresholds = c(0.80, 0.90, 0.95, 0.99),

  scenario_flags = list(
    include_no_qc = TRUE,
    include_night_only = TRUE,
    include_strong_beams = TRUE,
    include_agbd_uncertainty = TRUE,
    include_fixed_shots_per_polygon = TRUE
  ),

  fixed_shots = list(
    m = 10,
    replace = TRUE,
    run_for_scenarios = c("S1_baseline", "S_sens095")
  ),

  bootstrap = list(
    seed = 2025,
    replace_polygons = TRUE,
    n_boot_baseline = 2000,
    n_boot_alt = 500
  ),

  spatial = list(
    run_baseline = TRUE,
    run_moran = TRUE,
    run_variogram = TRUE,
    max_shots_per_group_lmm = 2500,
    seed_lmm_sample = 123
  ),

  dir_out   = repo_path("results", "supplementary"),
  xlsx_file = repo_path("results", "supplementary", "supplement_qc_sensitivity.xlsx")
)

if (isTRUE(fast_test)) {
  opt$bootstrap$n_boot_baseline <- 20
  opt$bootstrap$n_boot_alt <- 10
  opt$spatial$run_variogram <- FALSE
  message("GEDI_FAST_TEST=true: supplementary QC bootstrap iterations were reduced for a quick execution check.")
}

# --------------------------- 1) PACKAGES -------------------------------------
need <- c(
  "readr", "dplyr", "tidyr", "stringr", "janitor", "purrr", "tibble",
  "FSA", "openxlsx", "sf", "rlang", "lme4", "lmerTest", "spdep", "sp"
)

to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(need, require, character.only = TRUE))
options(dplyr.summarise.inform = FALSE)

# --------------------------- 2) HELPERS --------------------------------------
mk_outdir <- function() if (!dir.exists(opt$dir_out)) dir.create(opt$dir_out, recursive = TRUE, showWarnings = FALSE)
msg <- function(...) cat(paste0(..., "\n"))
resolve_path <- function(primary, fallback = NULL) {
  if (file.exists(primary)) return(primary)
  if (!is.null(fallback) && file.exists(fallback)) return(fallback)
  stop("Input file not found. Tried: ", primary, if (!is.null(fallback)) paste0(" and ", fallback) else "")
}
safe_sheet_name <- function(nm, existing = character(0)) {
  nm <- gsub("[:\\\\/?*\\[\\]]", "_", nm)
  nm <- trimws(ifelse(is.na(nm) | nm == "", "Sheet", nm))
  maxlen <- 31
  if (nchar(nm) > maxlen) nm <- substr(nm, 1, maxlen)
  if (nm %in% existing) {
    base <- substr(nm, 1, maxlen - 3); i <- 1
    repeat {
      candidate <- paste0(base, "_", sprintf("%02d", i))
      if (!(candidate %in% existing)) { nm <- candidate; break }
      i <- i + 1; if (i > 99) stop("Too many repeated sheet names.")
    }
  }
  nm
}

to_ascii_upper <- function(x) toupper(iconv(x, from = "", to = "ASCII//TRANSLIT"))
extrair_fito_grupo <- function(x) {
  xu <- to_ascii_upper(as.character(x))
  pref <- stringr::str_match(xu, "^(FOD|FOM|FES|FED)")[, 2]
  anyw <- ifelse(is.na(pref), stringr::str_match(xu, "(FOD|FOM|FES|FED)")[, 1], pref)
  miss <- which(is.na(anyw))
  if (length(miss)) {
    xr <- xu[miss]
    anyw[miss] <- ifelse(grepl("OMBROFILA\\s*DENSA|FLORESTA\\s*OMBROFILA\\s*DENSA", xr), "FOD",
                         ifelse(grepl("OMBROFILA\\s*MISTA|ARAUCARIA", xr), "FOM",
                                ifelse(grepl("ESTACIONAL\\s*SEMIDECIDUAL|SEMIDECIDUAL", xr), "FES",
                                       ifelse(grepl("ESTACIONAL\\s*DECIDUAL|DECIDUAL", xr), "FED", NA))))
  }
  factor(anyw, levels = c("FOD", "FOM", "FES", "FED"))
}

fito_sig_en <- c(FOD = "DOF", FOM = "MOF", FES = "SSdF", FED = "SDF")
fito_nome_en <- c(
  FOD = "Dense Ombrophilous Forest (DOF)",
  FOM = "Mixed Ombrophilous Forest (MOF)",
  FES = "Seasonal Semideciduous Forest (SSdF)",
  FED = "Seasonal Deciduous Forest (SDF)"
)
std_comparacao_en <- function(x) {
  x <- as.character(x)
  vapply(x, function(comp) {
    if (is.na(comp) || !nzchar(comp)) return(comp)
    parts <- strsplit(comp, "\\s*-\\s*")[[1]]
    if (length(parts) != 2) return(comp)
    a <- dplyr::recode(parts[1], !!!fito_sig_en, .default = parts[1])
    b <- dplyr::recode(parts[2], !!!fito_sig_en, .default = parts[2])
    paste0(a, "\u2013", b)
  }, character(1))
}
cover_to_percent <- function(x) {
  if (!is.numeric(x)) return(x)
  vmax <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(vmax)) return(x)
  if (vmax <= 1.05) 100 * x else x
}
calc_altura_dossel <- function(elev_highestreturn, elev_lowestmode_2b, elev_lowestmode_4a = NULL) {
  elev_highestreturn - dplyr::coalesce(elev_lowestmode_2b, elev_lowestmode_4a)
}
na_outside <- function(x, minv, maxv) {
  if (!is.numeric(x)) return(x)
  x[!is.finite(x)] <- NA_real_
  x[x < minv | x > maxv] <- NA_real_
  x
}
epsilon2_kw <- function(H, n, k) max((as.numeric(H) - k + 1) / (n - k), 0)

compute_dunn_tables <- function(data_long, var, ajuste = c("bonferroni", "bh")) {
  sub <- data_long %>% dplyr::select(fito_grupo, !!rlang::sym(var)) %>% dplyr::filter(is.finite(.data[[var]])) %>% droplevels()
  if (nrow(sub) == 0 || dplyr::n_distinct(sub$fito_grupo) < 2) return(NULL)
  res <- list()
  if ("bonferroni" %in% ajuste) {
    rb <- try(FSA::dunnTest(as.formula(paste(var, "~ fito_grupo")), data = sub, method = "bonferroni")$res, silent = TRUE)
    if (!inherits(rb, "try-error")) { rb$variavel <- var; rb$ajuste <- "Bonferroni"; res <- c(res, list(rb)) }
  }
  if ("bh" %in% ajuste) {
    rf <- try(FSA::dunnTest(as.formula(paste(var, "~ fito_grupo")), data = sub, method = "bh")$res, silent = TRUE)
    if (!inherits(rf, "try-error")) { rf$variavel <- var; rf$ajuste <- "FDR"; res <- c(res, list(rf)) }
  }
  if (!length(res)) return(NULL)
  dplyr::bind_rows(res) %>% dplyr::rename(comparacao = Comparison, z = Z, p_unadj = P.unadj, p_adj = P.adj)
}
calc_r_from_dunn <- function(dunn_tbl, n_tab) {
  if (is.null(dunn_tbl) || !nrow(dunn_tbl)) return(tibble::tibble())
  dunn_tbl %>%
    tidyr::separate(comparacao, into = c("grp1", "grp2"), sep = "\\s*-\\s*", remove = FALSE) %>%
    dplyr::left_join(n_tab %>% dplyr::rename(grp1 = fito_grupo, n1 = n_grupo), by = "grp1") %>%
    dplyr::left_join(n_tab %>% dplyr::rename(grp2 = fito_grupo, n2 = n_grupo), by = "grp2") %>%
    dplyr::mutate(
      N_ref = n1 + n2,
      r = as.numeric(abs(z)) / sqrt(as.numeric(N_ref)),
      comparacao_en = std_comparacao_en(comparacao)
    ) %>%
    dplyr::select(variavel, comparacao, comparacao_en, ajuste, z, N_ref, r, p_adj, p_unadj)
}
balance_once <- function(dd, group_col, n_target, replace = TRUE) {
  dd %>% dplyr::group_by(!!rlang::sym(group_col)) %>% dplyr::group_modify(~ dplyr::slice_sample(.x, n = n_target, replace = replace)) %>% dplyr::ungroup()
}

safe_save_workbook <- function(wb, target_path) {
  out_dir <- dirname(target_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  try_save <- function(path) {
    if (file.exists(path)) try(file.remove(path), silent = TRUE)
    ok <- try(openxlsx::saveWorkbook(wb, path, overwrite = TRUE), silent = TRUE)
    if (!inherits(ok, "try-error")) return(list(ok = TRUE, path = path, err = NULL))
    list(ok = FALSE, path = path, err = as.character(ok))
  }
  r1 <- try_save(target_path)
  if (r1$ok) return(r1$path)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  alt1 <- file.path(out_dir, paste0(tools::file_path_sans_ext(basename(target_path)), "_", stamp, ".xlsx"))
  r2 <- try_save(alt1)
  if (r2$ok) return(r2$path)
  alt2 <- file.path(tempdir(), paste0("supplement_qc_sensitivity_", stamp, ".xlsx"))
  r3 <- try_save(alt2)
  if (r3$ok) return(r3$path)
  stop("Failed to save workbook. Last error: ", r3$err)
}

assign_polygon_id_by_join <- function(df, pol, opt) {
  if (opt$id_col_csv %in% names(df)) df <- df %>% dplyr::rename(id_amostra_csv = !!rlang::sym(opt$id_col_csv)) else df$id_amostra_csv <- NA_character_
  pts <- sf::st_as_sf(df, coords = c(opt$lon_col, opt$lat_col), crs = 4326, remove = FALSE)
  pts$.pt_id <- seq_len(nrow(pts))
  pol_4326 <- sf::st_transform(pol, 4326)
  join_fun <- if (opt$join_method == "intersects") sf::st_intersects else sf::st_within
  pts_j <- sf::st_join(pts, pol_4326 %>% dplyr::select(id_amostra), join = join_fun, left = FALSE)
  dup <- pts_j %>% sf::st_drop_geometry() %>% dplyr::count(.pt_id, name = "n") %>% dplyr::filter(n > 1)
  if (nrow(dup) > 0) pts_j <- pts_j %>% dplyr::group_by(.pt_id) %>% dplyr::slice(1) %>% dplyr::ungroup()
  out <- pts_j %>% sf::st_drop_geometry() %>% dplyr::select(-.pt_id)
  list(df = out, log = tibble::tibble(join_method = opt$join_method, n_shots_before = nrow(df), n_shots_after = nrow(out), n_polygons_after = dplyr::n_distinct(out$id_amostra), pct_kept = 100 * nrow(out) / pmax(nrow(df), 1)))
}
apply_edge_buffer <- function(df_in, pol, opt) {
  pol_m <- sf::st_transform(pol, opt$crs_metrico)
  pts <- sf::st_as_sf(df_in, coords = c(opt$lon_col, opt$lat_col), crs = 4326, remove = FALSE)
  pts_m <- sf::st_transform(pts, sf::st_crs(pol_m))
  pol_interior <- try(suppressWarnings(sf::st_buffer(pol_m, dist = -opt$buffer_m)), silent = TRUE)
  do_erosion <- !(inherits(pol_interior, "try-error") || all(sf::st_is_empty(pol_interior)))
  if (do_erosion) {
    keep_idx <- lengths(sf::st_intersects(pts_m, pol_interior)) > 0
    metodo <- "geometric_erosion"
  } else {
    inside_idx <- lengths(sf::st_intersects(pts_m, pol_m)) > 0
    border <- sf::st_boundary(sf::st_union(pol_m))
    dist_borda <- as.numeric(sf::st_distance(pts_m, border))
    keep_idx <- inside_idx & (dist_borda > opt$buffer_m)
    metodo <- "global_distance_to_boundary"
  }
  df_out <- df_in[keep_idx, , drop = FALSE]
  list(df = df_out, impacto = tibble::tibble(buffer_m = opt$buffer_m, buffer_method = metodo, n_total = nrow(df_in), n_kept = nrow(df_out), n_removed = nrow(df_in) - nrow(df_out), removed_pct = 100 * (nrow(df_in) - nrow(df_out)) / pmax(nrow(df_in), 1)))
}

spearman_corr_matrix <- function(df, vars) {
  x <- df %>% dplyr::select(dplyr::all_of(vars)) %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  x <- x[stats::complete.cases(x), , drop = FALSE]
  if (nrow(x) < 10) return(NULL)
  stats::cor(x, method = "spearman")
}
flatten_cor <- function(mat, tag) {
  if (is.null(mat)) return(NULL)
  as.data.frame(as.table(mat)) %>% tibble::as_tibble() %>% dplyr::rename(var_row = Var1, var_col = Var2, rho = Freq) %>% dplyr::mutate(tag = tag)
}

new_scenario <- function(label, req_cols, filter_fn) list(label = label, req_cols = unique(req_cols), filter_fn = filter_fn)
classify_strong_beams <- function(df, beam_col, sens_col) {
  if (!(beam_col %in% names(df)) || !(sens_col %in% names(df))) return(NULL)
  b <- df[[beam_col]]; s <- df[[sens_col]]
  if (is.character(b)) {
    bl <- tolower(b); is_strong <- grepl("strong|high", bl)
    if (sum(is_strong, na.rm = TRUE) > 0) return(list(mode = "string_match", strong_beams = unique(b[is_strong])))
  }
  tmp <- tibble::tibble(beam = as.character(b), sens = as.numeric(s)) %>% dplyr::filter(!is.na(beam), is.finite(sens))
  if (nrow(tmp) < 50) return(NULL)
  beam_stats <- tmp %>% dplyr::group_by(beam) %>% dplyr::summarise(med_sens = median(sens, na.rm = TRUE), n = dplyr::n(), .groups = "drop") %>% dplyr::arrange(dplyr::desc(med_sens))
  strong <- beam_stats$beam[seq_len(ceiling(nrow(beam_stats) / 2))]
  list(mode = "median_sensitivity_top_half", strong_beams = strong, beam_stats = beam_stats)
}
build_qc_scenarios <- function(opt, df_for_beam_logic) {
  cc <- opt$qc_cols; v <- opt$qc_values
  qf2 <- cc$qf_2b; dg2 <- cc$dg_2b; s2 <- cc$sens_2b; b2 <- cc$beam_2b; sol2 <- cc$sol_2b
  qf4 <- cc$qf_4a; dg4 <- cc$dg_4a; s4 <- cc$sens_4a; b4 <- cc$beam_4a; sol4 <- cc$sol_4a
  scenarios <- list()
  if (isTRUE(opt$scenario_flags$include_no_qc)) scenarios[["S0_no_qc"]] <- new_scenario("No QC diagnostic", character(0), function(d) d)
  scenarios[["S1_baseline"]] <- new_scenario("Baseline QC: L2B quality=1/degrade=0 and L4A quality=1/degrade=0", c(qf2, dg2, qf4, dg4), function(d) d %>% dplyr::filter(.data[[qf2]] == v$quality_ok, .data[[dg2]] == v$degrade_ok, .data[[qf4]] == v$quality_ok, .data[[dg4]] == v$degrade_ok))
  for (thr in opt$sensitivity_thresholds) {
    local({
      thr0 <- thr
      sid <- paste0("S_sens", gsub("\\.", "", sprintf("%.2f", thr0)))
      scenarios[[sid]] <<- new_scenario(
        paste0("Baseline + sensitivity_2b >= ", thr0, " and sensitivity_4a >= ", thr0),
        c(qf2, dg2, qf4, dg4, s2, s4),
        function(d) {
          d %>% dplyr::filter(
            .data[[qf2]] == v$quality_ok, .data[[dg2]] == v$degrade_ok,
            .data[[qf4]] == v$quality_ok, .data[[dg4]] == v$degrade_ok,
            is.finite(.data[[s2]]), .data[[s2]] >= thr0,
            is.finite(.data[[s4]]), .data[[s4]] >= thr0
          )
        }
      )
    })
  }
  if (isTRUE(opt$scenario_flags$include_strong_beams)) {
    beam2 <- classify_strong_beams(df_for_beam_logic, b2, s2); beam4 <- classify_strong_beams(df_for_beam_logic, b4, s4)
    scenarios[["S_strong_beams"]] <- new_scenario("Baseline + strong beams only", c(qf2, dg2, qf4, dg4, b2, b4, s2, s4), function(d) {
      d2 <- d %>% dplyr::filter(.data[[qf2]] == v$quality_ok, .data[[dg2]] == v$degrade_ok, .data[[qf4]] == v$quality_ok, .data[[dg4]] == v$degrade_ok)
      if (is.null(beam2) || is.null(beam4)) return(d2)
      d2 %>% dplyr::filter(as.character(.data[[b2]]) %in% beam2$strong_beams, as.character(.data[[b4]]) %in% beam4$strong_beams)
    })
    attr(scenarios[["S_strong_beams"]], "beam2_info") <- beam2
    attr(scenarios[["S_strong_beams"]], "beam4_info") <- beam4
  }
  if (isTRUE(opt$scenario_flags$include_night_only)) scenarios[["S_night_only"]] <- new_scenario("Baseline + night-only", c(qf2, dg2, qf4, dg4, sol2, sol4), function(d) d %>% dplyr::filter(.data[[qf2]] == v$quality_ok, .data[[dg2]] == v$degrade_ok, .data[[qf4]] == v$quality_ok, .data[[dg4]] == v$degrade_ok, is.finite(.data[[sol2]]), .data[[sol2]] <= 0, is.finite(.data[[sol4]]), .data[[sol4]] <= 0))
  if (isTRUE(opt$scenario_flags$include_agbd_uncertainty)) scenarios[["S_agbd_low_uncert"]] <- new_scenario("Baseline + low AGBD uncertainty", c(qf2, dg2, qf4, dg4, "agbd"), function(d) {
    d2 <- d %>% dplyr::filter(.data[[qf2]] == v$quality_ok, .data[[dg2]] == v$degrade_ok, .data[[qf4]] == v$quality_ok, .data[[dg4]] == v$degrade_ok)
    if ("agbd_se" %in% names(d2)) { thr <- stats::quantile(d2$agbd_se, 0.90, na.rm = TRUE, names = FALSE); d2 <- d2 %>% dplyr::filter(is.finite(agbd_se), agbd_se <= thr) }
    if (all(c("agbd_pi_lower", "agbd_pi_upper") %in% names(d2))) { piw <- d2$agbd_pi_upper - d2$agbd_pi_lower; thr2 <- stats::quantile(piw, 0.90, na.rm = TRUE, names = FALSE); d2 <- d2 %>% dplyr::mutate(piw = piw) %>% dplyr::filter(is.finite(piw), piw <= thr2) %>% dplyr::select(-piw) }
    d2
  })
  scenarios
}

kw_observed <- function(df_area, vars_key) {
  purrr::map_dfr(vars_key, function(v) {
    sub <- df_area %>% dplyr::select(fito_grupo, !!rlang::sym(v)) %>% dplyr::filter(is.finite(.data[[v]])) %>% droplevels()
    if (nrow(sub) == 0 || dplyr::n_distinct(sub$fito_grupo) < 2) return(NULL)
    kw <- stats::kruskal.test(as.formula(paste(v, "~ fito_grupo")), data = sub)
    k_obs <- dplyr::n_distinct(sub$fito_grupo)
    tibble::tibble(variavel = v, n_polygons = nrow(sub), k_groups = k_obs, statistic_H = as.numeric(kw$statistic), p_global = kw$p.value, eps2_observed = epsilon2_kw(kw$statistic, nrow(sub), k_obs))
  })
}
dunn_observed <- function(df_area, vars_key) {
  purrr::map_dfr(vars_key, function(v) {
    dunn_v <- compute_dunn_tables(df_area, v)
    if (is.null(dunn_v) || !nrow(dunn_v)) return(NULL)
    n_tab <- df_area %>% dplyr::filter(is.finite(.data[[v]])) %>% dplyr::count(fito_grupo, name = "n_grupo")
    calc_r_from_dunn(dunn_v, n_tab)
  })
}
run_polygon_level_inference <- function(df_post, opt, n_boot) {
  vars_key <- opt$vars_key
  df_area <- df_post %>% dplyr::group_by(id_amostra, fito_grupo) %>% dplyr::summarise(dplyr::across(dplyr::all_of(vars_key), ~ stats::median(.x, na.rm = TRUE)), n_footprints = dplyr::n(), .groups = "drop")
  tab_counts <- df_area %>% dplyr::count(fito_grupo, name = "n_polygons")
  if (nrow(tab_counts) < 2) return(list(df_area = df_area, kw_observed = tibble::tibble(), dunn_observed = tibble::tibble(), kw_boot = tibble::tibble(), dunn_boot = tibble::tibble()))
  kw_obs <- kw_observed(df_area, vars_key)
  dunn_obs <- dunn_observed(df_area, vars_key)
  n_target <- min(tab_counts$n_polygons, na.rm = TRUE)
  set.seed(opt$bootstrap$seed)
  boot_kw <- list(); boot_dunn <- list()
  msg("  Bootstrap: ", n_boot, " replicates; target polygons/group = ", n_target)
  pb <- utils::txtProgressBar(min = 0, max = n_boot, style = 3)
  on.exit(close(pb), add = TRUE)
  for (b in seq_len(n_boot)) {
    utils::setTxtProgressBar(pb, b)
    dfb <- balance_once(df_area, "fito_grupo", n_target, replace = opt$bootstrap$replace_polygons)
    kwb <- purrr::map_dfr(vars_key, function(v) {
      sub <- dfb %>% dplyr::select(fito_grupo, !!rlang::sym(v)) %>% dplyr::filter(is.finite(.data[[v]])) %>% droplevels()
      if (nrow(sub) == 0 || dplyr::n_distinct(sub$fito_grupo) < 2) return(NULL)
      kw <- stats::kruskal.test(as.formula(paste(v, "~ fito_grupo")), data = sub)
      tibble::tibble(replica = b, variavel = v, p_global = kw$p.value, eps2 = epsilon2_kw(kw$statistic, nrow(sub), dplyr::n_distinct(sub$fito_grupo)))
    })
    if (nrow(kwb)) boot_kw[[b]] <- kwb
    dunn_b <- purrr::map_dfr(vars_key, ~ compute_dunn_tables(dfb, .x))
    if (nrow(dunn_b)) {
      n_tab <- dfb %>% dplyr::count(fito_grupo, name = "n_grupo")
      r_b <- calc_r_from_dunn(dunn_b, n_tab)
      if (nrow(r_b)) { r_b$replica <- b; boot_dunn[[b]] <- r_b }
    }
  }
  boot_kw_df <- if (length(boot_kw)) purrr::list_rbind(boot_kw) else tibble::tibble()
  boot_dunn_df <- if (length(boot_dunn)) purrr::list_rbind(boot_dunn) else tibble::tibble()
  kw_summary <- if (nrow(boot_kw_df)) boot_kw_df %>% dplyr::group_by(variavel) %>% dplyr::summarise(p_global_median = median(p_global, na.rm = TRUE), eps2_median = median(eps2, na.rm = TRUE), eps2_p025 = quantile(eps2, 0.025, na.rm = TRUE, names = FALSE), eps2_p975 = quantile(eps2, 0.975, na.rm = TRUE, names = FALSE), .groups = "drop") else tibble::tibble()
  dunn_summary <- if (nrow(boot_dunn_df)) boot_dunn_df %>% dplyr::group_by(variavel, comparacao, comparacao_en, ajuste) %>% dplyr::summarise(r_median = median(as.numeric(r), na.rm = TRUE), prop_sig = mean(!is.na(p_adj) & p_adj < 0.05, na.rm = TRUE), .groups = "drop") else tibble::tibble()
  list(df_area = df_area, kw_observed = kw_obs, dunn_observed = dunn_obs, kw_boot = kw_summary, dunn_boot = dunn_summary)
}
apply_fixed_shots_per_polygon <- function(df_post, m, replace = TRUE) {
  df_post %>% dplyr::group_by(id_amostra) %>% dplyr::group_modify(~ dplyr::slice_sample(.x, n = min(nrow(.x), m), replace = replace)) %>% dplyr::ungroup()
}

build_connected_knn_listw <- function(pts_lonlat, k_start = 8, k_max = 30, seed_jitter = 123) {
  n <- nrow(pts_lonlat)
  make_listw <- function(pts, k) { nb_obj <- spdep::knearneigh(pts, k = k); nb <- spdep::knn2nb(nb_obj); list(nb = nb, listw = spdep::nb2listw(nb, style = "W", zero.policy = TRUE)) }
  out_last <- NULL
  for (k in seq(k_start, min(k_max, n - 1))) { out <- try(make_listw(pts_lonlat, k), silent = TRUE); if (!inherits(out, "try-error")) { out_last <- out; if (spdep::n.comp.nb(out$nb)$nc == 1) return(out$listw) } }
  set.seed(seed_jitter); pts_j <- pts_lonlat + matrix(stats::rnorm(length(pts_lonlat), sd = 1e-6), ncol = 2)
  for (k in seq(k_start, min(k_max, n - 1))) { out <- try(make_listw(pts_j, k), silent = TRUE); if (!inherits(out, "try-error")) { out_last <- out; if (spdep::n.comp.nb(out$nb)$nc == 1) return(out$listw) } }
  if (!is.null(out_last)) return(out_last$listw)
  stop("Failed to build kNN listw.")
}
fit_baseline_lmm_spatial <- function(df_baseline, opt) {
  if (!isTRUE(opt$spatial$run_baseline)) return(list(lmm = NULL, moran = NULL, variogram = NULL))
  set.seed(opt$spatial$seed_lmm_sample)
  df_lmm <- df_baseline %>% dplyr::select(id_amostra, fito_grupo, dplyr::all_of(opt$vars_key), lat = !!rlang::sym(opt$lat_col), lon = !!rlang::sym(opt$lon_col)) %>% dplyr::group_by(fito_grupo) %>% dplyr::group_modify(~ dplyr::slice_sample(.x, n = min(nrow(.x), opt$spatial$max_shots_per_group_lmm))) %>% dplyr::ungroup()
  fit_one <- function(v) {
    sub <- df_lmm %>% dplyr::select(id_amostra, fito_grupo, !!rlang::sym(v)) %>% dplyr::filter(is.finite(.data[[v]]))
    if (nrow(sub) < 50 || dplyr::n_distinct(sub$id_amostra) < 10) return(NULL)
    m <- try(lme4::lmer(stats::as.formula(paste(v, "~ fito_grupo + (1|id_amostra)")), data = sub, REML = TRUE), silent = TRUE)
    if (inherits(m, "try-error")) return(NULL)
    an <- try(lmerTest::anova(m), silent = TRUE); if (inherits(an, "try-error")) an <- stats::anova(m)
    dat_m <- model.frame(m)
    res_area <- tibble::tibble(id_amostra = dat_m$id_amostra, res = as.numeric(residuals(m))) %>% dplyr::group_by(id_amostra) %>% dplyr::summarise(res = mean(res, na.rm = TRUE), .groups = "drop")
    coords_area <- df_lmm %>% dplyr::group_by(id_amostra) %>% dplyr::summarise(lat = mean(lat, na.rm = TRUE), lon = mean(lon, na.rm = TRUE), .groups = "drop") %>% stats::na.omit()
    da <- dplyr::inner_join(res_area, coords_area, by = "id_amostra") %>% dplyr::filter(is.finite(res), is.finite(lat), is.finite(lon))
    moran_row <- NULL
    if (isTRUE(opt$spatial$run_moran) && nrow(da) >= 20) {
      lw <- build_connected_knn_listw(as.matrix(da[, c("lon", "lat")]), k_start = 8, k_max = 30)
      mi <- spdep::moran.test(da$res, lw, zero.policy = TRUE, na.action = na.exclude)
      moran_row <- tibble::tibble(variavel = v, n_polygons = nrow(da), moran_i = unname(mi$estimate[["Moran I statistic"]]), p_value = mi$p.value, weights = "adaptive kNN")
    }
    variog_df <- NULL
    if (isTRUE(opt$spatial$run_variogram) && requireNamespace("gstat", quietly = TRUE) && nrow(da) >= 30) {
      sf_pts <- sf::st_as_sf(da, coords = c("lon", "lat"), crs = 4326)
      sp_pts <- as(sf::st_transform(sf_pts, opt$crs_metrico), "Spatial")
      vg <- try(gstat::variogram(res ~ 1, data = sp_pts), silent = TRUE)
      if (!inherits(vg, "try-error")) variog_df <- vg %>% dplyr::mutate(variavel = v)
    }
    list(anova = tibble::as_tibble(as.data.frame(an)) %>% dplyr::mutate(variavel = v), moran = moran_row, variogram = variog_df)
  }
  out <- purrr::map(opt$vars_key, fit_one)
  list(lmm = purrr::list_rbind(purrr::compact(purrr::map(out, "anova"))), moran = purrr::list_rbind(purrr::compact(purrr::map(out, "moran"))), variogram = purrr::list_rbind(purrr::compact(purrr::map(out, "variogram"))))
}
mark_redundant_scenarios <- function(log_df) {
  sig_df <- log_df %>% dplyr::mutate(signature = paste(n_shots_post_qc, n_polygons_post_qc, n_shots_post_buffer, n_polygons_post_buffer, sep = "|"))
  reps <- sig_df %>% dplyr::group_by(signature) %>% dplyr::slice(1) %>% dplyr::ungroup() %>% dplyr::select(signature, scenario_rep = scenario)
  sig_df %>% dplyr::left_join(reps, by = "signature") %>% dplyr::mutate(redundant = scenario != scenario_rep, status2 = dplyr::case_when(status != "RUN" ~ status, redundant ~ "REDUNDANT", TRUE ~ "RUN"))
}

# --------------------------- 3) MAIN EXECUTION -------------------------------
mk_outdir()
path_csv <- resolve_path(opt$path_csv, opt$path_csv_fallback)
poly_path <- resolve_path(opt$poly_path, opt$poly_path_fallback)
msg("Reading GEDI footprints: ", path_csv)
df <- readr::read_csv(path_csv, show_col_types = FALSE) %>% janitor::clean_names()

numeric_candidates <- intersect(c("elev_highestreturn", "elev_lowestmode_2b", "elev_lowestmode_4a", "cover", "pai", "fhd_normal", "agbd", opt$lat_col, opt$lon_col, opt$qc_cols$qf_2b, opt$qc_cols$dg_2b, opt$qc_cols$sens_2b, opt$qc_cols$sol_2b, opt$qc_cols$qf_4a, opt$qc_cols$dg_4a, opt$qc_cols$sens_4a, opt$qc_cols$sol_4a, "agbd_se", "agbd_pi_lower", "agbd_pi_upper"), names(df))
for (nm in numeric_candidates) df[[nm]] <- suppressWarnings(readr::parse_number(as.character(df[[nm]])))

df <- df %>% dplyr::mutate(
  fito_grupo = extrair_fito_grupo(.data[[opt$fito_col]]),
  altura_dossel = calc_altura_dossel(elev_highestreturn, if ("elev_lowestmode_2b" %in% names(.)) elev_lowestmode_2b else NA_real_, if ("elev_lowestmode_4a" %in% names(.)) elev_lowestmode_4a else NA_real_),
  cover = if ("cover" %in% names(.)) cover_to_percent(cover) else NA_real_,
  altura_dossel = na_outside(altura_dossel, 0, 120), cover = na_outside(cover, 0, 100),
  pai = if ("pai" %in% names(.)) na_outside(pai, 0, 15) else NA_real_,
  agbd = if ("agbd" %in% names(.)) na_outside(agbd, 0, 1000) else NA_real_,
  fhd_normal = if ("fhd_normal" %in% names(.)) na_outside(fhd_normal, 0, 15) else NA_real_
) %>% dplyr::filter(fito_grupo %in% c("FOD", "FOM", "FES", "FED")) %>% droplevels()

msg("Reading polygons: ", poly_path)
pol <- sf::st_read(poly_path, layer = opt$poly_layer, quiet = TRUE) %>% sf::st_make_valid()
if (is.na(sf::st_crs(pol))) stop("Polygon CRS is missing. Define the CRS in the GPKG before running this script.")
if (!(opt$poly_id_col %in% names(pol))) stop("Polygon ID column not found: ", opt$poly_id_col)
pol <- pol %>% dplyr::rename(id_amostra = !!rlang::sym(opt$poly_id_col))
pol$id_amostra <- as.character(pol$id_amostra)

msg("Assigning polygon IDs by spatial join...")
join_out <- assign_polygon_id_by_join(df, pol, opt)
df <- join_out$df
join_log <- join_out$log

cc <- opt$qc_cols; v <- opt$qc_values
required_qc <- c(cc$qf_2b, cc$dg_2b, cc$qf_4a, cc$dg_4a)
miss_qc <- setdiff(required_qc, names(df))
if (length(miss_qc)) stop("Missing required QC columns: ", paste(miss_qc, collapse = ", "))

df_probe <- df %>% dplyr::filter(.data[[cc$qf_2b]] == v$quality_ok, .data[[cc$dg_2b]] == v$degrade_ok, .data[[cc$qf_4a]] == v$quality_ok, .data[[cc$dg_4a]] == v$degrade_ok)
by_pol_probe <- df_probe %>% dplyr::distinct(id_amostra, fito_grupo) %>% dplyr::count(fito_grupo, name = "n_polygons")
by_sh_probe <- df_probe %>% dplyr::count(fito_grupo, name = "n_shots")
preflight <- tibble::tibble(
  scenario = "baseline_qc_post_join",
  n_shots = nrow(df_probe),
  n_polygons = dplyr::n_distinct(df_probe$id_amostra),
  min_polygons_group = if (nrow(by_pol_probe)) min(by_pol_probe$n_polygons) else 0,
  min_shots_group = if (nrow(by_sh_probe)) min(by_sh_probe$n_shots) else 0
)

msg("Building QC scenarios...")
qc_scenarios <- build_qc_scenarios(opt, df)
scenario_logs <- list(); scenario_results <- list(); fixedshot_results <- list(); corr_results <- list()

for (sid in names(qc_scenarios)) {
  msg("Scenario: ", sid)
  scen <- qc_scenarios[[sid]]
  missing <- setdiff(scen$req_cols, names(df))
  if (length(missing)) {
    scenario_logs[[sid]] <- tibble::tibble(scenario = sid, label = scen$label, status = "SKIPPED", reason = paste("Missing:", paste(missing, collapse = ", ")))
    next
  }
  df_qc <- scen$filter_fn(df)
  buf <- apply_edge_buffer(df_qc, pol, opt)
  df_post <- buf$df
  scenario_logs[[sid]] <- tibble::tibble(scenario = sid, label = scen$label, status = "RUN", reason = NA_character_, n_shots_post_qc = nrow(df_qc), n_polygons_post_qc = dplyr::n_distinct(df_qc$id_amostra), n_shots_post_buffer = nrow(df_post), n_polygons_post_buffer = dplyr::n_distinct(df_post$id_amostra), buffer_method = buf$impacto$buffer_method, buffer_removed_pct = buf$impacto$removed_pct)
  n_boot <- if (sid == "S1_baseline") opt$bootstrap$n_boot_baseline else opt$bootstrap$n_boot_alt
  inf <- run_polygon_level_inference(df_post, opt, n_boot)
  scenario_results[[sid]] <- list(
    kw_observed = inf$kw_observed %>% dplyr::mutate(scenario = sid),
    dunn_observed = inf$dunn_observed %>% dplyr::mutate(scenario = sid),
    kw_boot = inf$kw_boot %>% dplyr::mutate(scenario = sid),
    dunn_boot = inf$dunn_boot %>% dplyr::mutate(scenario = sid)
  )
  if (nrow(inf$df_area) >= 20) corr_results[[paste0(sid, "_polygon")]] <- spearman_corr_matrix(inf$df_area, opt$vars_key)
  if (sid == "S1_baseline") corr_results[[paste0(sid, "_shot")]] <- spearman_corr_matrix(df_post, opt$vars_key)
  if (isTRUE(opt$scenario_flags$include_fixed_shots_per_polygon) && sid %in% opt$fixed_shots$run_for_scenarios) {
    df_fixed <- apply_fixed_shots_per_polygon(df_post, opt$fixed_shots$m, replace = opt$fixed_shots$replace)
    inf_fixed <- run_polygon_level_inference(df_fixed, opt, opt$bootstrap$n_boot_alt)
    fixedshot_results[[sid]] <- list(kw_boot = inf_fixed$kw_boot %>% dplyr::mutate(scenario = sid, sampling_mode = paste0("fixed_", opt$fixed_shots$m)), dunn_boot = inf_fixed$dunn_boot %>% dplyr::mutate(scenario = sid, sampling_mode = paste0("fixed_", opt$fixed_shots$m)))
  }
}

log_df <- purrr::list_rbind(scenario_logs)
kw_observed_all <- purrr::imap_dfr(scenario_results, ~ .x$kw_observed)
dunn_observed_all <- purrr::imap_dfr(scenario_results, ~ .x$dunn_observed)
kw_boot_all <- purrr::imap_dfr(scenario_results, ~ .x$kw_boot)
dunn_boot_all <- purrr::imap_dfr(scenario_results, ~ .x$dunn_boot)
kw_fixed <- purrr::imap_dfr(fixedshot_results, ~ .x$kw_boot)
dunn_fixed <- purrr::imap_dfr(fixedshot_results, ~ .x$dunn_boot)

baseline_id <- "S1_baseline"
delta_eps2_observed <- tibble::tibble()
delta_eps2_boot <- tibble::tibble()
delta_r_observed <- tibble::tibble()
delta_r_boot <- tibble::tibble()
if (baseline_id %in% unique(kw_observed_all$scenario)) {
  base <- kw_observed_all %>% dplyr::filter(scenario == baseline_id) %>% dplyr::select(variavel, eps2_base = eps2_observed)
  delta_eps2_observed <- kw_observed_all %>% dplyr::left_join(base, by = "variavel") %>% dplyr::mutate(delta_eps2 = eps2_observed - eps2_base)
}
if (baseline_id %in% unique(kw_boot_all$scenario)) {
  base <- kw_boot_all %>% dplyr::filter(scenario == baseline_id) %>% dplyr::select(variavel, eps2_base = eps2_median)
  delta_eps2_boot <- kw_boot_all %>% dplyr::left_join(base, by = "variavel") %>% dplyr::mutate(delta_eps2 = eps2_median - eps2_base)
}
if (baseline_id %in% unique(dunn_observed_all$scenario)) {
  base <- dunn_observed_all %>% dplyr::filter(scenario == baseline_id) %>% dplyr::select(variavel, comparacao_en, ajuste, r_base = r)
  delta_r_observed <- dunn_observed_all %>% dplyr::left_join(base, by = c("variavel", "comparacao_en", "ajuste")) %>% dplyr::mutate(delta_r = r - r_base)
}
if (baseline_id %in% unique(dunn_boot_all$scenario)) {
  base <- dunn_boot_all %>% dplyr::filter(scenario == baseline_id) %>% dplyr::select(variavel, comparacao_en, ajuste, r_base = r_median, prop_sig_base = prop_sig)
  delta_r_boot <- dunn_boot_all %>% dplyr::left_join(base, by = c("variavel", "comparacao_en", "ajuste")) %>% dplyr::mutate(delta_r = r_median - r_base, delta_prop_sig = prop_sig - prop_sig_base)
}

log_df2 <- mark_redundant_scenarios(log_df)
keep_scenarios <- log_df2 %>% dplyr::filter(status2 == "RUN") %>% dplyr::pull(scenario)
redundancy_map <- log_df2 %>% dplyr::filter(status == "RUN") %>% dplyr::select(scenario, label, signature, scenario_rep, status2) %>% dplyr::arrange(signature, scenario)

lmm_out <- list(lmm = NULL, moran = NULL, variogram = NULL)
if (isTRUE(opt$spatial$run_baseline) && baseline_id %in% log_df$scenario) {
  msg("Running baseline LMM and spatial diagnostics...")
  df_base <- qc_scenarios[[baseline_id]]$filter_fn(df)
  df_base <- apply_edge_buffer(df_base, pol, opt)$df
  lmm_out <- fit_baseline_lmm_spatial(df_base, opt)
}

# --------------------------- 4) EXPORT ---------------------------------------
msg("Exporting supplementary QC sensitivity results...")
wb <- openxlsx::createWorkbook(); used <- character(0)
add_sheet <- function(nm) { nm2 <- safe_sheet_name(nm, used); openxlsx::addWorksheet(wb, nm2); used <<- c(used, nm2); nm2 }

sheets <- list(
  join = add_sheet("join_log"), pre = add_sheet("preflight"), log = add_sheet("scenario_log"),
  kw_obs = add_sheet("kw_observed_by_scenario"), dunn_obs = add_sheet("dunn_observed_by_scenario"),
  kw_boot = add_sheet("kw_boot_by_scenario"), dunn_boot = add_sheet("dunn_boot_by_scenario"),
  de_obs = add_sheet("delta_eps2_observed"), de_boot = add_sheet("delta_eps2_boot"),
  dr_obs = add_sheet("delta_r_observed"), dr_boot = add_sheet("delta_r_boot"),
  fixed_kw = add_sheet("fixedshots_kw_boot"), fixed_dunn = add_sheet("fixedshots_dunn_boot"),
  corr = add_sheet("correlations"), lmm = add_sheet("lmm_anova_baseline"), moran = add_sheet("moran_baseline"), vario = add_sheet("variogram_baseline"),
  red = add_sheet("redundancy_map"), beams = add_sheet("beam_strength_info")
)

openxlsx::writeData(wb, sheets$join, join_log)
openxlsx::writeData(wb, sheets$pre, preflight)
openxlsx::writeData(wb, sheets$log, log_df2)
if (nrow(kw_observed_all)) openxlsx::writeData(wb, sheets$kw_obs, kw_observed_all)
if (nrow(dunn_observed_all)) openxlsx::writeData(wb, sheets$dunn_obs, dunn_observed_all)
if (nrow(kw_boot_all)) openxlsx::writeData(wb, sheets$kw_boot, kw_boot_all)
if (nrow(dunn_boot_all)) openxlsx::writeData(wb, sheets$dunn_boot, dunn_boot_all)
if (nrow(delta_eps2_observed)) openxlsx::writeData(wb, sheets$de_obs, delta_eps2_observed)
if (nrow(delta_eps2_boot)) openxlsx::writeData(wb, sheets$de_boot, delta_eps2_boot)
if (nrow(delta_r_observed)) openxlsx::writeData(wb, sheets$dr_obs, delta_r_observed)
if (nrow(delta_r_boot)) openxlsx::writeData(wb, sheets$dr_boot, delta_r_boot)
if (nrow(kw_fixed)) openxlsx::writeData(wb, sheets$fixed_kw, kw_fixed)
if (nrow(dunn_fixed)) openxlsx::writeData(wb, sheets$fixed_dunn, dunn_fixed)

corr_tbl <- purrr::imap_dfr(corr_results, ~ flatten_cor(.x, .y))
if (nrow(corr_tbl)) openxlsx::writeData(wb, sheets$corr, corr_tbl)
if (!is.null(lmm_out$lmm) && nrow(lmm_out$lmm)) openxlsx::writeData(wb, sheets$lmm, lmm_out$lmm)
if (!is.null(lmm_out$moran) && nrow(lmm_out$moran)) openxlsx::writeData(wb, sheets$moran, lmm_out$moran)
if (!is.null(lmm_out$variogram) && nrow(lmm_out$variogram)) openxlsx::writeData(wb, sheets$vario, lmm_out$variogram)
openxlsx::writeData(wb, sheets$red, redundancy_map)

beam_rows <- list()
if ("S_strong_beams" %in% names(qc_scenarios)) {
  b2 <- attr(qc_scenarios[["S_strong_beams"]], "beam2_info"); b4 <- attr(qc_scenarios[["S_strong_beams"]], "beam4_info")
  if (!is.null(b2$beam_stats)) beam_rows[["beam2"]] <- b2$beam_stats %>% dplyr::mutate(product = "L2B", method = b2$mode)
  if (!is.null(b4$beam_stats)) beam_rows[["beam4"]] <- b4$beam_stats %>% dplyr::mutate(product = "L4A", method = b4$mode)
}
beam_df <- purrr::list_rbind(beam_rows)
if (nrow(beam_df)) openxlsx::writeData(wb, sheets$beams, beam_df)

mk_outdir()
out_path <- safe_save_workbook(wb, opt$xlsx_file)

# CSV exports for figure script and repository inspection
readr::write_csv(log_df2, file.path(opt$dir_out, "scenario_log.csv"))
if (nrow(kw_observed_all)) readr::write_csv(kw_observed_all, file.path(opt$dir_out, "kw_observed_by_scenario.csv"))
if (nrow(kw_boot_all)) readr::write_csv(kw_boot_all, file.path(opt$dir_out, "kw_boot_by_scenario.csv"))
if (nrow(delta_eps2_observed)) readr::write_csv(delta_eps2_observed, file.path(opt$dir_out, "delta_eps2_observed.csv"))
if (nrow(delta_eps2_boot)) readr::write_csv(delta_eps2_boot, file.path(opt$dir_out, "delta_eps2_boot.csv"))
if (nrow(corr_tbl)) readr::write_csv(corr_tbl, file.path(opt$dir_out, "correlations_qc_scenarios.csv"))
if (nrow(dunn_observed_all)) readr::write_csv(dunn_observed_all, file.path(opt$dir_out, "dunn_observed_by_scenario.csv"))
if (nrow(dunn_boot_all)) readr::write_csv(dunn_boot_all, file.path(opt$dir_out, "dunn_boot_by_scenario.csv"))
if (nrow(delta_r_observed)) readr::write_csv(delta_r_observed, file.path(opt$dir_out, "delta_r_observed.csv"))
if (nrow(delta_r_boot)) readr::write_csv(delta_r_boot, file.path(opt$dir_out, "delta_r_boot.csv"))
if (nrow(redundancy_map)) readr::write_csv(redundancy_map, file.path(opt$dir_out, "redundancy_map.csv"))
if (!is.null(lmm_out$lmm) && nrow(lmm_out$lmm)) readr::write_csv(lmm_out$lmm, file.path(opt$dir_out, "lmm_anova_baseline.csv"))
if (!is.null(lmm_out$moran) && nrow(lmm_out$moran)) readr::write_csv(lmm_out$moran, file.path(opt$dir_out, "moran_baseline.csv"))
if (!is.null(lmm_out$variogram) && nrow(lmm_out$variogram)) readr::write_csv(lmm_out$variogram, file.path(opt$dir_out, "variogram_baseline.csv"))
if (nrow(beam_df)) readr::write_csv(beam_df, file.path(opt$dir_out, "beam_strength_info.csv"))

saveRDS(
  list(
    join_log = join_log,
    preflight = preflight,
    scenario_log = log_df2,
    kw_observed_by_scenario = kw_observed_all,
    dunn_observed_by_scenario = dunn_observed_all,
    kw_boot_by_scenario = kw_boot_all,
    dunn_boot_by_scenario = dunn_boot_all,
    delta_eps2_observed = delta_eps2_observed,
    delta_eps2_boot = delta_eps2_boot,
    delta_r_observed = delta_r_observed,
    delta_r_boot = delta_r_boot,
    correlations = corr_tbl,
    lmm_anova_baseline = lmm_out$lmm,
    moran_baseline = lmm_out$moran,
    variogram_baseline = lmm_out$variogram,
    redundancy_map = redundancy_map,
    beam_strength_info = beam_df,
    options = opt
  ),
  file = file.path(opt$dir_out, "04_qc_sensitivity_results.rds")
)

capture.output(sessionInfo(), file = file.path(opt$dir_out, "sessionInfo_supplementary_qc.txt"))
msg("[OK] Supplementary QC results exported to: ", normalizePath(out_path))
