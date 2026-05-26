# -----------------------------------------------------------------------------
# Script 02 — Run manuscript statistical analyses
#
# Project: GEDI-derived forest structure in Atlantic Forest physiognomies
# Repository workflow: manuscript reproducibility
# Author: Marcelo C. S. Bandoria
#
# Purpose
# This script reproduces the statistical results reported in the manuscript. It
# uses the prepared dataset from Script 01, summarizes GEDI metrics by polygon,
# runs the polygon-level bootstrap, applies Kruskal-Wallis and Dunn post hoc
# tests, estimates effect sizes, and exports the final tables in scientific
# English.
#
# Important reproducibility note
# The bootstrap settings must match the manuscript workflow for final results.
# Use the fast-test mode only to check that the script runs; do not compare
# bootstrap values from the fast-test mode with the manuscript tables.
#
# Main input
#   results/intermediate/01_manuscript_prepared_dataset.rds
#
# Main outputs
#   results/main/manuscript_results.xlsx
#   results/main/tables/kw_polygon_bootstrap.csv
#   results/main/tables/dunn_polygon_bootstrap.csv
#   results/intermediate/02_manuscript_statistics.rds
#
# Run second. For a quick structural test, run:
#   Sys.setenv(GEDI_FAST_TEST = "true")
# For the final manuscript reproduction, run:
#   Sys.unsetenv("GEDI_FAST_TEST")
# -----------------------------------------------------------------------------

# ----------------------------- 0) OPTIONS ------------------------------------
find_repo_root <- function() {
  candidates <- unique(normalizePath(c(
    getwd(),
    dirname(getwd()),
    file.path(getwd(), "..")
  ), mustWork = FALSE))

  for (cand in candidates) {
    if (dir.exists(file.path(cand, "scripts")) &&
        dir.exists(file.path(cand, "data"))) {
      return(cand)
    }
  }
  normalizePath(getwd(), mustWork = FALSE)
}

repo_root <- find_repo_root()
repo_path <- function(...) file.path(repo_root, ...)

fast_test <- tolower(Sys.getenv("GEDI_FAST_TEST", "false")) %in% c("true", "1", "yes", "sim")

opt <- list(
  vars_key = c("altura_dossel","agbd","pai","cover","fhd_normal"),
  balancear_por = "fito_grupo",
  n_boot = if (fast_test) 20 else 2000,
  replace_balance = TRUE,
  seed_balance = 2025,
  n_max_lmm_por_grupo = if (fast_test) 500 else 2500,
  seed_sample = 123,
  dirs = list(
    intermediate = repo_path("results", "intermediate"),
    main = repo_path("results", "main")
  ),
  xlsx_file = repo_path("results", "main", "manuscript_results.xlsx")
)

# ----------------------------- 1) PACKAGES -----------------------------------
need <- c("dplyr","tidyr","purrr","tibble","rlang","FSA","openxlsx",
          "lme4","lmerTest","spdep","readr","sf")

to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(need, require, character.only = TRUE))
options(dplyr.summarise.inform = FALSE)

# ----------------------------- 2) HELPERS ------------------------------------
mk_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
invisible(lapply(opt$dirs, mk_dir))
msg <- function(...) cat(paste0(..., "\n"))

write_csv_safe <- function(x, path) {
  mk_dir(dirname(path))
  readr::write_csv(x, path, na = "")
  invisible(path)
}

safe_sheet_name <- function(nm, existing = character(0)) {
  nm <- gsub("[:\\\\/?*\\[\\]]", "_", nm)
  nm <- trimws(ifelse(is.na(nm) | nm == "", "Sheet", nm))
  maxlen <- 31
  if (nchar(nm) > maxlen) nm <- substr(nm, 1, maxlen)
  if (nm %in% existing) {
    base <- substr(nm, 1, maxlen - 3)
    i <- 1
    repeat {
      candidate <- paste0(base, "_", sprintf("%02d", i))
      if (!(candidate %in% existing)) { nm <- candidate; break }
      i <- i + 1
      if (i > 99) stop("Too many repeated sheet names.")
    }
  }
  nm
}

safemed <- function(x) suppressWarnings(stats::median(x, na.rm = TRUE))

epsilon2_kw <- function(H, n, k){
  max((H - k + 1) / (n - k), 0)
}

compute_dunn_tables <- function(data_long, var, ajuste = c("bonferroni","bh")){
  sub <- data_long |>
    dplyr::select(fito_grupo, !!rlang::sym(var)) |>
    dplyr::filter(is.finite(.data[[var]]))
  if (nrow(sub) == 0 || dplyr::n_distinct(sub$fito_grupo) < 2) return(NULL)

  res <- list()

  if ("bonferroni" %in% ajuste) {
    rb <- try(FSA::dunnTest(as.formula(paste(var, "~ fito_grupo")),
                            data = sub, method = "bonferroni")$res, silent = TRUE)
    if (!inherits(rb, "try-error")) {
      rb$variavel <- var
      rb$ajuste <- "Bonferroni"
      res <- c(res, list(rb))
    }
  }

  if ("bh" %in% ajuste) {
    rf <- try(FSA::dunnTest(as.formula(paste(var, "~ fito_grupo")),
                            data = sub, method = "bh")$res, silent = TRUE)
    if (!inherits(rf, "try-error")) {
      rf$variavel <- var
      rf$ajuste <- "FDR"
      res <- c(res, list(rf))
    }
  }

  if (!length(res)) return(NULL)

  dplyr::bind_rows(res) |>
    dplyr::rename(
      comparacao = Comparison,
      z = Z,
      p_unadj = P.unadj,
      p_adj = P.adj
    )
}

calc_r_from_dunn <- function(dunn_tbl, n_tab){
  if (is.null(dunn_tbl) || !nrow(dunn_tbl)) return(NULL)

  dunn_tbl |>
    tidyr::separate(comparacao, into = c("grp1","grp2"), sep = "\\s*-\\s*", remove = FALSE) |>
    dplyr::left_join(n_tab |> dplyr::rename(grp1 = fito_grupo, n1 = n_grupo), by = "grp1") |>
    dplyr::left_join(n_tab |> dplyr::rename(grp2 = fito_grupo, n2 = n_grupo), by = "grp2") |>
    dplyr::mutate(
      N_ref = n1 + n2,
      r = as.numeric(abs(z)) / sqrt(as.numeric(N_ref)),
      r_mag = dplyr::case_when(
        r < 0.10 ~ "trivial",
        r < 0.30 ~ "small",
        r < 0.50 ~ "medium",
        TRUE ~ "large"
      )
    ) |>
    dplyr::arrange(variavel, dplyr::desc(r)) |>
    dplyr::select(variavel, comparacao, z, N_ref, r, r_mag, ajuste, dplyr::any_of("p_unadj"), p_adj)
}

balance_once <- function(dd, group_col, n_target, replace = TRUE){
  dd |>
    dplyr::group_by(!!rlang::sym(group_col)) |>
    dplyr::group_modify(~ dplyr::slice_sample(.x, n = n_target, replace = replace)) |>
    dplyr::ungroup()
}

flatten_for_excel <- function(df){
  if (is.null(df)) return(data.frame())
  df2 <- tryCatch(sf::st_drop_geometry(df), error = function(e) df)
  df2 <- df2 |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~{
          if (inherits(., "units")) return(as.numeric(.))
          if (is.factor(.)) return(as.character(.))
          if (is.list(.)) {
            return(vapply(., function(x){
              paste0(as.character(x), collapse = ",")
            }, FUN.VALUE = character(1)))
          }
          .
        }
      )
    )
  as.data.frame(df2, stringsAsFactors = FALSE)
}

# ----------------------------- 3) LOAD PREPARED DATA --------------------------
prep_file <- repo_path("results", "intermediate", "01_manuscript_prepared_dataset.rds")
if (!file.exists(prep_file)) {
  stop("Prepared dataset not found. Run scripts/01_prepare_manuscript_dataset.R first.")
}
prep <- readRDS(prep_file)

df <- prep$df_balanced
vars_key <- opt$vars_key

msg("Running manuscript statistics with n_boot = ", opt$n_boot,
    if (fast_test) " [FAST TEST]" else " [FINAL MODE]")

# ----------------------------- 4) DESCRIPTIVE STATS ---------------------------
desc_expl <- df |>
  dplyr::select(fito_grupo, dplyr::all_of(vars_key)) |>
  tidyr::pivot_longer(-fito_grupo, names_to = "variavel", values_to = "valor") |>
  dplyr::mutate(valor = suppressWarnings(as.numeric(valor))) |>
  dplyr::group_by(fito_grupo, variavel) |>
  dplyr::summarise(
    n = sum(is.finite(valor)),
    mean = mean(valor, na.rm = TRUE),
    sd = stats::sd(valor, na.rm = TRUE),
    p25 = as.numeric(stats::quantile(valor, .25, na.rm = TRUE, names = FALSE)),
    median = safemed(valor),
    p75 = as.numeric(stats::quantile(valor, .75, na.rm = TRUE, names = FALSE)),
    min = suppressWarnings(as.numeric(min(valor, na.rm = TRUE))),
    max = suppressWarnings(as.numeric(max(valor, na.rm = TRUE))),
    .groups = "drop"
  )

# ----------------------------- 5) KW SHOT-LEVEL EXPLORATORY -------------------
kw_global_shots <- purrr::map_dfr(vars_key, function(v){
  sub <- df |>
    dplyr::select(fito_grupo, !!rlang::sym(v)) |>
    dplyr::filter(is.finite(.data[[v]]))
  if (nrow(sub) == 0 || dplyr::n_distinct(sub$fito_grupo) < 2) return(NULL)
  kw <- stats::kruskal.test(as.formula(paste(v, "~ fito_grupo")), data = sub)
  tibble::tibble(
    variavel = v,
    teste = "Kruskal_shots",
    p_global = kw$p.value,
    efeito_epsilon2 = epsilon2_kw(
      as.numeric(kw$statistic),
      nrow(sub),
      nlevels(sub$fito_grupo)
    )
  )
})

# ----------------------------- 6) POLYGON-LEVEL BOOTSTRAP ---------------------
df_area <- df |>
  dplyr::group_by(id_amostra, fito_grupo) |>
  dplyr::summarise(
    dplyr::across(dplyr::all_of(vars_key), ~ stats::median(.x, na.rm = TRUE)),
    .groups = "drop"
  )

tab_counts_area <- df_area |>
  dplyr::count(!!rlang::sym(opt$balancear_por), name = "n")

if (nrow(tab_counts_area) < 2) stop("Fewer than two groups after polygon aggregation.")

n_target <- min(tab_counts_area$n, na.rm = TRUE)
msg("Polygon bootstrap target per group = ", n_target)

set.seed(opt$seed_balance)
boot_kw_area <- list()
boot_dunn_all <- list()

for (b in seq_len(opt$n_boot)) {
  if (b %% max(1, floor(opt$n_boot / 10)) == 0 || b == 1) {
    msg("Bootstrap replicate ", b, " / ", opt$n_boot)
  }

  dfb <- balance_once(
    df_area,
    group_col = opt$balancear_por,
    n_target = n_target,
    replace = opt$replace_balance
  )

  kwb <- purrr::map_dfr(vars_key, function(v){
    sub <- dfb |>
      dplyr::select(fito_grupo, !!rlang::sym(v)) |>
      dplyr::filter(is.finite(.data[[v]]))
    if (nrow(sub) == 0 || dplyr::n_distinct(sub$fito_grupo) < 2) return(NULL)
    kw <- stats::kruskal.test(as.formula(paste(v, "~ fito_grupo")), data = sub)
    tibble::tibble(
      replica = b,
      variavel = v,
      p_global = kw$p.value,
      efeito_epsilon2 = epsilon2_kw(
        as.numeric(kw$statistic),
        nrow(sub),
        nlevels(sub$fito_grupo)
      )
    )
  })
  if (nrow(kwb)) boot_kw_area[[b]] <- kwb

  dunn_b <- purrr::map_dfr(vars_key, ~ compute_dunn_tables(dfb, .x))
  if (nrow(dunn_b)) {
    n_tab <- dfb |> dplyr::count(fito_grupo, name = "n_grupo")
    r_b <- calc_r_from_dunn(dunn_b, n_tab)
    if (!is.null(r_b) && nrow(r_b)) {
      r_b$replica <- b
      boot_dunn_all[[b]] <- r_b
    }
  }
}

boot_kw_area_df <- if (length(boot_kw_area)) purrr::list_rbind(boot_kw_area) else tibble::tibble()
boot_dunn_all_df <- if (length(boot_dunn_all)) purrr::list_rbind(boot_dunn_all) else tibble::tibble()

kw_area_summary <- if (nrow(boot_kw_area_df)) {
  boot_kw_area_df |>
    dplyr::group_by(variavel) |>
    dplyr::summarise(
      p_global_mediana = stats::median(p_global, na.rm = TRUE),
      eps2_mediana = stats::median(efeito_epsilon2, na.rm = TRUE),
      .groups = "drop"
    )
} else tibble::tibble()

dunn_summary <- if (nrow(boot_dunn_all_df)) {
  boot_dunn_all_df |>
    dplyr::mutate(sig = !is.na(p_adj) & p_adj < 0.05) |>
    dplyr::group_by(variavel, comparacao, ajuste) |>
    dplyr::summarise(
      prop_sig = mean(sig, na.rm = TRUE),
      r_mediana = stats::median(as.numeric(r), na.rm = TRUE),
      r_p025 = tryCatch(as.numeric(stats::quantile(as.numeric(r), 0.025, na.rm = TRUE, names = FALSE)), error = function(e) NA_real_),
      r_p975 = tryCatch(as.numeric(stats::quantile(as.numeric(r), 0.975, na.rm = TRUE, names = FALSE)), error = function(e) NA_real_),
      n_replicas = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(variavel, dplyr::desc(r_mediana))
} else tibble::tibble()

# ----------------------------- 7) LMM AND MORAN -------------------------------
set.seed(opt$seed_sample)

df_lmm <- df |>
  dplyr::select(id_amostra, fito_grupo, dplyr::all_of(vars_key),
                lat = latitude_bin0, lon = longitude_bin0) |>
  dplyr::filter(dplyr::if_any(dplyr::all_of(vars_key), is.finite)) |>
  dplyr::group_by(fito_grupo) |>
  dplyr::group_modify(~ dplyr::slice_sample(.x, n = min(nrow(.x), opt$n_max_lmm_por_grupo))) |>
  dplyr::ungroup()

format_anova <- function(anova_obj, var_name){
  tab <- as.data.frame(anova_obj)
  tab$variavel <- var_name
  nm <- names(tab)
  nm <- sub("NumDF", "df_num", nm, ignore.case = TRUE)
  nm <- sub("DenDF", "df_den", nm, ignore.case = TRUE)
  nm <- sub("^Df$", "df_num", nm)
  nm <- sub("^F value$", "F_value", nm)
  nm <- sub("^Pr\\(>F\\)$", "p_value", nm)
  names(tab) <- nm
  tab[] <- lapply(tab, function(col){
    if (inherits(col, "units")) return(as.numeric(col))
    if (is.list(col)) return(suppressWarnings(as.numeric(unlist(col))))
    if (is.factor(col)) return(as.character(col))
    col
  })
  tibble::as_tibble(tab, .name_repair = "unique")
}

fit_lmm <- function(var){
  sub <- df_lmm |>
    dplyr::select(id_amostra, fito_grupo, !!rlang::sym(var)) |>
    dplyr::filter(is.finite(.data[[var]]))

  if (nrow(sub) < 10 || dplyr::n_distinct(sub$id_amostra) < 5) return(NULL)

  f <- stats::as.formula(paste(var, "~ fito_grupo + (1|id_amostra)"))
  m <- try(lme4::lmer(f, data = sub, REML = TRUE), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)

  an <- try(lmerTest::anova(m), silent = TRUE)
  if (inherits(an, "try-error")) an <- stats::anova(m)

  list(model = m, anova = format_anova(an, var))
}

lmm_results <- purrr::map(vars_key, fit_lmm)
names(lmm_results) <- vars_key

lmm_anova <- purrr::imap_dfr(lmm_results, function(res, var){
  if (is.null(res)) return(NULL)
  res$anova
})

calc_moran_residuos <- function(res_lmm, var_name) {
  if (is.null(res_lmm) || is.null(res_lmm$model)) return(NULL)

  m <- res_lmm$model
  dat_m <- model.frame(m)
  res_m <- residuals(m)

  res_area <- tibble::tibble(
    id_amostra = dat_m$id_amostra,
    res = as.numeric(res_m)
  ) |>
    dplyr::group_by(id_amostra) |>
    dplyr::summarise(res = mean(res, na.rm = TRUE), .groups = "drop")

  coords_area <- df |>
    dplyr::select(id_amostra, lat = latitude_bin0, lon = longitude_bin0) |>
    dplyr::group_by(id_amostra) |>
    dplyr::summarise(lat = mean(lat, na.rm = TRUE), lon = mean(lon, na.rm = TRUE), .groups = "drop") |>
    stats::na.omit()

  da <- dplyr::inner_join(res_area, coords_area, by = "id_amostra") |>
    dplyr::filter(is.finite(res), is.finite(lat), is.finite(lon))

  if (nrow(da) < 10) return(NULL)

  da_u <- da |>
    dplyr::mutate(lon_r = round(lon, 6), lat_r = round(lat, 6)) |>
    dplyr::group_by(lon_r, lat_r) |>
    dplyr::summarise(
      res = mean(res, na.rm = TRUE),
      lon = mean(lon),
      lat = mean(lat),
      n_areas_agr = dplyr::n(),
      .groups = "drop"
    )

  nU <- nrow(da_u)
  if (nU < 10) return(NULL)

  k <- max(1, min(8, nU - 1))
  pts <- as.matrix(da_u[, c("lon","lat")])

  nb_obj <- try(spdep::knearneigh(pts, k = k), silent = TRUE)
  if (inherits(nb_obj, "try-error")) {
    set.seed(123)
    eps <- 1e-6
    pts_j <- pts + matrix(stats::rnorm(length(pts), sd = eps), ncol = 2)
    nb_obj <- spdep::knearneigh(pts_j, k = k)
  }

  nb <- spdep::knn2nb(nb_obj)
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  mi <- spdep::moran.test(da_u$res, lw, zero.policy = TRUE, na.action = na.exclude)

  tibble::tibble(
    variavel = var_name,
    n_areas = nU,
    k = k,
    moran_i = unname(mi$estimate[["Moran I statistic"]]),
    e_i = unname(mi$estimate[["Expectation"]]),
    var_i = unname(mi$estimate[["Variance"]]),
    p_value = mi$p.value,
    metodo_viz = "kNN (unique coords; jitter if needed)"
  )
}

moran_tab <- purrr::map2_dfr(lmm_results, vars_key, ~ calc_moran_residuos(.x, .y))

# ----------------------------- 8) SUMMARY TABLES ------------------------------
n_areas_pos <- df |>
  dplyr::distinct(id_amostra, fito_grupo) |>
  dplyr::count(fito_grupo, name = "n_areas_pos_buffer")

n_shots_pos <- df |>
  dplyr::count(fito_grupo, name = "n_shots_pos_buffer")

if (!is.null(prep$area_resumo)) {
  area_gr <- prep$area_resumo |>
    dplyr::filter(fito_grupo_pol %in% levels(df$fito_grupo)) |>
    dplyr::rename(fito_grupo = fito_grupo_pol)
} else {
  area_gr <- NULL
}

resumo_grupo <- dplyr::full_join(n_areas_pos, n_shots_pos, by = "fito_grupo")
if (!is.null(area_gr)) resumo_grupo <- dplyr::full_join(resumo_grupo, area_gr, by = "fito_grupo")

top3 <- NULL
if (nrow(dunn_summary)) {
  top3 <- dunn_summary |>
    dplyr::group_by(variavel) |>
    dplyr::slice_max(order_by = r_mediana, n = 3, with_ties = FALSE) |>
    dplyr::ungroup()
}


# ----------------------------- 8.1) SCIENTIFIC ENGLISH STANDARDIZATION --------
# The internal column names reproduce the original manuscript workbook exactly.
# The following helpers create parallel outputs with English scientific labels,
# without changing the numerical results or the original workbook structure.

metric_code_en <- c(
  altura_dossel = "H",
  agbd          = "AGBD",
  pai           = "PAI",
  cover         = "COVER",
  fhd_normal    = "FHD"
)

metric_label_en <- c(
  altura_dossel = "Canopy height (H, m)",
  agbd          = "Aboveground biomass density (AGBD, Mg ha^-1)",
  pai           = "Plant area index (PAI, m^2 m^-2)",
  cover         = "Canopy cover (COVER, %)",
  fhd_normal    = "Foliage height diversity (FHD)"
)

phys_code_en <- c(FOD = "DOF", FOM = "MOF", FES = "SSdF", FED = "SDF")
phys_name_en <- c(
  FOD = "Dense Ombrophilous Forest (DOF)",
  FOM = "Mixed Ombrophilous Forest (MOF)",
  FES = "Seasonal Semideciduous Forest (SSdF)",
  FED = "Seasonal Deciduous Forest (SDF)"
)

edge_method_en <- c(
  erosao_geometrica = "geometric erosion",
  distancia_borda   = "edge-distance fallback"
)

rename_if_present <- function(df, mapping) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  for (old in names(mapping)) {
    new <- unname(mapping[[old]])
    if (old %in% names(df) && !(new %in% names(df))) names(df)[names(df) == old] <- new
  }
  df
}

standardize_comparison_en <- function(x) {
  x <- as.character(x)
  vapply(x, function(comp) {
    if (is.na(comp) || !nzchar(comp)) return(comp)
    parts <- strsplit(comp, "\\s*-\\s*")[[1]]
    if (length(parts) != 2) return(comp)
    a <- dplyr::recode(parts[1], !!!phys_code_en, .default = parts[1])
    b <- dplyr::recode(parts[2], !!!phys_code_en, .default = parts[2])
    paste0(a, "\u2013", b)
  }, character(1))
}

standardize_scientific_english <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  df <- tibble::as_tibble(df)

  if ("variavel" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        metric_raw = as.character(variavel),
        metric = dplyr::recode(as.character(variavel), !!!metric_code_en, .default = as.character(variavel)),
        metric_label = dplyr::recode(as.character(variavel), !!!metric_label_en, .default = as.character(variavel))
      ) |>
      dplyr::select(-variavel) |>
      dplyr::relocate(metric, metric_label, metric_raw)
  }

  if ("fito_grupo" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        physiognomy_raw = as.character(fito_grupo),
        physiognomy = dplyr::recode(as.character(fito_grupo), !!!phys_code_en, .default = as.character(fito_grupo)),
        physiognomy_name = dplyr::recode(as.character(fito_grupo), !!!phys_name_en, .default = as.character(fito_grupo))
      ) |>
      dplyr::select(-fito_grupo) |>
      dplyr::relocate(physiognomy, physiognomy_name, physiognomy_raw)
  }

  if ("fito_grupo_pol" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        physiognomy_raw = as.character(fito_grupo_pol),
        physiognomy = dplyr::recode(as.character(fito_grupo_pol), !!!phys_code_en, .default = as.character(fito_grupo_pol)),
        physiognomy_name = dplyr::recode(as.character(fito_grupo_pol), !!!phys_name_en, .default = as.character(fito_grupo_pol))
      ) |>
      dplyr::select(-fito_grupo_pol) |>
      dplyr::relocate(physiognomy, physiognomy_name, physiognomy_raw)
  }

  if ("comparacao" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        comparison_raw = as.character(comparacao),
        comparison = standardize_comparison_en(comparacao)
      ) |>
      dplyr::select(-comparacao) |>
      dplyr::relocate(comparison, comparison_raw)
  }

  if ("metodo_borda" %in% names(df)) {
    df <- df |>
      dplyr::mutate(edge_filter_method = dplyr::recode(as.character(metodo_borda), !!!edge_method_en, .default = as.character(metodo_borda))) |>
      dplyr::select(-metodo_borda)
  }

  df <- rename_if_present(df, c(
    id_amostra = "polygon_id",
    teste = "test",
    ajuste = "p_adjustment",
    p_global = "global_p_value",
    p_global_mediana = "median_global_p",
    efeito_epsilon2 = "epsilon_squared",
    eps2_mediana = "median_epsilon_squared",
    z = "z_statistic",
    p_unadj = "unadjusted_p_value",
    p_adj = "adjusted_p_value",
    N_ref = "reference_sample_size",
    r = "effect_size_r",
    r_mag = "effect_size_magnitude",
    prop_sig = "significance_frequency",
    r_mediana = "median_effect_size_r",
    r_p025 = "effect_size_r_ci95_lower",
    r_p975 = "effect_size_r_ci95_upper",
    n_replicas = "n_bootstrap_replicates",
    replica = "bootstrap_replicate",
    n_grupo = "group_sample_size",
    n_areas = "n_polygons",
    n_areas_agr = "n_aggregated_polygons",
    n_areas_pos_buffer = "n_polygons_after_buffer_and_shot_balancing",
    n_shots_pos_buffer = "n_shots_after_buffer_and_shot_balancing",
    n_poligonos = "n_polygons",
    area_km2 = "area_km2",
    area_med_km2 = "mean_area_km2",
    area_p50_km2 = "median_area_km2",
    etapa = "processing_step",
    metodo = "method",
    n_total = "n_total",
    n_mantidos = "n_kept",
    n_removidos = "n_removed",
    prop_removida = "proportion_removed",
    perdeu_tudo = "lost_all_shots_after_filter",
    n_pre = "n_before",
    n_pos = "n_after",
    titulo = "section_title",
    nota = "note",
    caminho_csv = "input_csv",
    caminho_poly = "input_polygon_file",
    layer_poly = "polygon_layer",
    n_total_prebuffer = "n_total_prebuffer",
    n_total_posbuffer = "n_total_postbuffer",
    moran_i = "moran_i",
    e_i = "expected_i",
    var_i = "variance_i",
    metodo_viz = "neighbourhood_method"
  ))

  df
}

make_run_log_en <- function() {
  tibble::tibble(
    input_csv = prep$opt$path_csv,
    input_polygon_file = prep$opt$poly_path,
    polygon_layer = prep$opt$poly_layer,
    edge_filter_method = dplyr::recode(as.character(prep$metodo_borda), !!!edge_method_en, .default = as.character(prep$metodo_borda)),
    buffer_m = prep$opt$buffer_m,
    n_total_prebuffer = prep$impacto_global$n_total,
    n_total_postbuffer = prep$impacto_global$n_mantidos,
    n_bootstrap_replicates = opt$n_boot,
    fast_test = fast_test,
    manuscript_exact_mode = !fast_test
  )
}

# English-standardized copies for CSV and Excel outputs
english_results <- list(
  run_log = make_run_log_en(),
  area_summary = standardize_scientific_english(flatten_for_excel(prep$area_resumo)),
  edge_filter_global = standardize_scientific_english(prep$impacto_global),
  edge_filter_by_physiognomy = standardize_scientific_english(prep$impacto_grupo),
  edge_filter_by_polygon = standardize_scientific_english(prep$impacto_area),
  shot_counts_before_balancing = standardize_scientific_english(prep$shots_counts_pre),
  shot_counts_after_balancing = standardize_scientific_english(prep$shots_counts_pos),
  descriptive_statistics = standardize_scientific_english(desc_expl),
  kw_shot_exploratory = standardize_scientific_english(kw_global_shots),
  polygon_medians = standardize_scientific_english(df_area),
  polygon_counts = standardize_scientific_english(tab_counts_area),
  kw_polygon_bootstrap = standardize_scientific_english(kw_area_summary),
  dunn_polygon_bootstrap = standardize_scientific_english(dunn_summary),
  lmm_anova = standardize_scientific_english(lmm_anova),
  moran_i = standardize_scientific_english(moran_tab),
  group_summary = standardize_scientific_english(resumo_grupo),
  top3_pairwise_effects = standardize_scientific_english(top3)
)

# ----------------------------- 9) ENGLISH-ONLY EXPORTS ------------------------
# Repository-facing outputs are written only in scientific English.
# Numerical values are unchanged relative to the manuscript-reference workflow.

tables_dir <- repo_path("results", "main", "tables")
mk_dir(tables_dir)

for (nm in names(english_results)) {
  tab <- english_results[[nm]]
  if (!is.null(tab) && is.data.frame(tab) && nrow(tab) > 0) {
    write_csv_safe(tab, file.path(tables_dir, paste0(nm, ".csv")))
  }
}

# ----------------------------- 10) ENGLISH WORKBOOK --------------------------
# This is the only Excel workbook generated by this repository workflow.
# Sheet names, column names, metric labels, and physiognomy labels are in English.

wb <- openxlsx::createWorkbook()
used_sheets <- character(0)
add_sheet <- function(nm){
  nm2 <- safe_sheet_name(nm, used_sheets)
  openxlsx::addWorksheet(wb, nm2)
  used_sheets <<- c(used_sheets, nm2)
  nm2
}

sheet_order <- c(
  "run_log",
  "area_summary",
  "edge_filter_global",
  "edge_filter_by_physiognomy",
  "edge_filter_by_polygon",
  "shot_counts_before_balancing",
  "shot_counts_after_balancing",
  "descriptive_statistics",
  "kw_shot_exploratory",
  "polygon_medians",
  "polygon_counts",
  "kw_polygon_bootstrap",
  "dunn_polygon_bootstrap",
  "lmm_anova",
  "moran_i",
  "group_summary",
  "top3_pairwise_effects"
)

for (nm in sheet_order) {
  tab <- english_results[[nm]]
  if (!is.null(tab) && is.data.frame(tab) && nrow(tab) > 0) {
    sh <- add_sheet(nm)
    openxlsx::writeData(wb, sh, tab)
    openxlsx::freezePane(wb, sh, firstRow = TRUE)
    openxlsx::setColWidths(wb, sh, cols = 1:ncol(tab), widths = "auto")
  }
}

openxlsx::saveWorkbook(wb, opt$xlsx_file, overwrite = TRUE)

# ----------------------------- 11) INTERNAL RDS ------------------------------
stats <- list(
  prep_file = prep_file,
  opt = opt,
  desc_expl = desc_expl,
  kw_global_shots = kw_global_shots,
  df_area = df_area,
  tab_counts_area = tab_counts_area,
  boot_kw_area_df = boot_kw_area_df,
  boot_dunn_all_df = boot_dunn_all_df,
  kw_area_summary = kw_area_summary,
  dunn_summary = dunn_summary,
  lmm_anova = lmm_anova,
  moran_tab = moran_tab,
  resumo_grupo = resumo_grupo,
  top3 = top3,
  english_results = english_results
)
saveRDS(stats, repo_path("results", "intermediate", "02_manuscript_statistics.rds"))

capture.output(sessionInfo(), file = repo_path("results", "main", "session_info_02_statistics.txt"))

msg("\nEnglish workbook exported:")
msg(opt$xlsx_file)
msg("\nKey manuscript table: results/main/tables/kw_polygon_bootstrap.csv")
print(english_results$kw_polygon_bootstrap, n = Inf)
msg("\nDone: 02_run_manuscript_statistics.R")
