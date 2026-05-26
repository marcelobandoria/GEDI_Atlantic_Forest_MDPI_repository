# -----------------------------------------------------------------------------
# Script 01 — Prepare the manuscript analytical dataset
#
# Project: GEDI-derived forest structure in Atlantic Forest physiognomies
# Repository workflow: manuscript reproducibility
# Author: Marcelo C. S. Bandoria
#
# Purpose
# This script prepares the same analytical dataset used to generate the
# manuscript results. It reads the final GEDI footprint table, derives the
# structural variables, applies the 60 m internal edge filter, balances the
# number of shots by forest physiognomy, and saves the prepared dataset for the
# statistical analysis in Script 02.
#
# Important reproducibility note
# The manuscript results were produced from the consolidated analytical CSV.
# Therefore, this script does not reassign polygon IDs by a new spatial join and
# does not reapply additional GEDI quality-control filters in the main workflow.
# Those checks belong to separate robustness/sensitivity analyses.
#
# Main inputs
#   data/gedi_footprints_filtered.csv
#   spatial/old_growth_candidate_polygons.gpkg
#
# Main outputs
#   results/intermediate/01_manuscript_prepared_dataset.rds
#   results/main/tables/edge_filter_global.csv
#   results/main/tables/group_summary.csv
#   results/main/tables/diagnostics_expected_counts.csv
#
# Run first. For a quick structural test, run:
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
  path_csv   = repo_path("data", "gedi_footprints_filtered.csv"),
  poly_path  = repo_path("spatial", "old_growth_candidate_polygons.gpkg"),
  poly_layer = "atual__samples",

  # CSV columns
  lat_col  = "latitude_bin0",
  lon_col  = "longitude_bin0",
  id_col   = "id_amostra",
  fito_col = "fitofisionomia",

  # Polygon columns
  poly_id_col   = NULL,
  poly_fito_col = NULL,

  # Main manuscript behavior
  apply_baseline_qc = FALSE,       # keep FALSE to reproduce the manuscript workbook
  use_spatial_join_for_id = FALSE, # keep FALSE to reproduce the manuscript workbook
  buffer_m    = 60,
  crs_metrico = 3857,

  # Shot balancing used by the manuscript workbook
  balancear_shots    = TRUE,
  shots_target       = NULL,       # NULL = minimum group size after buffer
  replace_shots      = FALSE,
  seed_balance_shots = 2025,

  calcular_areas     = TRUE,
  salvar_lista_polig = FALSE,

  dirs = list(
    intermediate = repo_path("results", "intermediate"),
    main         = repo_path("results", "main"),
    figures      = repo_path("results", "figures")
  ),

  expected = list(
    n_prebuffer       = 252152,
    n_postbuffer      = 238961,
    n_removed_buffer  = 13191,
    balanced_per_group= 12440
  )
)

# ----------------------------- 1) PACKAGES -----------------------------------
need <- c("readr","dplyr","tidyr","stringr","janitor","purrr",
          "sf","rlang","units","tibble")

to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(need, require, character.only = TRUE))
options(dplyr.summarise.inform = FALSE)

if (!requireNamespace("lwgeom", quietly = TRUE)) {
  message("Package 'lwgeom' not found; geodetic-area fallback will be used if needed.")
}

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

to_ascii_upper <- function(x){
  toupper(iconv(x, from = "", to = "ASCII//TRANSLIT"))
}

extrair_fito_grupo <- function(x){
  xu   <- to_ascii_upper(as.character(x))
  pref <- stringr::str_match(xu, "^(FOD|FOM|FES|FED)")[, 2]
  anyw <- ifelse(is.na(pref), stringr::str_match(xu, "(FOD|FOM|FES|FED)")[, 1], pref)

  miss <- which(is.na(anyw))
  if (length(miss)) {
    xr <- xu[miss]
    is_fod <- grepl("OMBROFILA\\s*DENSA|FLORESTA\\s*OMBROFILA\\s*DENSA", xr)
    is_fom <- grepl("OMBROFILA\\s*MISTA|ARAUCARIA", xr)
    is_fes <- grepl("ESTACIONAL\\s*SEMIDECIDUAL|SEMIDECIDUAL", xr)
    is_fed <- grepl("ESTACIONAL\\s*DECIDUAL|DECIDUAL", xr)
    anyw[miss] <- ifelse(is_fod, "FOD",
                          ifelse(is_fom, "FOM",
                                 ifelse(is_fes, "FES",
                                        ifelse(is_fed, "FED", NA))))
  }
  factor(anyw, levels = c("FOD","FOM","FES","FED"))
}

cover_to_percent <- function(x){
  if (!is.numeric(x)) return(x)
  vmax <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(vmax)) return(x)
  if (vmax <= 1.05) 100 * x else x
}

calc_H <- function(h_top, z2b, z4a){
  h_top - dplyr::coalesce(z2b, z4a)
}

na_outside <- function(x, minv, maxv){
  if (!is.numeric(x)) return(x)
  x[!is.finite(x)] <- NA_real_
  x[x < minv | x > maxv] <- NA_real_
  x
}

geod_area_km2 <- function(s){
  s <- sf::st_make_valid(s)
  if (requireNamespace("lwgeom", quietly = TRUE) &&
      ("st_geod_area" %in% getNamespaceExports("lwgeom"))) {
    return(as.numeric(lwgeom::st_geod_area(s)) / 1e6)
  }
  if ("st_geod_area" %in% getNamespaceExports("sf")) {
    return(as.numeric(sf::st_geod_area(s)) / 1e6)
  }
  s_eq <- try(sf::st_transform(s, 6933), silent = TRUE)
  if (inherits(s_eq, "try-error")) s_eq <- sf::st_transform(s, 3857)
  as.numeric(sf::st_area(s_eq)) / 1e6
}

compare_expected <- function(name, observed, expected) {
  tibble::tibble(
    item = name,
    observed = observed,
    expected = expected,
    difference = observed - expected,
    status = ifelse(is.na(expected), "not_checked",
                    ifelse(observed == expected, "OK", "CHECK"))
  )
}

# ----------------------------- 3) READ DATA ----------------------------------
msg("Repository root: ", repo_root)
msg("Reading analytical GEDI CSV: ", opt$path_csv)
if (!file.exists(opt$path_csv)) stop("CSV not found: ", opt$path_csv)

df_raw <- readr::read_csv(opt$path_csv, show_col_types = FALSE) |>
  janitor::clean_names()

required_cols <- c(opt$lat_col, opt$lon_col, opt$id_col, opt$fito_col,
                   "elev_highestreturn", "cover", "pai", "fhd_normal", "agbd")
missing_req <- setdiff(required_cols, names(df_raw))
if (length(missing_req)) stop("Missing required columns in CSV: ", paste(missing_req, collapse = ", "))

numeric_candidates <- intersect(
  c("elev_highestreturn","elev_lowestmode_2b","elev_lowestmode_4a",
    "cover","pai","fhd_normal","agbd", opt$lat_col, opt$lon_col,
    "l2b_quality_flag","degrade_flag_2b","l4_quality_flag","degrade_flag_4a",
    "sensitivity_2b","sensitivity_4a","solar_elevation_2b","solar_elevation_4a",
    "agbd_se","agbd_pi_lower","agbd_pi_upper"),
  names(df_raw)
)
for (nm in numeric_candidates) {
  df_raw[[nm]] <- suppressWarnings(readr::parse_number(as.character(df_raw[[nm]])))
}

# ----------------------------- 4) DERIVE VARIABLES ----------------------------
df <- df_raw |>
  dplyr::mutate(
    fito_grupo = extrair_fito_grupo(.data[[opt$fito_col]]),
    altura_dossel = calc_H(
      elev_highestreturn,
      if ("elev_lowestmode_2b" %in% names(dplyr::cur_data_all())) elev_lowestmode_2b else NA_real_,
      if ("elev_lowestmode_4a" %in% names(dplyr::cur_data_all())) elev_lowestmode_4a else NA_real_
    ),
    cover = if ("cover" %in% names(dplyr::cur_data_all())) cover_to_percent(cover) else NA_real_,
    altura_dossel = na_outside(altura_dossel, 0, 120),
    cover         = na_outside(cover,         0, 100),
    pai           = if ("pai" %in% names(dplyr::cur_data_all())) na_outside(pai, 0, 15) else NA_real_,
    agbd          = if ("agbd" %in% names(dplyr::cur_data_all())) na_outside(agbd, 0, 1000) else NA_real_,
    fhd_normal    = if ("fhd_normal" %in% names(dplyr::cur_data_all())) na_outside(fhd_normal, 0, 15) else NA_real_
  ) |>
  dplyr::filter(fito_grupo %in% c("FOD","FOM","FES","FED")) |>
  droplevels()

# Preserve manuscript behavior: use id_amostra already stored in CSV.
if (!opt$id_col %in% names(df)) stop("The manuscript-reproduction workflow requires id_amostra in the CSV.")
names(df)[names(df) == opt$id_col] <- "id_amostra"
df$id_amostra <- as.character(df$id_amostra)

if (isTRUE(opt$apply_baseline_qc)) {
  stop("apply_baseline_qc is TRUE. Set it to FALSE to reproduce the manuscript-reference workflow.")
}

# ----------------------------- 5) READ POLYGONS -------------------------------
msg("Reading sampling polygons: ", opt$poly_path)
if (!file.exists(opt$poly_path)) stop("Polygon GeoPackage not found: ", opt$poly_path)

pol <- sf::st_read(opt$poly_path, layer = opt$poly_layer, quiet = TRUE) |>
  sf::st_make_valid()

cand_id <- unique(na.omit(c(opt$poly_id_col, "id_amostra","id","ID","Id",
                            "sample_id","amostra","gid","fid")))
hit_id <- cand_id[cand_id %in% names(pol)][1]
if (is.na(hit_id)) {
  warning("Polygon ID column not found; creating synthetic id_amostra for polygon-area reporting only.")
  pol$id_amostra <- sprintf("poly_%05d", seq_len(nrow(pol)))
} else {
  pol <- pol |> dplyr::rename(id_amostra = !!rlang::sym(hit_id))
}
pol$id_amostra <- as.character(pol$id_amostra)

cand_fito <- unique(na.omit(c(opt$poly_fito_col, "fitofisionomia","fito","fito_grupo",
                              "classe","classe_fito","vegetacao")))
hit_fito <- cand_fito[cand_fito %in% names(pol)][1]
if (!is.na(hit_fito) && hit_fito != "fitofisionomia") {
  pol <- pol |> dplyr::rename(fitofisionomia = !!rlang::sym(hit_fito))
}
if (!"fitofisionomia" %in% names(pol)) pol$fitofisionomia <- NA_character_

pol$fito_grupo_pol <- extrair_fito_grupo(pol$fitofisionomia)

area_resumo <- NULL
pol_list_export <- NULL
if (isTRUE(opt$calcular_areas)) {
  pol$area_km2 <- geod_area_km2(pol)
  pol_ng <- sf::st_drop_geometry(pol)

  area_grupo <- pol_ng |>
    dplyr::filter(!is.na(fito_grupo_pol)) |>
    dplyr::group_by(fito_grupo_pol) |>
    dplyr::summarise(
      n_poligonos  = dplyr::n(),
      area_km2     = sum(area_km2, na.rm = TRUE),
      area_med_km2 = mean(area_km2, na.rm = TRUE),
      area_p50_km2 = stats::median(area_km2, na.rm = TRUE),
      .groups      = "drop"
    ) |>
    dplyr::arrange(fito_grupo_pol)

  area_total <- tibble::tibble(
    fito_grupo_pol = "TOTAL",
    n_poligonos    = nrow(pol_ng),
    area_km2       = sum(pol_ng$area_km2, na.rm = TRUE),
    area_med_km2   = mean(pol_ng$area_km2, na.rm = TRUE),
    area_p50_km2   = stats::median(pol_ng$area_km2, na.rm = TRUE)
  )
  area_resumo <- dplyr::bind_rows(area_grupo, area_total)

  if (isTRUE(opt$salvar_lista_polig)) {
    pol_list_export <- pol_ng |> dplyr::select(id_amostra, fito_grupo_pol, area_km2)
  }
}

# ----------------------------- 6) EDGE BUFFER --------------------------------
msg("Applying 60 m internal edge buffer, preserving manuscript logic...")

poly_crs <- sf::st_crs(pol)
need_metric <- is.na(poly_crs) || poly_crs$units_gdal %in% c(NA, "degree")
pol_m <- if (need_metric) sf::st_transform(pol, opt$crs_metrico) else pol

pts <- sf::st_as_sf(
  df,
  coords = c(opt$lon_col, opt$lat_col),
  crs = 4326,
  remove = FALSE
)
pts_m <- sf::st_transform(pts, sf::st_crs(pol_m))

pol_interior <- try(suppressWarnings(sf::st_buffer(pol_m, dist = -opt$buffer_m)), silent = TRUE)
do_erosion <- !(inherits(pol_interior, "try-error") || all(sf::st_is_empty(pol_interior)))

bordas <- sf::st_boundary(sf::st_union(pol_m))
dist_borda <- as.numeric(sf::st_distance(pts_m, bordas))
df$dist_borda_m <- dist_borda

if (do_erosion) {
  keep_idx <- lengths(sf::st_intersects(pts_m, pol_interior)) > 0
  metodo_borda <- "erosao_geometrica"
} else {
  inside_idx <- lengths(sf::st_intersects(pts_m, pol_m)) > 0
  keep_idx <- inside_idx & (dist_borda > opt$buffer_m)
  metodo_borda <- "distancia_borda"
}

df$classe_borda <- ifelse(keep_idx, "MANTIDO_>60m", "REMOVIDO_<=60m")
df_bf <- df[keep_idx, , drop = FALSE]

impacto_global <- tibble::tibble(
  etapa         = "buffer_60m",
  metodo        = metodo_borda,
  n_total       = nrow(df),
  n_mantidos    = nrow(df_bf),
  n_removidos   = nrow(df) - nrow(df_bf),
  prop_removida = (nrow(df) - nrow(df_bf)) / pmax(nrow(df), 1)
)

impacto_grupo <- df |>
  dplyr::count(fito_grupo, name = "n_pre") |>
  dplyr::left_join(df_bf |> dplyr::count(fito_grupo, name = "n_pos"), by = "fito_grupo") |>
  dplyr::mutate(
    n_pos = tidyr::replace_na(n_pos, 0L),
    n_removidos = n_pre - n_pos,
    prop_removida = n_removidos / pmax(n_pre, 1)
  ) |>
  dplyr::arrange(fito_grupo)

impacto_area <- df |>
  dplyr::count(id_amostra, name = "n_pre") |>
  dplyr::left_join(df_bf |> dplyr::count(id_amostra, name = "n_pos"), by = "id_amostra") |>
  dplyr::mutate(
    n_pos = tidyr::replace_na(n_pos, 0L),
    n_removidos = n_pre - n_pos,
    perdeu_tudo = n_pos == 0
  )

# ----------------------------- 7) SHOT BALANCING ------------------------------
df_balanced <- df_bf

if (isTRUE(opt$balancear_shots)) {
  msg("Balancing shots by physiognomy as in the manuscript workbook...")
  set.seed(opt$seed_balance_shots)

  shots_counts_pre <- df_bf |>
    dplyr::count(fito_grupo, name = "n_pre")

  n_min_obs <- min(shots_counts_pre$n_pre, na.rm = TRUE)
  target <- if (is.null(opt$shots_target)) n_min_obs else min(as.integer(opt$shots_target), n_min_obs)

  df_balanced <- df_bf |>
    dplyr::group_by(fito_grupo) |>
    dplyr::group_modify(~ dplyr::slice_sample(
      .x,
      n = min(nrow(.x), target),
      replace = opt$replace_shots
    )) |>
    dplyr::ungroup()

  shots_counts_pos <- df_balanced |>
    dplyr::count(fito_grupo, name = "n_pos")
} else {
  shots_counts_pre <- tibble::tibble()
  shots_counts_pos <- tibble::tibble()
}

# ----------------------------- 8) DIAGNOSTICS --------------------------------
diagnostics_expected <- dplyr::bind_rows(
  compare_expected("n_prebuffer", nrow(df), opt$expected$n_prebuffer),
  compare_expected("n_postbuffer", nrow(df_bf), opt$expected$n_postbuffer),
  compare_expected("n_removed_buffer", nrow(df) - nrow(df_bf), opt$expected$n_removed_buffer),
  compare_expected("balanced_min_group", if (nrow(shots_counts_pos)) min(shots_counts_pos$n_pos) else NA_integer_,
                   opt$expected$balanced_per_group)
)

if (any(diagnostics_expected$status == "CHECK", na.rm = TRUE)) {
  warning("Some manuscript-control values differ. See results/main/tables/preparation_diagnostics.csv")
}

# ----------------------------- 8.1) ENGLISH-ONLY REPOSITORY OUTPUTS ----------
# All repository-readable outputs are written in scientific English.
# Internal object names are preserved only inside the RDS file to reproduce the
# manuscript-reference workflow exactly.

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

standardize_preparation_english <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  df <- tibble::as_tibble(df)

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

  if ("metodo_borda" %in% names(df)) {
    df <- df |>
      dplyr::mutate(edge_filter_method = dplyr::recode(as.character(metodo_borda), !!!edge_method_en, .default = as.character(metodo_borda))) |>
      dplyr::select(-metodo_borda)
  }

  rename_if_present(df, c(
    id_amostra = "polygon_id",
    item = "diagnostic_item",
    observed = "observed_value",
    expected = "expected_value",
    difference = "difference_from_expected",
    etapa = "processing_step",
    metodo = "method",
    n_total = "n_total",
    n_mantidos = "n_kept",
    n_removidos = "n_removed",
    prop_removida = "proportion_removed",
    n_pre = "n_before",
    n_pos = "n_after",
    perdeu_tudo = "lost_all_shots_after_edge_filter",
    n_poligonos = "n_polygons",
    area_km2 = "area_km2",
    area_med_km2 = "mean_area_km2",
    area_p50_km2 = "median_area_km2"
  ))
}

dir_english <- repo_path("results", "main", "tables")
mk_dir(dir_english)
write_csv_safe(standardize_preparation_english(diagnostics_expected), file.path(dir_english, "preparation_diagnostics.csv"))
write_csv_safe(standardize_preparation_english(impacto_global), file.path(dir_english, "edge_filter_global.csv"))
write_csv_safe(standardize_preparation_english(impacto_grupo), file.path(dir_english, "edge_filter_by_physiognomy.csv"))
write_csv_safe(standardize_preparation_english(impacto_area), file.path(dir_english, "edge_filter_by_polygon.csv"))
if (!is.null(area_resumo)) write_csv_safe(standardize_preparation_english(area_resumo), file.path(dir_english, "area_summary.csv"))
if (nrow(shots_counts_pre)) write_csv_safe(standardize_preparation_english(shots_counts_pre), file.path(dir_english, "shot_counts_before_balancing.csv"))
if (nrow(shots_counts_pos)) write_csv_safe(standardize_preparation_english(shots_counts_pos), file.path(dir_english, "shot_counts_after_balancing.csv"))

prep <- list(
  repo_root = repo_root,
  opt = opt,
  fast_test = fast_test,
  df_raw = df_raw,
  df_prebuffer = df,
  df_postbuffer = df_bf,
  df_balanced = df_balanced,
  pol = pol,
  area_resumo = area_resumo,
  pol_list_export = pol_list_export,
  impacto_global = impacto_global,
  impacto_grupo = impacto_grupo,
  impacto_area = impacto_area,
  shots_counts_pre = shots_counts_pre,
  shots_counts_pos = shots_counts_pos,
  diagnostics_expected = diagnostics_expected,
  metodo_borda = metodo_borda
)

saveRDS(prep, repo_path("results", "intermediate", "01_manuscript_prepared_dataset.rds"))

msg("\nPrepared dataset saved:")
msg(repo_path("results", "intermediate", "01_manuscript_prepared_dataset.rds"))

msg("\nExpected-count diagnostics:")
print(diagnostics_expected, n = Inf)

capture.output(sessionInfo(), file = repo_path("results", "main", "session_info_01_prepare.txt"))
msg("\nDone: 01_prepare_manuscript_dataset.R")
