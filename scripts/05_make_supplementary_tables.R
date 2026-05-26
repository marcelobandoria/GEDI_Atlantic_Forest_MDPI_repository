# -----------------------------------------------------------------------------
# Script 05 — Build supplementary tables and Supplementary Data workbook
#
# Project: GEDI-derived forest structure in Atlantic Forest physiognomies
# Repository workflow: manuscript and Supplementary Information reproducibility
# Author: Marcelo C. S. Bandoria
#
# Purpose
# This script converts the QC/acquisition-sensitivity outputs from Script 04
# into the supplementary tables cited in the submitted Supplementary Information
# (Tables S1a-S4) and into the full Supplementary Data Excel workbook.
#
# Main inputs
#   results/supplementary/04_qc_sensitivity_results.rds
#   results/supplementary/*.csv from Script 04
#
# Main outputs
#   results/supplementary/Supplementary_Data_Bandoria_et_al_2026.xlsx
#   results/supplementary/supplementary_tables.xlsx
#   results/supplementary/tables/table_s*.csv
# -----------------------------------------------------------------------------

# ----------------------------- 0) OPTIONS ------------------------------------
find_repo_root <- function() {
  candidates <- unique(normalizePath(c(getwd(), dirname(getwd()), file.path(getwd(), "..")), mustWork = FALSE))
  for (cand in candidates) {
    if (dir.exists(file.path(cand, "scripts")) && dir.exists(file.path(cand, "data"))) return(cand)
  }
  normalizePath(getwd(), mustWork = FALSE)
}

repo_root <- find_repo_root()
repo_path <- function(...) file.path(repo_root, ...)

opt <- list(
  qc_rds = repo_path("results", "supplementary", "04_qc_sensitivity_results.rds"),
  supp_dir = repo_path("results", "supplementary"),
  table_dir = repo_path("results", "supplementary", "tables"),
  full_workbook = repo_path("results", "supplementary", "Supplementary_Data_Bandoria_et_al_2026.xlsx"),
  compact_workbook = repo_path("results", "supplementary", "supplementary_tables.xlsx")
)

# ----------------------------- 1) PACKAGES -----------------------------------
need <- c("dplyr", "tidyr", "readr", "openxlsx", "tibble", "purrr", "stringr")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(need, require, character.only = TRUE))
options(dplyr.summarise.inform = FALSE)

# ----------------------------- 2) HELPERS ------------------------------------
mk_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
mk_dir(opt$supp_dir); mk_dir(opt$table_dir)
msg <- function(...) cat(paste0(..., "\n"))

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
      i <- i + 1
      if (i > 99) stop("Too many repeated sheet names.")
    }
  }
  nm
}

write_csv_safe <- function(x, file) {
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) return(invisible(NULL))
  readr::write_csv(x, file, na = "")
  invisible(file)
}

read_csv_if_exists <- function(file) {
  if (file.exists(file)) readr::read_csv(file, show_col_types = FALSE) else tibble::tibble()
}

metric_code <- c(
  altura_dossel = "H", canopy_height = "H", H = "H",
  agbd = "AGBD", AGBD = "AGBD",
  pai = "PAI", PAI = "PAI",
  cover = "COVER", COVER = "COVER",
  fhd_normal = "FHD", FHD = "FHD"
)
metric_label <- c(
  H = "Canopy height (H, m)",
  AGBD = "Aboveground biomass density (AGBD, Mg ha^-1)",
  PAI = "Plant area index (PAI, m^2 m^-2)",
  COVER = "Canopy cover (COVER, %)",
  FHD = "Foliage height diversity (FHD)"
)
metric_order <- c("H", "AGBD", "PAI", "COVER", "FHD")

std_metric <- function(x) dplyr::recode(as.character(x), !!!metric_code, .default = as.character(x))
std_metric_label <- function(x) dplyr::recode(std_metric(x), !!!metric_label, .default = std_metric(x))

first_existing_col <- function(df, candidates, default = NA) {
  hit <- intersect(candidates, names(df))
  if (length(hit) > 0) return(df[[hit[1]]])
  rep(default, nrow(df))
}

# ----------------------------- 3) LOAD QC OUTPUTS ----------------------------
if (file.exists(opt$qc_rds)) {
  qc <- readRDS(opt$qc_rds)
} else {
  warning("QC RDS not found; reading available CSV files from results/supplementary/.")
  qc <- list(
    scenario_log = read_csv_if_exists(file.path(opt$supp_dir, "scenario_log.csv")),
    kw_boot_by_scenario = read_csv_if_exists(file.path(opt$supp_dir, "kw_boot_by_scenario.csv")),
    delta_eps2_boot = read_csv_if_exists(file.path(opt$supp_dir, "delta_eps2_boot.csv")),
    dunn_boot_by_scenario = read_csv_if_exists(file.path(opt$supp_dir, "dunn_boot_by_scenario.csv")),
    delta_r_boot = read_csv_if_exists(file.path(opt$supp_dir, "delta_r_boot.csv")),
    correlations = read_csv_if_exists(file.path(opt$supp_dir, "correlations_qc_scenarios.csv")),
    redundancy_map = read_csv_if_exists(file.path(opt$supp_dir, "redundancy_map.csv")),
    lmm_anova_baseline = read_csv_if_exists(file.path(opt$supp_dir, "lmm_anova_baseline.csv")),
    moran_baseline = read_csv_if_exists(file.path(opt$supp_dir, "moran_baseline.csv"))
  )
}

get_tab <- function(name) {
  x <- qc[[name]]
  if (is.null(x) || !is.data.frame(x)) tibble::tibble() else tibble::as_tibble(x)
}

join_log <- get_tab("join_log")
preflight <- get_tab("preflight")
scenario_log <- get_tab("scenario_log")
redundancy_map <- get_tab("redundancy_map")
correlations <- get_tab("correlations")
kw_boot <- get_tab("kw_boot_by_scenario")
dunn_boot <- get_tab("dunn_boot_by_scenario")
delta_eps2 <- get_tab("delta_eps2_boot")
delta_r <- get_tab("delta_r_boot")
lmm_anova <- get_tab("lmm_anova_baseline")
moran_tab <- get_tab("moran_baseline")
beam_info <- get_tab("beam_strength_info")

# ----------------------------- 4) STANDARDIZE OUTPUTS -------------------------
if (nrow(correlations) > 0) {
  # Accept both long format (var_row, var_col, rho) and matrix-like format.
  if (!all(c("var_row", "var_col", "rho", "tag") %in% names(correlations))) {
    metric_cols <- intersect(names(correlations), names(metric_code))
    if (length(metric_cols) > 0 && "var_row" %in% names(correlations)) {
      correlations <- correlations |>
        tidyr::pivot_longer(cols = dplyr::all_of(metric_cols), names_to = "var_col", values_to = "rho")
    }
  }
  correlations <- correlations |>
    dplyr::mutate(
      metric_row = std_metric(var_row),
      metric_col = std_metric(var_col),
      metric_pair = paste(pmin(metric_row, metric_col), pmax(metric_row, metric_col), sep = "--"),
      abs_rho = abs(as.numeric(rho)),
      collinearity_flag = dplyr::case_when(
        abs_rho >= 0.80 ~ "high_collinearity_abs_rho_ge_0.80",
        abs_rho >= 0.70 ~ "potential_collinearity_abs_rho_ge_0.70",
        TRUE ~ "not_flagged"
      )
    )
}

if (nrow(kw_boot) > 0) {
  kw_boot$metric <- std_metric(first_existing_col(kw_boot, c("variavel", "variable")))
  kw_boot$metric_label <- std_metric_label(kw_boot$metric)
}
if (nrow(delta_eps2) > 0) {
  delta_eps2$metric <- std_metric(first_existing_col(delta_eps2, c("variavel", "variable")))
  delta_eps2$metric_label <- std_metric_label(delta_eps2$metric)
}
if (nrow(dunn_boot) > 0) {
  dunn_boot$metric <- std_metric(first_existing_col(dunn_boot, c("variavel", "variable")))
  dunn_boot$metric_label <- std_metric_label(dunn_boot$metric)
  dunn_boot$comparison <- first_existing_col(dunn_boot, c("comparacao_en", "comparison", "comparacao"))
  dunn_boot$adjustment <- first_existing_col(dunn_boot, c("ajuste", "adjustment"))
}
if (nrow(delta_r) > 0) {
  delta_r$metric <- std_metric(first_existing_col(delta_r, c("variavel", "variable")))
  delta_r$metric_label <- std_metric_label(delta_r$metric)
  delta_r$comparison <- first_existing_col(delta_r, c("comparacao_en", "comparison", "comparacao"))
  delta_r$adjustment <- first_existing_col(delta_r, c("ajuste", "adjustment"))
}

# ----------------------------- 5) SUPPLEMENTARY TABLES -----------------------
# Table S1a: baseline polygon-level Spearman correlations.
table_s1a <- correlations |>
  dplyr::filter(tag == "S1_baseline_polygon", metric_row != metric_col) |>
  dplyr::mutate(pair_id = paste(pmin(metric_row, metric_col), pmax(metric_row, metric_col), sep = "--")) |>
  dplyr::group_by(pair_id) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    metric_1 = sub("--.*", "", pair_id),
    metric_2 = sub(".*--", "", pair_id),
    spearman_rho = as.numeric(rho),
    abs_spearman_rho = abs_rho,
    collinearity_flag
  ) |>
  dplyr::arrange(metric_1, metric_2)

# Table S1b: scenario stability of polygon-level correlations.
table_s1b <- correlations |>
  dplyr::filter(grepl("_polygon$", tag), metric_row != metric_col) |>
  dplyr::mutate(pair_id = paste(pmin(metric_row, metric_col), pmax(metric_row, metric_col), sep = "--")) |>
  dplyr::group_by(tag, pair_id) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::group_by(pair_id) |>
  dplyr::summarise(
    baseline_rho = suppressWarnings(rho[tag == "S1_baseline_polygon"][1]),
    median_rho_across_scenarios = stats::median(as.numeric(rho), na.rm = TRUE),
    min_rho_across_scenarios = min(as.numeric(rho), na.rm = TRUE),
    max_rho_across_scenarios = max(as.numeric(rho), na.rm = TRUE),
    n_scenarios = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    metric_1 = sub("--.*", "", pair_id),
    metric_2 = sub(".*--", "", pair_id)
  ) |>
  dplyr::select(metric_1, metric_2, baseline_rho, median_rho_across_scenarios, min_rho_across_scenarios, max_rho_across_scenarios, n_scenarios)

# Table S2a: scenario/sample-size impact.
table_s2a <- scenario_log |>
  dplyr::select(dplyr::any_of(c("scenario", "label", "status", "status2", "n_shots_post_qc", "n_polygons_post_qc", "n_shots_post_buffer", "n_polygons_post_buffer", "buffer_method", "buffer_removed_pct", "reason"))) |>
  dplyr::arrange(scenario)

scenario_definitions <- tibble::tribble(
  ~scenario, ~definition,
  "S0_no_qc", "No per-shot QC screening; all footprints assigned to polygons by spatial join were retained before the 60 m internal buffer.",
  "S1_baseline", "Baseline QC retaining L2B and L4A observations with quality_flag = 1 and degrade_flag = 0.",
  "S_sens080", "Baseline QC plus sensitivity_2b >= 0.80 and sensitivity_4a >= 0.80.",
  "S_sens090", "Baseline QC plus sensitivity_2b >= 0.90 and sensitivity_4a >= 0.90.",
  "S_sens095", "Baseline QC plus sensitivity_2b >= 0.95 and sensitivity_4a >= 0.95.",
  "S_sens099", "Baseline QC plus sensitivity_2b >= 0.99 and sensitivity_4a >= 0.99.",
  "S_strong_beams", "Baseline QC plus restriction to strong/high-energy beams using data-driven per-beam sensitivity statistics.",
  "S_night_only", "Baseline QC plus nighttime acquisitions only, using solar_elevation <= 0 for both L2B and L4A.",
  "S_agbd_low_uncert", "Baseline QC plus retention of the lowest-uncertainty AGBD observations, retaining the best 90% when uncertainty fields are available."
)

table_s2b <- redundancy_map |>
  dplyr::full_join(scenario_definitions, by = "scenario") |>
  dplyr::select(dplyr::any_of(c("scenario", "label", "definition", "signature", "scenario_rep", "status2"))) |>
  dplyr::arrange(scenario)

# Table S3a: KW results by scenario with delta.
if (nrow(delta_eps2) > 0) {
  delta_eps2$median_global_p_tmp <- first_existing_col(delta_eps2, c("p_global_mediana", "p_global_median", "median_global_p"))
  delta_eps2$median_eps2_tmp <- first_existing_col(delta_eps2, c("eps2_mediana", "eps2_median", "median_epsilon_squared"))
  delta_eps2$baseline_eps2_tmp <- first_existing_col(delta_eps2, c("eps2_base", "baseline_epsilon_squared"))
  delta_eps2$delta_eps2_tmp <- first_existing_col(delta_eps2, c("delta_eps2", "delta_epsilon_squared"))
}

table_s3a <- delta_eps2 |>
  dplyr::transmute(
    scenario,
    metric,
    metric_label,
    median_global_p = median_global_p_tmp,
    median_epsilon_squared = median_eps2_tmp,
    baseline_epsilon_squared = baseline_eps2_tmp,
    delta_epsilon_squared = delta_eps2_tmp
  ) |>
  dplyr::arrange(scenario, factor(metric, levels = metric_order))

# Table S3b: scenario-level summary of delta epsilon squared.
table_s3b <- table_s3a |>
  dplyr::group_by(scenario) |>
  dplyr::summarise(
    median_delta_epsilon_squared = stats::median(delta_epsilon_squared, na.rm = TRUE),
    min_delta_epsilon_squared = min(delta_epsilon_squared, na.rm = TRUE),
    max_delta_epsilon_squared = max(delta_epsilon_squared, na.rm = TRUE),
    n_metrics = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(scenario)

# Table S4: pairwise robustness across scenarios (Bonferroni compact table).
if (nrow(dunn_boot) > 0) {
  dunn_boot$median_r_tmp <- first_existing_col(dunn_boot, c("r_mediana", "r_median", "median_r"))
}

table_s4 <- dunn_boot |>
  dplyr::filter(adjustment == "Bonferroni") |>
  dplyr::transmute(
    scenario,
    metric,
    metric_label,
    comparison,
    adjustment,
    median_r = median_r_tmp,
    proportion_significant = prop_sig
  ) |>
  dplyr::arrange(metric, comparison, scenario)

supplementary_tables <- list(
  S1a_polygon_collinearity_baseline = table_s1a,
  S1b_collinearity_stability = table_s1b,
  S2a_qc_scenario_sample_size = table_s2a,
  S2b_scenario_definitions_redundancy = table_s2b,
  S3a_kw_by_scenario = table_s3a,
  S3b_delta_eps2_summary = table_s3b,
  S4_pairwise_robustness = table_s4
)

# ----------------------------- 6) EXPORT COMPACT TABLES ----------------------
for (nm in names(supplementary_tables)) {
  write_csv_safe(supplementary_tables[[nm]], file.path(opt$table_dir, paste0(tolower(nm), ".csv")))
}

manifest <- tibble::tibble(
  table_id = names(supplementary_tables),
  file = paste0(tolower(table_id), ".csv"),
  n_rows = vapply(supplementary_tables, nrow, integer(1)),
  n_columns = vapply(supplementary_tables, ncol, integer(1)),
  cited_in_submission = c("Table S1a", "Table S1b", "Table S2a", "Table S2b", "Table S3a", "Table S3b", "Table S4")
)
write_csv_safe(manifest, file.path(opt$supp_dir, "supplementary_table_manifest.csv"))

wb <- openxlsx::createWorkbook()
used <- character(0)
add_sheet <- function(nm) {
  nm2 <- safe_sheet_name(nm, used)
  openxlsx::addWorksheet(wb, nm2)
  used <<- c(used, nm2)
  nm2
}
sh <- add_sheet("manifest")
openxlsx::writeData(wb, sh, manifest)
for (nm in names(supplementary_tables)) {
  tab <- supplementary_tables[[nm]]
  sh <- add_sheet(nm)
  openxlsx::writeData(wb, sh, tab)
  openxlsx::freezePane(wb, sh, firstRow = TRUE)
  if (ncol(tab) > 0) openxlsx::setColWidths(wb, sh, cols = 1:ncol(tab), widths = "auto")
}
openxlsx::saveWorkbook(wb, opt$compact_workbook, overwrite = TRUE)

# ----------------------------- 7) EXPORT FULL SUPPLEMENTARY DATA -------------
readme <- tibble::tribble(
  ~Field, ~Value, ~Where_cited_or_used,
  "Supplementary Data file", "Supplementary_Data_Bandoria_et_al_2026.xlsx", "Supplementary Information: Supplementary Data 1",
  "Associated manuscript", "Regional comparison of Atlantic Forest physiognomies using GEDI-derived structural metrics", "Submitted manuscript and Supplementary Information",
  "Purpose", "Complete scenario-wise outputs for GEDI QC/acquisition sensitivity analyses", "Supplementary Methods S3-S4; Fig. S1-S2; Tables S1a-S4",
  "Primary unit", "Polygon-level GEDI summaries", "Main manuscript statistical analysis",
  "Generated by", "scripts/05_make_supplementary_tables.R after scripts/04_run_qc_sensitivity_analysis.R", "Repository workflow"
)

full_sheets <- list(
  README = readme,
  Meta_join_log = join_log,
  Meta_preflight = preflight,
  S1_correlations_long = correlations,
  S2a_scenario_log = scenario_log,
  S2b_redundancy_map = redundancy_map,
  S3a_kw_by_scenario = kw_boot,
  S3b_delta_eps2 = delta_eps2,
  S4a_dunn_by_scenario = dunn_boot,
  S4b_delta_r = delta_r,
  S5_lmm_anova_baseline = lmm_anova,
  S6_moran_baseline = moran_tab,
  S7_beam_strength_info = beam_info
)

wb2 <- openxlsx::createWorkbook()
used <- character(0)
add_sheet2 <- function(nm) {
  nm2 <- safe_sheet_name(nm, used)
  openxlsx::addWorksheet(wb2, nm2)
  used <<- c(used, nm2)
  nm2
}
for (nm in names(full_sheets)) {
  tab <- full_sheets[[nm]]
  if (is.null(tab) || !is.data.frame(tab) || nrow(tab) == 0) next
  sh <- add_sheet2(nm)
  openxlsx::writeData(wb2, sh, tab)
  openxlsx::freezePane(wb2, sh, firstRow = TRUE)
  if (ncol(tab) > 0) openxlsx::setColWidths(wb2, sh, cols = 1:ncol(tab), widths = "auto")
}
openxlsx::saveWorkbook(wb2, opt$full_workbook, overwrite = TRUE)

capture.output(sessionInfo(), file = file.path(opt$supp_dir, "session_info_05_supplementary_tables.txt"))

msg("Supplementary compact tables exported: ", opt$compact_workbook)
msg("Full Supplementary Data workbook exported: ", opt$full_workbook)
msg("Done: 05_make_supplementary_tables.R")
