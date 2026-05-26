# -----------------------------------------------------------------------------
# Script 07 — Check coverage of submitted manuscript and supplementary outputs
#
# Project: GEDI-derived forest structure in Atlantic Forest physiognomies
# Repository workflow: final reproducibility check
# Author: Marcelo C. S. Bandoria
#
# Purpose
# This script checks whether each data-dependent table and figure cited in the
# submitted manuscript and Supplementary Information has a corresponding output
# in the repository workflow. Static cartographic or interpretation-key figures
# are flagged as external assets because they are not produced by the statistical
# R workflow.
# -----------------------------------------------------------------------------

find_repo_root <- function() {
  candidates <- unique(normalizePath(c(getwd(), dirname(getwd()), file.path(getwd(), "..")), mustWork = FALSE))
  for (cand in candidates) {
    if (dir.exists(file.path(cand, "scripts")) && dir.exists(file.path(cand, "data"))) return(cand)
  }
  normalizePath(getwd(), mustWork = FALSE)
}

repo_root <- find_repo_root()
repo_path <- function(...) file.path(repo_root, ...)

need <- c("dplyr", "readr", "tibble")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(need, require, character.only = TRUE))

mk_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
mk_dir(repo_path("results", "checks"))

expected <- tibble::tribble(
  ~document, ~item, ~description, ~generated_by, ~expected_file, ~coverage_type,
  "Main manuscript", "Table 1", "Spatial sampling summary and 60-m edge-filter impact by phytophysiognomy", "01, 02", "results/main/tables/group_summary.csv", "script_generated",
  "Main manuscript", "Table 2", "Descriptive statistics by phytophysiognomy", "02", "results/main/tables/descriptive_statistics.csv", "script_generated",
  "Main manuscript", "Table 3", "Global and polygon-level Kruskal-Wallis results", "02", "results/main/tables/kw_polygon_bootstrap.csv", "script_generated",
  "Main manuscript", "Table 4", "Post hoc Dunn pairwise comparisons among phytophysiognomies", "02", "results/main/tables/dunn_polygon_bootstrap.csv", "script_generated",
  "Main manuscript", "Table 5", "ANOVA results from linear mixed models", "02", "results/main/tables/lmm_anova.csv", "script_generated",
  "Main manuscript", "Table 6", "Residual spatial autocorrelation using Moran's I", "02", "results/main/tables/moran_i.csv", "script_generated",
  "Main manuscript", "Figure 1", "Atlantic Forest extent and old-growth candidate forests", "external GIS/cartography", "manuscript_assets/figures/figure_01_atlantic_forest_extent.png", "external_asset",
  "Main manuscript", "Figure 2", "Study area and distribution of phytophysiognomies", "external GIS/cartography", "manuscript_assets/figures/figure_02_study_area_phytophysiognomies.png", "external_asset",
  "Main manuscript", "Figure 3", "Distribution of sampling across old-growth forests", "external GIS/cartography", "manuscript_assets/figures/figure_03_sampling_distribution.png", "external_asset",
  "Main manuscript", "Figure 4", "Visual interpretation keys for phytophysiognomies", "external imagery/QA-QC", "manuscript_assets/figures/figure_04_visual_interpretation_key.png", "external_asset",
  "Main manuscript", "Figure 5", "GEDI sampling within old-growth physiognomies", "external GIS/cartography", "manuscript_assets/figures/figure_05_gedi_sampling.png", "external_asset",
  "Main manuscript", "Figure 6", "Research flowchart", "external diagram", "manuscript_assets/figures/figure_06_research_flowchart.png", "external_asset",
  "Main manuscript", "Figure 7", "Boxplots of GEDI structural variables by phytophysiognomy", "03", "results/figures/figure_07_boxplots_by_physiognomy.png", "script_generated",
  "Supplementary Information", "Fig. S1", "Polygon-level Spearman collinearity heatmap", "06", "results/supplementary/figures/figure_s1_polygon_spearman_heatmap.png", "script_generated",
  "Supplementary Information", "Fig. S2", "QC/acquisition sensitivity of polygon-level Kruskal-Wallis effect sizes", "06", "results/supplementary/figures/figure_s2_qc_delta_epsilon2.png", "script_generated",
  "Supplementary Information", "Table S1a", "Polygon-level collinearity under baseline QC", "05", "results/supplementary/tables/s1a_polygon_collinearity_baseline.csv", "script_generated",
  "Supplementary Information", "Table S1b", "Stability of polygon-level collinearity across scenarios", "05", "results/supplementary/tables/s1b_collinearity_stability.csv", "script_generated",
  "Supplementary Information", "Table S2a", "QC/acquisition scenarios and sample-size impact", "05", "results/supplementary/tables/s2a_qc_scenario_sample_size.csv", "script_generated",
  "Supplementary Information", "Table S2b", "Scenario definitions and redundancy handling", "05", "results/supplementary/tables/s2b_scenario_definitions_redundancy.csv", "script_generated",
  "Supplementary Information", "Table S3a", "Polygon-level Kruskal-Wallis results by scenario", "05", "results/supplementary/tables/s3a_kw_by_scenario.csv", "script_generated",
  "Supplementary Information", "Table S3b", "Scenario-level summary of delta epsilon squared", "05", "results/supplementary/tables/s3b_delta_eps2_summary.csv", "script_generated",
  "Supplementary Information", "Table S4", "Pairwise robustness of physiognomy separation across scenarios", "05", "results/supplementary/tables/s4_pairwise_robustness.csv", "script_generated",
  "Supplementary Data", "Supplementary Data 1", "Complete scenario-wise QC/acquisition outputs", "05", "results/supplementary/Supplementary_Data_Bandoria_et_al_2026.xlsx", "script_generated"
)

coverage <- expected |>
  dplyr::mutate(
    absolute_path = repo_path(expected_file),
    exists = file.exists(absolute_path),
    status = dplyr::case_when(
      coverage_type == "script_generated" & exists ~ "OK",
      coverage_type == "script_generated" & !exists ~ "MISSING_SCRIPT_OUTPUT",
      coverage_type == "external_asset" & exists ~ "OK_EXTERNAL_ASSET_PRESENT",
      coverage_type == "external_asset" & !exists ~ "EXTERNAL_ASSET_NOT_CHECKED_IN",
      TRUE ~ "CHECK"
    )
  )

readr::write_csv(coverage, repo_path("results", "checks", "submission_output_coverage.csv"), na = "")

summary <- coverage |>
  dplyr::count(coverage_type, status, name = "n_items")
readr::write_csv(summary, repo_path("results", "checks", "submission_output_coverage_summary.csv"), na = "")

capture.output(sessionInfo(), file = repo_path("results", "checks", "session_info_07_coverage.txt"))

print(summary, n = Inf)
message("Coverage report written to results/checks/submission_output_coverage.csv")
