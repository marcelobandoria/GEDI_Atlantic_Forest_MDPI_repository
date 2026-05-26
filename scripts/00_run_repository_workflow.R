# -----------------------------------------------------------------------------
# Script 00 — Run the complete repository workflow
#
# Project: GEDI-derived forest structure in Atlantic Forest physiognomies
# Repository workflow: manuscript and Supplementary Information reproducibility
# Author: Marcelo C. S. Bandoria
#
# Purpose
# This script runs the full analysis workflow in the same order expected by the
# repository. It generates the main manuscript tables and figures, the
# supplementary QC/acquisition sensitivity outputs, the Supplementary Data Excel
# workbook, and the final coverage report.
#
# Quick test
#   Sys.setenv(GEDI_FAST_TEST = "true")
# Final run
#   Sys.unsetenv("GEDI_FAST_TEST")
# -----------------------------------------------------------------------------

find_repo_root <- function() {
  candidates <- unique(normalizePath(c(getwd(), dirname(getwd()), file.path(getwd(), "..")), mustWork = FALSE))
  for (cand in candidates) {
    if (dir.exists(file.path(cand, "scripts")) && dir.exists(file.path(cand, "data"))) return(cand)
  }
  normalizePath(getwd(), mustWork = FALSE)
}

repo_root <- find_repo_root()
repo_script <- function(name) file.path(repo_root, "scripts", name)

workflow <- c(
  "01_prepare_manuscript_dataset.R",
  "02_run_main_statistics.R",
  "03_make_main_figures.R",
  "04_run_qc_sensitivity_analysis.R",
  "05_make_supplementary_tables.R",
  "06_make_supplementary_figures.R",
  "07_check_submission_coverage.R"
)

for (script in workflow) {
  path <- repo_script(script)
  if (!file.exists(path)) stop("Missing workflow script: ", path)
  message("\n--- Running ", script, " ---")
  source(path, local = FALSE)
}

message("\nRepository workflow completed.")
