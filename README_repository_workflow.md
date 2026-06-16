# Repository workflow

[![DOI](to be updated after publication of the new Zenodo version.svg)](to be updated after publication of the new Zenodo version)

This repository workflow reproduces the data-dependent outputs cited in the submitted manuscript and Supplementary Information for the study:

**Regional comparison of Atlantic Forest physiognomies using GEDI-derived structural metrics**

## Archived version

- **GitHub release:** v1.0.3
- **Zenodo DOI:** to be updated after publication of the new Zenodo version
- **GitHub repository:** https://github.com/marcelobandoria/regional-atlantic-forest-gedi-structural-metrics

## Required input files

Place the input files in the repository root using this structure:

```text
data/gedi_footprints_filtered.csv
spatial/old_growth_candidate_polygons.gpkg
scripts/*.R
```

The polygon GeoPackage is expected to contain the layer `atual__samples`.

## Scripts

```text
00_run_repository_workflow.R
01_prepare_manuscript_dataset.R
02_run_main_statistics.R
03_make_main_figures.R
04_run_qc_sensitivity_analysis.R
05_make_supplementary_tables.R
06_make_supplementary_figures.R
07_check_submission_coverage.R
```

## Script roles

| Script | Role |
|---|---|
| `00_run_repository_workflow.R` | Runs the complete workflow in order. |
| `01_prepare_manuscript_dataset.R` | Prepares the analytical dataset used for manuscript reproducibility. |
| `02_run_main_statistics.R` | Generates the main statistical tables. |
| `03_make_main_figures.R` | Generates the main boxplot figures, including Figure 7. |
| `04_run_qc_sensitivity_analysis.R` | Runs supplementary QC/acquisition sensitivity analyses. |
| `05_make_supplementary_tables.R` | Generates Supplementary Tables S1a–S4 and the Supplementary Data workbook. |
| `06_make_supplementary_figures.R` | Generates Supplementary Figures S1 and S2. |
| `07_check_submission_coverage.R` | Checks whether all submitted data-dependent outputs are present. |

## Quick test

```r
Sys.setenv("GEDI_FAST_TEST", "true")
source("scripts/00_run_repository_workflow.R")
```

This mode reduces bootstrap iterations and figure DPI. It is only for checking that the workflow runs.

## Final run

```r
Sys.unsetenv("GEDI_FAST_TEST")
source("scripts/00_run_repository_workflow.R")
```

The final run uses the full bootstrap settings used for manuscript reproducibility.

## Main outputs

```text
results/main/manuscript_results.xlsx
results/main/tables/
results/figures/figure_07_boxplots_by_physiognomy.png
```

## Supplementary outputs

```text
Complete scenario-wise outputs workbook: results/supplementary/Supplementary_Data_Bandoria_et_al_2026.xlsx
results/supplementary/supplementary_tables.xlsx
results/supplementary/tables/
results/supplementary/figures/figure_s1_polygon_spearman_heatmap.png
results/supplementary/figures/figure_s2_qc_delta_epsilon2.png
```

## Coverage check

After running the workflow, inspect:

```text
results/checks/submission_output_coverage.csv
results/checks/submission_output_coverage_summary.csv
```

These files indicate whether each table and figure cited in the submitted documentation is produced by the workflow or is an external/static asset.
