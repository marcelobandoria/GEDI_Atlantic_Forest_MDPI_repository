# Reproducibility guide

This guide explains how to reproduce the data-dependent outputs of the manuscript and Supplementary Information.

## 1. Open the R project

Open:

```text
GEDI_Atlantic_Forest_MDPI_repository.Rproj
```

The working directory should be the repository root.

## 2. Install required R packages

The scripts automatically check and install missing packages when necessary. The main packages are listed in `metadata/software_dependencies.csv`.

Core package groups:

- Data input and cleaning: `readr`, `janitor`, `dplyr`, `tidyr`, `stringr`, `purrr`, `tibble`
- Spatial processing: `sf`, `units`, optional `lwgeom`
- Statistical analysis: `FSA`, `lme4`, `lmerTest`, `spdep`, optional `gstat`
- Tables and outputs: `openxlsx`
- Figures: `ggplot2`, `scales`, `cowplot`, `patchwork`, `sysfonts`, `showtext`

## 3. Run the quick test

```r
Sys.setenv(GEDI_FAST_TEST = "true")
source("scripts/00_run_repository_workflow.R")
```

This checks whether all scripts run. Do not use quick-test results for manuscript verification because bootstrap iterations are reduced.

## 4. Run the final workflow

```r
Sys.unsetenv("GEDI_FAST_TEST")
source("scripts/00_run_repository_workflow.R")
```

This produces the final manuscript and supplementary outputs.

## 5. Validate coverage

```r
source("scripts/07_check_submission_coverage.R")
```

Then inspect:

```text
results/checks/submission_output_coverage.csv
results/checks/submission_output_coverage_summary.csv
```

## 6. Verify key manuscript-reference diagnostics

The main workflow should reproduce the following reference counts:

- 252,152 GEDI footprints before the 60 m edge filter.
- 13,191 footprints removed by the 60 m edge filter.
- 238,961 footprints retained after the 60 m edge filter.
- 12,440 balanced shots per physiognomy in the manuscript-reference workflow.

## 7. Interpret workflow distinction

The main manuscript workflow reproduces the analytical path used for the submitted manuscript tables and Figure 7.

The QC/acquisition workflow is a supplementary robustness workflow. It should be used to reproduce Supplementary Methods S3-S4, Supplementary Figures S1-S2, Supplementary Tables S1a-S4, and the Supplementary Data workbook. It should not replace the manuscript-reference main analysis.
