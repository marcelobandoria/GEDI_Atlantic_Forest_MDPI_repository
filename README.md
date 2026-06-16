# Data and code for: Regional comparison of Atlantic Forest physiognomies using GEDI-derived structural metrics

This repository contains the derived datasets, spatial sampling files, R scripts, metadata, documentation, complete scenario-wise outputs, supplementary outputs, corrected article tables, and reproducible outputs associated with the manuscript:

**Regional comparison of Atlantic Forest physiognomies using GEDI-derived structural metrics**

The repository supports transparency, reproducibility, and data availability for the GEDI-based regional comparison of Atlantic Forest phytophysiognomies using polygon-level structural metrics.

## Repository version associated with the revised manuscript

The repository version associated with the revised manuscript is:

* Repository version: **v1.0.3**
* Zenodo DOI: **to be updated after publication of the new Zenodo version**
* GitHub repository: `https://github.com/marcelobandoria/regional-atlantic-forest-gedi-structural-metrics`

## Study overview

The study compares GEDI-derived forest structural metrics among old-growth candidate polygons in four Atlantic Forest phytophysiognomies:

* Dense Ombrophilous Forest (DOF)
* Mixed Ombrophilous Forest (MOF)
* Seasonal Semideciduous Forest (SSdF)
* Seasonal Deciduous Forest (SDF)

The workflow analyzes five GEDI-derived structural variables:

* Canopy height (H, m)
* Aboveground biomass density (AGBD, Mg ha^-1)
* Plant area index (PAI, m^2 m^-2)
* Canopy cover (COVER, %)
* Foliage height diversity (FHD)

## Repository contents

This repository includes:

* derived GEDI-based structural datasets;
* spatial sampling files;
* R scripts used for preprocessing, statistical analyses, figures, and supplementary outputs;
* metadata and documentation;
* complete scenario-wise outputs;
* supplementary outputs supporting the manuscript;
* corrected article tables corresponding to the revised manuscript.

## Repository structure

```text
regional-atlantic-forest-gedi-structural-metrics/
├── data/
│   └── gedi_footprints_filtered.csv
├── spatial/
│   └── old_growth_candidate_polygons.gpkg
├── scripts/
│   ├── 00_run_repository_workflow.R
│   ├── 01_prepare_manuscript_dataset.R
│   ├── 02_run_main_statistics.R
│   ├── 03_make_main_figures.R
│   ├── 04_run_qc_sensitivity_analysis.R
│   ├── 05_make_supplementary_tables.R
│   ├── 06_make_supplementary_figures.R
│   └── 07_check_submission_coverage.R
├── results/
│   ├── main/
│   ├── figures/
│   ├── supplementary/
│   └── checks/
├── metadata/
├── documentation/
│   └── article_tables/
├── manuscript_assets/
│   └── figures/
├── README.md
├── README_repository_workflow.md
├── CITATION.cff
├── LICENSE.md
└── regional-atlantic-forest-gedi-structural-metrics.Rproj
```

## Required input files

The workflow expects the following input files:

```text
data/gedi_footprints_filtered.csv
spatial/old_growth_candidate_polygons.gpkg
```

The polygon GeoPackage must contain the layer:

```text
atual__samples
```

## How to reproduce the results

Open the R project file or set the working directory to the repository root. Then run:

```r
Sys.unsetenv("GEDI_FAST_TEST")
source("scripts/00_run_repository_workflow.R")
```

For a fast structural test, use:

```r
Sys.setenv("GEDI_FAST_TEST", "true")
source("scripts/00_run_repository_workflow.R")
```

The fast mode reduces bootstrap iterations and figure resolution. It should not be used to reproduce the final manuscript values.

## Main outputs

The workflow generates the main manuscript outputs under:

```text
results/main/
results/figures/
```

Key outputs include:

```text
results/main/manuscript_results.xlsx
results/main/tables/group_summary.csv
results/main/tables/descriptive_statistics.csv
results/main/tables/kw_shot_exploratory.csv
results/main/tables/kw_polygon_bootstrap.csv
results/main/tables/dunn_polygon_bootstrap.csv
results/main/tables/lmm_anova.csv
results/main/tables/moran_i.csv
results/figures/figure_07_boxplots_by_physiognomy.png
```

## Corrected article tables

The corrected article tables submitted with the revised manuscript are available under:

```text
documentation/article_tables/
```

These files correspond to Tables 1–6 in the revised manuscript and were checked for consistency with the manuscript text, Supplementary Information, and repository outputs.

## Supplementary outputs

Supplementary outputs are generated under:

```text
results/supplementary/
```

Key outputs include:

```text
results/supplementary/Supplementary_Data_Bandoria_et_al_2026.xlsx
results/supplementary/supplementary_tables.xlsx
results/supplementary/tables/
results/supplementary/figures/figure_s1_polygon_spearman_heatmap.png
results/supplementary/figures/figure_s2_qc_delta_epsilon2.png
```

The supplementary file submitted with the manuscript contains only the compact supporting material: Supplementary Methods S1–S4, Supplementary Figures S1–S2, and Supplementary Tables S1a–S1b, S2a–S2b, S3a–S3b, and S4.

Complete processing logs, full correlation tables, scenario-wise Dunn outputs, and effect-size deltas are retained in the repository for transparency and reproducibility.

## Static manuscript assets

Figures 1–6 in the manuscript are cartographic, visual interpretation, or diagrammatic assets. They are stored as static files under:

```text
manuscript_assets/figures/
```

The repository workflow checks their expected presence but does not recreate them.

## Coverage check

After running the workflow, inspect:

```text
results/checks/submission_output_coverage.csv
results/checks/submission_output_coverage_summary.csv
```

These files indicate whether each data-dependent table and figure cited in the submitted manuscript and Supplementary Information is generated by the workflow or is an external/static asset.

## Notes on reproducibility

The repository was organized to document the workflow used in the manuscript. Scripts and outputs are provided to support reproducibility of the analyses and to allow verification of the scenario-wise results reported in the manuscript and supplementary material.

## Citation

If you use this repository, please cite:

Bandoria, M. C. S.; Seixas, H. T.; Rosa, M. R.; Molin, P. G.; Queiroz, A. P. (2026). *Data and code for: Regional comparison of Atlantic Forest physiognomies using GEDI-derived structural metrics* (v1.0.3). Zenodo. DOI to be updated after publication of the new Zenodo version.

## License

This repository is distributed under the terms described in `LICENSE.md`.
