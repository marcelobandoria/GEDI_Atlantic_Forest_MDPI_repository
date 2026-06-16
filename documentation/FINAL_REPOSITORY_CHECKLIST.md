# Final repository checklist

Use this checklist before publishing the repository publicly.

## 1. Root-level files

Required root-level files:

- [ ] `README.md`
- [ ] `README_repository_workflow.md`
- [ ] `CITATION.cff`
- [ ] `LICENSE.md`
- [ ] `.gitignore`
- [ ] `regional-atlantic-forest-gedi-structural-metrics.Rproj`

Recommended optional files:

- [ ] `CHANGELOG.md`
- [ ] `documentation/REPRODUCIBILITY_GUIDE.md`
- [ ] `documentation/DATA_AND_OUTPUT_COVERAGE.md`
- [ ] `documentation/STATIC_ASSETS_GUIDE.md`

## 2. Input data folders

### `data/`

Required:

- [ ] `data/gedi_footprints_filtered.csv`

This is the final GEDI analytical table used by the manuscript-reference workflow.

Minimum expected columns:

- [ ] `id_amostra`
- [ ] `fitofisionomia`
- [ ] `latitude_bin0`
- [ ] `longitude_bin0`
- [ ] `elev_highestreturn`
- [ ] `elev_lowestmode_2b`
- [ ] `elev_lowestmode_4a`
- [ ] `cover`
- [ ] `pai`
- [ ] `fhd_normal`
- [ ] `agbd`
- [ ] `l2b_quality_flag`
- [ ] `degrade_flag_2b`
- [ ] `l4_quality_flag`
- [ ] `degrade_flag_4a`
- [ ] `sensitivity_2b`
- [ ] `sensitivity_4a`
- [ ] `beam_2b`
- [ ] `beam_4a`
- [ ] `solar_elevation_2b`
- [ ] `solar_elevation_4a`

### `spatial/`

Required:

- [ ] `spatial/old_growth_candidate_polygons.gpkg`

Expected layer and fields:

- [ ] Layer: `atual__samples`
- [ ] Polygon ID field: `id`
- [ ] Physiognomy field: `fitofisionomia`
- [ ] Valid polygon geometry
- [ ] Valid coordinate reference system

## 3. Scripts

Required files under `scripts/`:

- [ ] `00_run_repository_workflow.R`
- [ ] `01_prepare_manuscript_dataset.R`
- [ ] `02_run_main_statistics.R`
- [ ] `03_make_main_figures.R`
- [ ] `04_run_qc_sensitivity_analysis.R`
- [ ] `05_make_supplementary_tables.R`
- [ ] `06_make_supplementary_figures.R`
- [ ] `07_check_submission_coverage.R`

Script-level checks:

- [ ] All scripts use repository-relative paths.
- [ ] All outputs are in English scientific terminology.
- [ ] Fast-test mode is available through `GEDI_FAST_TEST=true`.
- [ ] Final mode uses the full bootstrap settings.
- [ ] The main workflow reproduces the manuscript-reference results.
- [ ] The supplementary workflow is clearly treated as robustness/sensitivity analysis.

## 4. Results folders

The following folders may be empty before execution but should exist:

- [ ] `results/main/`
- [ ] `results/main/tables/`
- [ ] `results/figures/`
- [ ] `results/supplementary/`
- [ ] `results/supplementary/tables/`
- [ ] `results/supplementary/figures/`
- [ ] `results/checks/`

## 5. Main manuscript outputs

After the final run, check:

- [ ] `results/main/manuscript_results.xlsx`
- [ ] `results/main/tables/group_summary.csv`
- [ ] `results/main/tables/descriptive_statistics.csv`
- [ ] `results/main/tables/kw_shot_exploratory.csv`
- [ ] `results/main/tables/kw_polygon_bootstrap.csv`
- [ ] `results/main/tables/dunn_polygon_bootstrap.csv`
- [ ] `results/main/tables/lmm_anova.csv`
- [ ] `results/main/tables/moran_i.csv`
- [ ] `results/figures/figure_07_boxplots_by_physiognomy.png`

Expected manuscript-reference diagnostic values:

- [ ] 252,152 GEDI footprints before the 60 m edge filter.
- [ ] 13,191 footprints removed by the 60 m edge filter.
- [ ] 238,961 footprints retained after the 60 m edge filter.
- [ ] Balanced shot count equals 12,440 per physiognomy in the manuscript-reference workflow.

## 6. Supplementary outputs

After the final run, check:

- [ ] Complete scenario-wise outputs workbook: results/supplementary/Supplementary_Data_Bandoria_et_al_2026.xlsx
- [ ] `results/supplementary/supplementary_tables.xlsx`
- [ ] `results/supplementary/tables/s1a_polygon_collinearity_baseline.csv`
- [ ] `results/supplementary/tables/s1b_collinearity_stability.csv`
- [ ] `results/supplementary/tables/s2a_qc_scenario_sample_size.csv`
- [ ] `results/supplementary/tables/s2b_scenario_definitions_redundancy.csv`
- [ ] `results/supplementary/tables/s3a_kw_by_scenario.csv`
- [ ] `results/supplementary/tables/s3b_delta_eps2_summary.csv`
- [ ] `results/supplementary/tables/s4_pairwise_robustness.csv`
- [ ] `results/supplementary/figures/figure_s1_polygon_spearman_heatmap.png`
- [ ] `results/supplementary/figures/figure_s2_qc_delta_epsilon2.png`

## 7. Static manuscript assets

Figures 1–6 are external/static assets and should be archived under:

```text
manuscript_assets/figures/
```

Required static figure files:

- [ ] `figure_01_atlantic_forest_extent_old_growth_candidates.png`
- [ ] `figure_02_study_area_phytophysiognomies.png`
- [ ] `figure_03_sampling_distribution_old_growth_forests.png`
- [ ] `figure_04_visual_interpretation_key.png`
- [ ] `figure_05_gedi_sampling_within_physiognomies.png`
- [ ] `figure_06_research_flowchart.png`

## 8. Metadata

Required metadata files:

- [ ] `metadata/data_dictionary.csv`
- [ ] `metadata/input_manifest.csv`
- [ ] `metadata/output_manifest.csv`
- [ ] `metadata/software_dependencies.csv`
- [ ] `metadata/static_assets_manifest.csv`
- [ ] `metadata/table_figure_crosswalk.csv`
- [ ] `metadata/processing_summary.csv`

## 9. Final coverage check

Run:

```r
source("scripts/07_check_submission_coverage.R")
```

Then inspect:

- [ ] `results/checks/submission_output_coverage.csv`
- [ ] `results/checks/submission_output_coverage_summary.csv`

All script-generated items should be present. Static figures should be present under `manuscript_assets/figures/`.

## 10. Final publication checks

- [ ] Remove temporary files: `.Rhistory`, `.RData`, `.Rproj.user/`, `Thumbs.db`, `.DS_Store`.
- [ ] Confirm that no private credentials, tokens, or local absolute paths remain in scripts.
- [ ] Confirm that all tables and figures are in English scientific terminology.
- [ ] Confirm that third-party data sources are cited in README or documentation.
- [ ] Confirm that the repository license is compatible with public release.
- [ ] Create a final tagged release, for example `v1.0.3`.
- [ ] Archive the release in Zenodo or institutional repository if needed.
