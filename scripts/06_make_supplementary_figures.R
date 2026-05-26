# -----------------------------------------------------------------------------
# Script 06 — Generate supplementary figures cited in the submission
#
# Project: GEDI-derived forest structure in Atlantic Forest physiognomies
# Repository workflow: manuscript and Supplementary Information reproducibility
# Author: Marcelo C. S. Bandoria
#
# Purpose
# This script generates the two supplementary figures cited in the submitted
# Supplementary Information: Figure S1 (polygon-level Spearman collinearity) and
# Figure S2 (QC/acquisition sensitivity of polygon-level Kruskal-Wallis effect
# sizes, expressed as delta epsilon squared relative to baseline).
#
# Main inputs
#   results/supplementary/correlations_qc_scenarios.csv
#   results/supplementary/delta_eps2_boot.csv
#
# Main outputs
#   results/supplementary/figures/figure_s1_polygon_spearman_heatmap.png
#   results/supplementary/figures/figure_s2_qc_delta_epsilon2.png
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
fast_test <- tolower(Sys.getenv("GEDI_FAST_TEST", "false")) %in% c("true", "1", "yes", "sim")

opt <- list(
  dir_out = repo_path("results", "supplementary"),
  dir_fig = repo_path("results", "supplementary", "figures"),
  correlations_file = repo_path("results", "supplementary", "correlations_qc_scenarios.csv"),
  delta_eps2_file = repo_path("results", "supplementary", "delta_eps2_boot.csv"),
  dpi = if (fast_test) 150 else 500,
  font_family = "Times New Roman"
)

# ----------------------------- 1) PACKAGES -----------------------------------
need <- c("dplyr", "tidyr", "ggplot2", "readr", "tibble", "grid", "sysfonts", "showtext", "stringr")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(need, require, character.only = TRUE))
options(dplyr.summarise.inform = FALSE)

# ----------------------------- 2) HELPERS ------------------------------------
mk_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
mk_dir(opt$dir_out); mk_dir(opt$dir_fig)
msg <- function(...) cat(paste0(..., "\n"))

try({
  sysfonts::font_add(family = opt$font_family, regular = opt$font_family)
  showtext::showtext_auto()
}, silent = TRUE)

theme_repo <- function() {
  ggplot2::theme_minimal(base_family = opt$font_family, base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      panel.grid.minor = ggplot2::element_blank()
    )
}

save_plot <- function(p, filename, width = 7, height = 5) {
  path <- file.path(opt$dir_fig, filename)
  ggplot2::ggsave(path, p, width = width, height = height, dpi = opt$dpi)
  path
}

metric_code <- c(
  altura_dossel = "H", canopy_height = "H", H = "H",
  agbd = "AGBD", AGBD = "AGBD",
  pai = "PAI", PAI = "PAI",
  cover = "COVER", COVER = "COVER",
  fhd_normal = "FHD", FHD = "FHD"
)
metric_order <- c("H", "AGBD", "PAI", "COVER", "FHD")
std_metric <- function(x) dplyr::recode(as.character(x), !!!metric_code, .default = as.character(x))

scenario_labels <- c(
  S0_no_qc = "No QC",
  S1_baseline = "Baseline",
  S_sens080 = "Sensitivity >= 0.80",
  S_sens090 = "Sensitivity >= 0.90",
  S_sens095 = "Sensitivity >= 0.95",
  S_sens099 = "Sensitivity >= 0.99",
  S_strong_beams = "Strong beams",
  S_night_only = "Night only",
  S_agbd_low_uncert = "Low AGBD uncertainty"
)
std_scenario <- function(x) dplyr::recode(as.character(x), !!!scenario_labels, .default = as.character(x))

figure_registry <- tibble::tibble(figure_id = character(), file = character(), caption = character())
register <- function(id, file, caption) {
  figure_registry <<- dplyr::bind_rows(figure_registry, tibble::tibble(figure_id = id, file = basename(file), caption = caption))
}

# ----------------------------- 3) FIGURE S1 ----------------------------------
if (file.exists(opt$correlations_file)) {
  corr <- readr::read_csv(opt$correlations_file, show_col_types = FALSE)
  if (!all(c("var_row", "var_col", "rho", "tag") %in% names(corr))) {
    metric_cols <- intersect(names(corr), names(metric_code))
    if (length(metric_cols) > 0 && "var_row" %in% names(corr)) {
      corr <- corr |>
        tidyr::pivot_longer(cols = dplyr::all_of(metric_cols), names_to = "var_col", values_to = "rho")
    }
  }

  if (all(c("var_row", "var_col", "rho", "tag") %in% names(corr))) {
    corr_baseline <- corr |>
      dplyr::filter(tag == "S1_baseline_polygon") |>
      dplyr::mutate(
        metric_x = factor(std_metric(var_col), levels = metric_order),
        metric_y = factor(std_metric(var_row), levels = rev(metric_order)),
        rho = as.numeric(rho),
        label = sprintf("%.2f", rho)
      ) |>
      dplyr::filter(!is.na(metric_x), !is.na(metric_y), is.finite(rho))

    if (nrow(corr_baseline) > 0) {
      p_s1 <- ggplot2::ggplot(corr_baseline, ggplot2::aes(metric_x, metric_y, fill = rho)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.4) +
        ggplot2::geom_text(ggplot2::aes(label = label), size = 3.2) +
        ggplot2::scale_fill_gradient2(low = "#3B4CC0", mid = "white", high = "#B40426", midpoint = 0, limits = c(-1, 1), name = "Spearman's rho") +
        ggplot2::labs(x = NULL, y = NULL, title = "Polygon-level Spearman collinearity among GEDI metrics") +
        ggplot2::coord_fixed() +
        theme_repo()
      file_s1 <- save_plot(p_s1, "figure_s1_polygon_spearman_heatmap.png", width = 6.5, height = 5.5)
      register("Fig. S1", file_s1, "Spearman collinearity among GEDI-derived structural metrics at the polygon level under the baseline QC scenario.")
    }
  }
} else {
  warning("Correlation file not found. Run Script 04 first: ", opt$correlations_file)
}

# ----------------------------- 4) FIGURE S2 ----------------------------------
if (file.exists(opt$delta_eps2_file)) {
  delta <- readr::read_csv(opt$delta_eps2_file, show_col_types = FALSE)
  if (nrow(delta) > 0) {
    var_col <- if ("variavel" %in% names(delta)) "variavel" else "variable"
    delta_plot <- delta |>
      dplyr::mutate(
        metric = factor(std_metric(.data[[var_col]]), levels = metric_order),
        scenario_label = std_scenario(scenario),
        delta_epsilon_squared = as.numeric(delta_eps2)
      ) |>
      dplyr::filter(!is.na(metric), is.finite(delta_epsilon_squared), scenario != "S1_baseline")

    if (nrow(delta_plot) > 0) {
      p_s2 <- ggplot2::ggplot(delta_plot, ggplot2::aes(x = scenario_label, y = delta_epsilon_squared, fill = metric)) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed") +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7, colour = "grey20") +
        ggplot2::labs(
          x = "QC/acquisition scenario",
          y = expression(Delta * epsilon^2),
          fill = "Metric",
          title = "QC/acquisition sensitivity of polygon-level Kruskal-Wallis effect sizes"
        ) +
        theme_repo()
      file_s2 <- save_plot(p_s2, "figure_s2_qc_delta_epsilon2.png", width = 8.5, height = 5.5)
      register("Fig. S2", file_s2, "Change in polygon-level Kruskal-Wallis epsilon-squared effect size relative to the baseline scenario across QC and acquisition subsets.")
    }
  }
} else {
  warning("Delta epsilon-squared file not found. Run Script 04 first: ", opt$delta_eps2_file)
}

# ----------------------------- 5) EXPORT CAPTIONS ----------------------------
readr::write_csv(figure_registry, file.path(opt$dir_out, "supplementary_figure_captions.csv"), na = "")
capture.output(sessionInfo(), file = file.path(opt$dir_out, "session_info_06_supplementary_figures.txt"))

msg("Supplementary figures exported to: ", opt$dir_fig)
msg("Done: 06_make_supplementary_figures.R")
