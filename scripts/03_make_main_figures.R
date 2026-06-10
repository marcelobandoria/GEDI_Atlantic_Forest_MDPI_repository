# -----------------------------------------------------------------------------
# Script 03 — Generate manuscript exploratory boxplots
#
# Project: GEDI-derived forest structure in Atlantic Forest physiognomies
# Repository workflow: manuscript reproducibility
# Author: Marcelo C. S. Bandoria
#
# Purpose
# This script generates the English-only exploratory boxplots used in the
# manuscript workflow. The figures are produced for the five GEDI-derived
# structural metrics: canopy height, AGBD, PAI, canopy cover, and FHD.
#
# Scope
# This script intentionally generates only the main exploratory boxplots by
# variable. It does not generate QC-sensitivity figures, correlation heatmaps,
# distance-to-edge histograms, or supplementary synthesis panels.
#
# Main input
#   results/intermediate/01_manuscript_prepared_dataset.rds
#
# Main outputs
#   results/figures/figure_01_canopy_height_boxplot.png
#   results/figures/figure_02_agbd_boxplot.png
#   results/figures/figure_03_pai_boxplot.png
#   results/figures/figure_04_canopy_cover_boxplot.png
#   results/figures/figure_05_fhd_boxplot.png
#
# Run third. For a quick figure test, run:
#   Sys.setenv(GEDI_FAST_TEST = "true")
# For final repository figures, run:
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
  font_family = "Times New Roman",
  base_size = 12,
  dpi = if (fast_test) 150 else 500,
  dir_fig = repo_path("results", "figures"),
  xlsx_file = repo_path("results", "main", "manuscript_results.xlsx"),
  rng = list(
    H     = c(0, 50),
    FHD   = c(0, 5),
    AGBD  = c(0, 300),
    PAI   = c(0.1, 8),
    COVER = c(10, 95)
  ),
  breaks = list(
    H     = seq(0, 50, 10),
    FHD   = seq(0, 5, 0.5),
    AGBD  = seq(0, 500, 50),
    PAI   = seq(0, 10, 1),
    COVER = seq(10, 95, 10)
  ),
  colors = c(
    FOD = "#0052CC",
    FOM = "#00AA50",
    FES = "#7A00CC",
    FED = "#DC0000"
  )
)

# ----------------------------- 1) PACKAGES -----------------------------------
need <- c("dplyr", "ggplot2", "grid", "openxlsx", "readr", "sysfonts", "showtext", "tibble", "cowplot")

to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(need, require, character.only = TRUE))
options(dplyr.summarise.inform = FALSE)

# ----------------------------- 2) HELPERS ------------------------------------
mk_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
mk_dir(opt$dir_fig)
msg <- function(...) cat(paste0(..., "\n"))

try({
  sysfonts::font_add(family = opt$font_family, regular = opt$font_family)
  showtext::showtext_auto()
}, silent = TRUE)

theme_pub <- function(){
  ggplot2::theme_minimal(base_family = opt$font_family, base_size = opt$base_size) +
    ggplot2::theme(
      legend.position   = "right",
      legend.title      = element_text(size = 12, face = "bold"),
      legend.text       = element_text(size = 12),
      legend.key.height = grid::unit(0.8, "cm"),
      legend.key.width  = grid::unit(0.8, "cm"),
      plot.title        = element_text(size = 11, face = "bold"),
      axis.title.x      = element_text(size = 11, margin = margin(t = 6)),
      axis.title.y      = element_text(size = 11, margin = margin(r = 6)),
      axis.text.x       = element_text(size = 9, angle = 0, vjust = 1, hjust = 0.5, margin = margin(t = 4)),
      axis.text.y       = element_text(size = 9),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank()
    )
}

save_plot <- function(p, fname, w = 7, h = 5){
  f <- file.path(opt$dir_fig, paste0(fname, ".png"))
  ggplot2::ggsave(filename = f, plot = p, width = w, height = h, dpi = opt$dpi)
  f
}

# ----------------------------- 3) LOAD DATA ----------------------------------
prep_file <- repo_path("results", "intermediate", "01_manuscript_prepared_dataset.rds")
if (!file.exists(prep_file)) {
  stop("Prepared dataset not found. Run scripts/01_prepare_manuscript_dataset.R first.")
}
prep <- readRDS(prep_file)
df <- prep$df_balanced

# ----------------------------- 4) LABELS --------------------------------------
phys_labels_legend <- c(
  FOD = "Dense Ombrophilous Forest (DOF)",
  FOM = "Mixed Ombrophilous Forest (MOF)",
  FES = "Seasonal Semideciduous Forest (SSdF)",
  FED = "Seasonal Deciduous Forest (SDF)"
)

phys_labels_axis <- c(
  FOD = "DOF",
  FOM = "MOF",
  FES = "SSdF",
  FED = "SDF"
)

metric_labels <- c(
  altura_dossel = "Canopy height - H (m)",
  agbd          = "Aboveground biomass density - AGBD (Mg ha^-1)",
  pai           = "Plant area index - PAI (m^2 m^-2)",
  cover         = "Canopy cover - COVER (%)",
  fhd_normal    = "Foliage height diversity - FHD"
)

theme_set(theme_pub())

# ----------------------------- 5) BOXPLOTS -----------------------------------
make_boxplot <- function(data, var, ylab, ylim, breaks){
  ggplot(data, aes(fito_grupo, .data[[var]], fill = fito_grupo)) +
    geom_boxplot(outlier.alpha = .25, width = .7, colour = "grey20") +
    scale_fill_manual(
      values = opt$colors,
      drop = TRUE,
      name = "Forest physiognomies",
      labels = phys_labels_legend
    ) +
    scale_x_discrete(labels = phys_labels_axis) +
    scale_y_continuous(
      limits = ylim,
      breaks = breaks,
      expand = ggplot2::expansion(mult = c(0.01, 0.05))
    ) +
    labs(
      x = "Forest physiognomies",
      y = ylab,
      title = paste("Distribution of", ylab, "by forest physiognomy")
    ) +
    theme_pub()
}

plot_specs <- list(
  figure_01_canopy_height_boxplot = list(var = "altura_dossel", label = metric_labels[["altura_dossel"]], ylim = opt$rng$H,     breaks = opt$breaks$H),
  figure_02_agbd_boxplot          = list(var = "agbd",          label = metric_labels[["agbd"]],          ylim = opt$rng$AGBD,  breaks = opt$breaks$AGBD),
  figure_03_pai_boxplot           = list(var = "pai",           label = metric_labels[["pai"]],           ylim = opt$rng$PAI,   breaks = opt$breaks$PAI),
  figure_04_canopy_cover_boxplot  = list(var = "cover",         label = metric_labels[["cover"]],         ylim = opt$rng$COVER, breaks = opt$breaks$COVER),
  figure_05_fhd_boxplot           = list(var = "fhd_normal",    label = metric_labels[["fhd_normal"]],    ylim = opt$rng$FHD,   breaks = opt$breaks$FHD)
)

plot_objects <- lapply(names(plot_specs), function(fname) {
  spec <- plot_specs[[fname]]
  make_boxplot(df, spec$var, spec$label, spec$ylim, spec$breaks)
})
names(plot_objects) <- names(plot_specs)

figure_paths <- mapply(
  FUN = function(fname, p) save_plot(p, fname, 7, 5),
  fname = names(plot_objects),
  p = plot_objects,
  SIMPLIFY = TRUE
)

combined_figure <- cowplot::plot_grid(plotlist = plot_objects, ncol = 3, labels = "AUTO")
combined_path <- save_plot(combined_figure, "figure_07_boxplots_by_physiognomy", 12, 8)
figure_paths <- c(figure_paths, figure_07_boxplots_by_physiognomy = combined_path)

figure_captions <- tibble::tibble(
  figure_file = basename(figure_paths),
  figure_type = "Exploratory boxplot",
  description = c(
    "Canopy height distribution by Atlantic Forest physiognomy.",
    "Aboveground biomass density distribution by Atlantic Forest physiognomy.",
    "Plant area index distribution by Atlantic Forest physiognomy.",
    "Canopy cover distribution by Atlantic Forest physiognomy.",
    "Foliage height diversity distribution by Atlantic Forest physiognomy.",
    "Combined multi-panel boxplot figure corresponding to manuscript Figure 7."
  ),
  note = "Figures use the balanced post-edge-filter dataset reproduced from the manuscript-reference workflow."
)
readr::write_csv(figure_captions, repo_path("results", "figures", "figure_captions.csv"))

# ----------------------------- 6) INSERT INTO WORKBOOK ------------------------
if (file.exists(opt$xlsx_file)) {
  msg("Inserting boxplot figures into workbook: ", opt$xlsx_file)
  wb <- openxlsx::loadWorkbook(opt$xlsx_file)
  if ("figures" %in% names(wb)) openxlsx::removeWorksheet(wb, "figures")
  openxlsx::addWorksheet(wb, "figures")

  openxlsx::writeData(wb, "figures", figure_captions, startRow = 1, startCol = 1)

  row_pos <- 10
  for (img in figure_paths) {
    openxlsx::insertImage(
      wb, "figures", img,
      startRow = row_pos, startCol = 1,
      width = 18, height = 14, units = "cm"
    )
    row_pos <- row_pos + 24
  }

  openxlsx::saveWorkbook(wb, opt$xlsx_file, overwrite = TRUE)
}

capture.output(sessionInfo(), file = repo_path("results", "figures", "session_info_03_figures.txt"))

msg("\nFigures generated:")
print(figure_paths)
msg("\nDone: 03_make_manuscript_figures.R")
