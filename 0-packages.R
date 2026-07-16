# Packages used across the analysis scripts

library(tidyverse)       # dplyr, ggplot2, stringr, tidyr, forcats, etc.
library(openxlsx)        # read.xlsx()
library(splitstackshape) # cSplit_e()
library(cowplot)         # plot_grid(), ggdraw(), draw_label()
library(patchwork)       # combine ggplots (plot_layout())
library(pheatmap)        # clustered heatmaps
rm(list = ls())

scripts <- c(
  "1-UFcompmetrics-cleaning.R",
  "2-PctArticlesbyMetricsbyAvianCarbon.R",
  "3-CarbonAvianOutcomes.R",
  "4-ClusterAnalysis.R",
  "5-Scale.R",
  "6-articlespermetric.R",
  "7-VegLayerTypes.R"
)

results <- data.frame(script = scripts, status = NA_character_, message = NA_character_)
for (i in seq_along(scripts)) {
  res <- tryCatch({ source(scripts[i]); "OK" },
                  error = function(e) paste("ERROR:", conditionMessage(e)))
  results$status[i]  <- if (res == "OK") "pass" else "fail"
  results$message[i] <- if (res == "OK") "" else res
}
results