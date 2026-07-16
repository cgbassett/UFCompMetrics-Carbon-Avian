#Corey Bassett
#Regenerate publication-ready (title-free) versions of the pipeline figures.
#Sources the analysis scripts to (re)build the plot objects, then writes
#copies with descriptive titles/captions removed into figs/no title figs.
#The Carbon/Avian panel identifiers are retained so panels stay distinguishable.

# Rebuild all plot objects. Each script loads packages and cleaned data, so
# DATA_DIR must be set (see README) before running this script.
source('2-PctArticlesbyMetricsbyAvianCarbon.R')  # sidebyside_pct
source('3-CarbonAvianOutcomes.R')                # carbon/avian metric-outcome heatmaps
source('4-ClusterAnalysis.R')                    # co-occurrence helpers + *_jacc matrices
source('6-articlespermetric.R')                  # combined_hist
source('7-VegLayerTypes.R')                      # veglayer_plot, veglayer_freq_plot

# Output folder
out_dir <- "figs/no title figs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Percent-by-metric side-by-side (keep Carbon/Avian panel identifiers)
ggsave(file.path(out_dir, "pct_articles_by_metric_avian_carbon.pdf"),
       sidebyside_pct, width = 12, height = 6, units = "in", dpi = 300)

# Metric-by-outcome heatmaps
ggsave(file.path(out_dir, "carbon_metric_outcome_heatmap.pdf"),
       carbon_metric_outcome_heatmap + labs(title = NULL),
       width = 8, height = 7, units = "in", dpi = 300)
ggsave(file.path(out_dir, "avian_metric_outcome_heatmap.pdf"),
       avian_metric_outcome_heatmap + labs(title = NULL, caption = NULL),
       width = 8, height = 7, units = "in", dpi = 300)

# Co-occurrence heatmaps (short "Avian"/"Carbon" label instead of full title)
draw_cooccur_heatmap(plot_cooccur_heatmap(avian_jacc, "Avian"),
                     file.path(out_dir, "avian_metric_cooccurrence.pdf"))
draw_cooccur_heatmap(plot_cooccur_heatmap(carbon_jacc, "Carbon"),
                     file.path(out_dir, "carbon_metric_cooccurrence.pdf"))

# Metrics-per-article histograms (shared title dropped; panels keep labels/tags)
ggsave(file.path(out_dir, "histograms_metrics_per_article_pct.pdf"),
       combined_hist, width = 7, height = 5, units = "in", dpi = 300)

# Vegetation layer figures
ggsave(file.path(out_dir, "veglayer_types_per_article.pdf"),
       veglayer_plot + labs(title = NULL),
       width = 7, height = 5, units = "in", dpi = 300)
ggsave(file.path(out_dir, "veglayer_freq_by_type.pdf"),
       veglayer_freq_plot + labs(title = NULL),
       width = 7, height = 5, units = "in", dpi = 300)
