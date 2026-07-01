#Corey Bassett
#Script for metric co-occurrence analysis (Jaccard similarity)

#packagesneeded
source('0-packages.R')

# Build binary matrices (articles x metrics), replacing NA with 0
avian_comp_cols <- names(avian.separatemetrics) |> stringr::str_subset("^Composition\\.metric_")
comp_cols       <- names(carbon.separatemetrics) |> stringr::str_subset("^Composition\\.metric_")

avian_mat <- avian.separatemetrics |>
  select(all_of(avian_comp_cols)) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  as.matrix()

carbon_mat <- carbon.separatemetrics |>
  select(all_of(comp_cols)) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  as.matrix()

# Clean up column names
colnames(avian_mat)  <- str_remove(colnames(avian_mat),  "Composition\\.metric_") |> str_to_title()
colnames(carbon_mat) <- str_remove(colnames(carbon_mat), "Composition\\.metric_") |> str_to_title()

# Remove N/A column if present
avian_mat  <- avian_mat[,  colnames(avian_mat)  != "N/A"]
carbon_mat <- carbon_mat[, colnames(carbon_mat) != "N/A"]

# Compute Jaccard similarity between metrics (columns)
jaccard_similarity <- function(mat) {
  cooccur <- crossprod(mat)
  totals  <- diag(cooccur)
  n       <- ncol(mat)
  jacc    <- matrix(0, n, n, dimnames = list(colnames(mat), colnames(mat)))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      union_ij   <- totals[i] + totals[j] - cooccur[i, j]
      jacc[i, j] <- if (union_ij == 0) 0 else cooccur[i, j] / union_ij
    }
  }
  jacc
}

# pheatmap has no native legend-title argument, so add one as a grob in the
# cell directly above the legend (keeps the "1" tick at the top of the bar)
add_legend_title <- function(ph, label = "Jaccard\nsimilarity", fontsize = 8) {
  g       <- ph$gtable
  leg_col <- max(g$layout$l[g$layout$name == "legend"])
  title   <- grid::textGrob(
    label, x = grid::unit(0, "npc"), y = grid::unit(0.15, "npc"),
    hjust = 0, vjust = 0, gp = grid::gpar(fontsize = fontsize)
  )
  gtable::gtable_add_grob(g, title, t = 2, l = leg_col, name = "legend_title")
}

# Helper to build clustered heatmap with dendrograms via pheatmap
plot_cooccur_heatmap <- function(jacc_mat, title) {
  # sqrt(1 - Jaccard) is Euclidean; ward.D2 squares it internally, so the
  # input must be the un-squared distance (not 1 - S) for correct Ward's method
  d <- as.dist(sqrt(1 - jacc_mat))

  ph <- pheatmap(
    jacc_mat,
    clustering_distance_rows = d,
    clustering_distance_cols = d,
    clustering_method        = "ward.D2",
    color        = viridisLite::viridis(100),
    breaks       = seq(0, 1, length.out = 101),
    legend_breaks = c(0, 0.25, 0.5, 0.75, 1),
    legend_labels = c("0", "0.25", "0.50", "0.75", "1"),
    border_color = "white",
    display_numbers = FALSE,
    main         = title,
    fontsize     = 8,
    silent       = TRUE
  )
  add_legend_title(ph)
}

# Draw the augmented gtable to screen or to a PDF file
draw_cooccur_heatmap <- function(g, filename = NULL, width = 8, height = 7) {
  if (!is.null(filename)) {
    pdf(filename, width = width, height = height)
    on.exit(dev.off())
  }
  grid::grid.newpage()
  grid::grid.draw(g)
}

avian_jacc  <- jaccard_similarity(avian_mat)
carbon_jacc <- jaccard_similarity(carbon_mat)

# Build (dendrograms drawn on rows and columns, legend titled above the bar)
avian_cooccur_plot <- plot_cooccur_heatmap(avian_jacc,
                                           "Avian: Metric Co-occurrence (Jaccard Similarity)")
carbon_cooccur_plot <- plot_cooccur_heatmap(carbon_jacc,
                                            "Carbon: Metric Co-occurrence (Jaccard Similarity)")

# Display
draw_cooccur_heatmap(avian_cooccur_plot)
draw_cooccur_heatmap(carbon_cooccur_plot)

# Save
draw_cooccur_heatmap(avian_cooccur_plot,  "figs/avian_metric_cooccurrence.pdf")
draw_cooccur_heatmap(carbon_cooccur_plot, "figs/carbon_metric_cooccurrence.pdf")

