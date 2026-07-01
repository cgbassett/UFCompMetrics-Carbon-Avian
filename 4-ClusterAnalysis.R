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

# Helper to plot clustered heatmap with dendrograms via pheatmap
plot_cooccur_heatmap <- function(jacc_mat, title, filename = NA) {
  # sqrt(1 - Jaccard) is Euclidean; ward.D2 squares it internally, so the
  # input must be the un-squared distance (not 1 - S) for correct Ward's method
  d <- as.dist(sqrt(1 - jacc_mat))

  pheatmap(
    jacc_mat,
    clustering_distance_rows = d,
    clustering_distance_cols = d,
    clustering_method        = "ward.D2",
    color        = viridisLite::viridis(100),
    breaks       = seq(0, 1, length.out = 101),
    legend_breaks = c(0, 0.25, 0.5, 0.75, 1, 1),
    legend_labels = c("0", "0.25", "0.50", "0.75", "1",
                      "Jaccard\nsimilarity\n"),
    border_color = "white",
    display_numbers = FALSE,
    main         = title,
    fontsize     = 8,
    filename     = filename
  )
}

avian_jacc  <- jaccard_similarity(avian_mat)
carbon_jacc <- jaccard_similarity(carbon_mat)

# Plot (dendrograms drawn on rows and columns)
avian_cooccur_plot <- plot_cooccur_heatmap(avian_jacc,
                                           "Avian: Metric Co-occurrence (Jaccard Similarity)")
carbon_cooccur_plot <- plot_cooccur_heatmap(carbon_jacc,
                                            "Carbon: Metric Co-occurrence (Jaccard Similarity)")

# Save (pheatmap writes directly to file based on extension)
plot_cooccur_heatmap(avian_jacc,
                     "Avian: Metric Co-occurrence (Jaccard Similarity)",
                     filename = "figs/avian_metric_cooccurrence.pdf")
plot_cooccur_heatmap(carbon_jacc,
                     "Carbon: Metric Co-occurrence (Jaccard Similarity)",
                     filename = "figs/carbon_metric_cooccurrence.pdf")

