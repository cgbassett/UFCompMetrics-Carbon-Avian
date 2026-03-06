#Corey Bassett
#Script for metric co-occurrence cluster analysis (Jaccard similarity)

#packagesneeded
source('0-packages.R')
library(tidyverse)

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

# Helper to plot clustered heatmap
plot_cooccur_heatmap <- function(jacc_mat, title) {
  hc    <- hclust(as.dist(1 - jacc_mat), method = "ward.D2")
  order <- hc$labels[hc$order]

  as.data.frame(jacc_mat) |>
    rownames_to_column("metric_x") |>
    pivot_longer(-metric_x, names_to = "metric_y", values_to = "jaccard") |>
    mutate(metric_x = factor(metric_x, levels = order),
           metric_y = factor(metric_y, levels = order)) |>
    ggplot(aes(x = metric_x, y = metric_y, fill = jaccard)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "viridis", limits = c(0, 1),
                         labels = scales::percent) +
    labs(x = NULL, y = NULL, fill = "Jaccard\nSimilarity", title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text    = element_text(size = 8))
}

avian_jacc  <- jaccard_similarity(avian_mat)
carbon_jacc <- jaccard_similarity(carbon_mat)

# Plot and save
avian_cooccur_plot <- plot_cooccur_heatmap(avian_jacc,
                                           "Avian: Metric Co-occurrence (Jaccard Similarity)")
carbon_cooccur_plot <- plot_cooccur_heatmap(carbon_jacc,
                                            "Carbon: Metric Co-occurrence (Jaccard Similarity)")

avian_cooccur_plot
carbon_cooccur_plot

ggsave("figs/avian_metric_cooccurrence.pdf",  plot = avian_cooccur_plot,
       width = 8, height = 7, units = "in", dpi = 300)
ggsave("figs/carbon_metric_cooccurrence.pdf", plot = carbon_cooccur_plot,
       width = 8, height = 7, units = "in", dpi = 300)
library(pheatmap)

# Function to compute Jaccard similarity matrix between metrics (columns)
jaccard_similarity <- function(mat) {
  # crossprod gives co-occurrence counts (A ∩ B)
  cooccur <- crossprod(mat)
  # diagonal gives each metric's total count
  totals <- diag(cooccur)
  # Jaccard: |A ∩ B| / |A ∪ B| = cooccur[i,j] / (totals[i] + totals[j] - cooccur[i,j])
  n <- ncol(mat)
  jacc <- matrix(0, n, n, dimnames = list(colnames(mat), colnames(mat)))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      union_ij <- totals[i] + totals[j] - cooccur[i, j]
      jacc[i, j] <- if (union_ij == 0) 0 else cooccur[i, j] / union_ij
    }
  }
  jacc
}

avian_jacc  <- jaccard_similarity(avian_mat)
carbon_jacc <- jaccard_similarity(carbon_mat)

# Plot clustered heatmaps
pheatmap(avian_jacc,
         clustering_distance_rows = as.dist(1 - avian_jacc),
         clustering_distance_cols = as.dist(1 - avian_jacc),
         clustering_method = "ward.D2",
         color = viridis::viridis(100),
         main = "Avian: Metric Co-occurrence (Jaccard Similarity)",
         fontsize = 9)

pheatmap(carbon_jacc,
         clustering_distance_rows = as.dist(1 - carbon_jacc),
         clustering_distance_cols = as.dist(1 - carbon_jacc),
         clustering_method = "ward.D2",
         color = viridis::viridis(100),
         main = "Carbon: Metric Co-occurrence (Jaccard Similarity)",
         fontsize = 9)