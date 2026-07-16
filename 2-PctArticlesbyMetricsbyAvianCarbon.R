#Corey Bassett
#Standalone script: side-by-side bar charts of percent of articles by
#composition metric, for carbon and avian articles.

# Packages ------------------------------------------------------------------
library(tidyverse)      # dplyr, stringr, ggplot2, forcats, etc.
library(splitstackshape) # cSplit_e()
library(patchwork)       # combine plots with a shared legend
library(openxlsx)        # read.xlsx()

# Data location. Set DATA_DIR to the folder containing the raw data files,
# e.g. Sys.setenv(DATA_DIR = "path/to/data")
data_dir <- Sys.getenv("DATA_DIR")

# Category lookup for each composition metric
list.compmetrics <- read.xlsx(file.path(data_dir, "ListofUFCompMetrics.xlsx"))

# Helper: read raw data, split the multi-value Composition.metric column into
# one indicator column per metric, drop N/A-only articles, and count how many
# articles used each metric.
count_metrics <- function(path, keep_cols, na_articles) {
  raw <- read.csv(path)

  meta <- raw[, keep_cols] %>%
    mutate(across(where(is.character), str_trim))
  meta <- replace(meta, meta == "", NA)

  separated <- cSplit_e(meta, split.col = "Composition.metric",
                        sep = ",", type = "character") %>%
    filter(!Rayyan.ID %in% na_articles)

  metrics_only <- select(separated, contains("Composition.metric_"))
  colnames(metrics_only) <- sub("Composition.metric_", "", colnames(metrics_only))
  metrics_only[is.na(metrics_only)] <- 0

  data.frame(
    Column = names(colSums(metrics_only == 1)),
    Count  = colSums(metrics_only == 1)
  ) %>%
    filter(Column != "N/A")
}

# Carbon --------------------------------------------------------------------
carbon_cols <- c("Rayyan.ID", "Full.citation", "Title", "Year", "Journal",
                 "Publication.Type.", "Country.of.First.Author", "Study.Country",
                 "Urb.scale", "Year.start", "Year.end", "Comparator",
                 "Forest.comp", "Rec.included", "Rec1", "Rec2", "Rec3",
                 "Carbon.metric", "Composition.metric")

counts.carbon.compmetricsonly_df <- count_metrics(
  file.path(data_dir, "CarbonUFCompositionMetric_RawData_21Nov24.csv"),
  keep_cols = carbon_cols,
  na_articles = c(879385490, 879386384)
)

# Avian ---------------------------------------------------------------------
avian_cols <- c("Rayyan.ID", "Citation", "Country.Auth", "Title", "Journal",
                "Study.country", "Urb.scale", "Forest.comp", "Scale.meas",
                "Bird.domainraw", "Bird.category", "Category.multi",
                "Composition.metric")

counts.avian.compmetricsonly_df <- count_metrics(
  file.path(data_dir, "AvianUFCompositionMetric_RawData_21Nov24.csv"),
  keep_cols = avian_cols,
  na_articles = c(364026331, 364026955)
)

# Percent of articles per metric, with category ------------------------------
# Full set of metrics and their categories (drop the N/A placeholder). Joining
# counts onto this keeps every metric in both charts, including those at 0%.
metric_categories <- list.compmetrics %>%
  filter(Composition.metric != "N/A") %>%
  rename(Column = Composition.metric, Category = Category.of.composition.metric)

pct.carbon <- metric_categories %>%
  left_join(mutate(counts.carbon.compmetricsonly_df, Column = as.character(Column)),
            by = "Column") %>%
  mutate(Count = replace_na(Count, 0),
         Percent = (Count / 109) * 100)

pct.avian <- metric_categories %>%
  left_join(mutate(counts.avian.compmetricsonly_df, Column = as.character(Column)),
            by = "Column") %>%
  mutate(Count = replace_na(Count, 0),
         Percent = (Count / 158) * 100)

# Shared category colors so the legend matches across panels
category_colors <- c(
  "size"                  = "#440154FF",
  "structure"             = "#443A83FF",
  "taxonomy"              = "#31688EFF",
  "tree characteristics"  = "#35B779FF",
  "vegetation layer type" = "#FDE725FF"
)

# Carbon panel: metrics sorted by their own percent
carbonpct_barplot <- pct.carbon %>%
  ggplot(aes(x = Percent, y = fct_reorder(Column, Percent), fill = Category)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), hjust = -0.1, size = 2.5) +
  scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 105), expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(values = category_colors) +
  labs(title = "Carbon (n=109)", x = "Percent of articles", y = "Composition metric") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8), plot.title = element_text(hjust = 0))

# Avian panel: metrics sorted by their own percent
avianpct_barplot <- pct.avian %>%
  ggplot(aes(x = Percent, y = fct_reorder(Column, Percent), fill = Category)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), hjust = -0.1, size = 2.5) +
  scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 105), expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(values = category_colors) +
  labs(title = "Avian (n=158)", x = "Percent of articles", y = "Composition metric") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8), plot.title = element_text(hjust = 0))

# Side-by-side with a single shared legend at the bottom
sidebyside_pct <- carbonpct_barplot + avianpct_barplot +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(sidebyside_pct)
ggsave("figs/pct_articles_by_metric_avian_carbon.pdf", plot = sidebyside_pct,
       width = 12, height = 6, units = "in", dpi = 300)
