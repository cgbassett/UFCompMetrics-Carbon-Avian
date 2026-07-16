#Corey Bassett
#Script for visualizations and analysis of vegetation layer types category between the avian and carbon datasets

#packages and cleaned data (sources 0-packages.R internally)
source('1-UFcompmetrics-cleaning.R')

#How many vegetation layer types are considered per article?

# Select only vegetation layer columns (exclude vertical layer assessment)
avian.veglayercompmetricsonly <- avian.separatemetrics |>
  select(contains("layer"), -contains("vertical layer assessment"))

carbon.veglayercompmetricsonly <- carbon.separatemetrics |>
  select(contains("layer"), -contains("vertical layer assessment"))

# Count layer types studied per article
avian.veglayer_ctperarticle <- avian.veglayercompmetricsonly |>
  mutate(n_layer_types = rowSums(across(everything()), na.rm = TRUE))

carbon.veglayer_ctperarticle <- carbon.veglayercompmetricsonly |>
  mutate(n_layer_types = rowSums(across(everything()), na.rm = TRUE))

# Side-by-side bar chart of percentage of articles by number of layer types
veglayer_plot <- bind_rows(
  avian.veglayer_ctperarticle |> count(n_layer_types) |> mutate(dataset = "Avian"),
  carbon.veglayer_ctperarticle |> count(n_layer_types) |> mutate(dataset = "Carbon")
) |>
  mutate(n_layer_types = factor(n_layer_types)) |>
  group_by(dataset) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  ggplot(aes(x = n_layer_types, y = prop, fill = dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9),
           color = "black", alpha = 0.7) +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("Avian" = "#404788FF", "Carbon" = "#FDE725FF")) +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 0.6)) +
  labs(
    x = "Number of Vegetation Layer Types",
    y = "Percentage of Articles",
    fill = "Dataset",
    title = "Vegetation Layer Types Studied per Article"
  ) +
  theme_minimal()

veglayer_plot

ggsave("figs/veglayer_types_per_article.pdf", plot = veglayer_plot,
       width = 7, height = 5, units = "in", dpi = 300)

median(avian.veglayer_ctperarticle$n_layer_types)
median(carbon.veglayer_ctperarticle$n_layer_types)

mean(avian.veglayer_ctperarticle$n_layer_types)
mean(carbon.veglayer_ctperarticle$n_layer_types)

# Total articles per dataset for normalization
n_avian <- nrow(avian.separatemetrics)
n_carbon <- nrow(carbon.separatemetrics)

# Frequency of each specific layer type, normalized by total articles
veglayer_freq_plot <- bind_rows(
  avian.veglayercompmetricsonly |>
    summarise(across(everything(), ~sum(., na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "layer", values_to = "n") |>
    mutate(dataset = "Avian", prop = n / n_avian),
  carbon.veglayercompmetricsonly |>
    summarise(across(everything(), ~sum(., na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "layer", values_to = "n") |>
    mutate(dataset = "Carbon", prop = n / n_carbon)
) |>
  mutate(layer = str_remove(layer, "Composition.metric_"),
         layer = str_to_title(layer),
         layer = fct_reorder(layer, prop, .fun = max)) |>
  ggplot(aes(y = layer, x = prop, fill = dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9),
           color = "black", alpha = 0.7) +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%")),
            position = position_dodge(width = 0.9),
            hjust = -0.1, size = 3) +
  scale_fill_manual(values = c("Avian" = "#404788FF", "Carbon" = "#FDE725FF")) +
  scale_x_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 0.85)) +
  labs(
    y = "Vegetation Layer Type",
    x = "Percentage of Articles",
    fill = "Dataset",
    title = "Frequency of Vegetation Layer Types Studied"
  ) +
  theme_minimal()

veglayer_freq_plot

ggsave("figs/veglayer_freq_by_type.pdf", plot = veglayer_freq_plot,
       width = 7, height = 5, units = "in", dpi = 300)
