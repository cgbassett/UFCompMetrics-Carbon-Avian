#Corey Bassett
#Script for Carbon and Avian outcomes, as coded previously in Hutt-Taylor et al., 2024 (See Table 3 of that paper for definitions).

#packagesneeded
source('0-packages.R')

#carbon outcomes

  # Split the Carbon.metric into individual items and count unique strings
  carbonoutcomes_strings <- unlist(strsplit(as.character(carbon.separatemetrics$Carbon.metric), ", "))
  
  # Count the number of unique strings
  carbonoutcomes_count <- table(carbonoutcomes_strings)
  print(carbonoutcomes_count)
  
carbon.outcomes <- data.frame(Outcome = c("Above-ground biomass", 
                                  "Below-ground biomass", 
                                  "Dead wood/organic matter", 
                                  "Infrastructure (timber buildings)",
                                  "Soil"),
                       Count = c(109,39,7,1,13)
                       )
carbon.outcomes <- 
  carbon.outcomes %>%
  mutate(Percent = (Count/109)*100)

#avian outcomes
  
  # Split the Bird.domainraw into individual items and count unique strings
  avianoutcomes_strings <- unlist(strsplit(as.character(avian.separatemetrics$Bird.domainraw), ", "))
  
  # Count the number of unique strings
  avianoutcomes_count <- table(avianoutcomes_strings)
  print(avianoutcomes_count)
  
  
  avian.outcomes <- data.frame(Outcome = c("Behaviour",          
                                           "Biodiversity",             
                                           "Breeding", 
                                           "Demographics/Patterns",
                                           "Resources",              
                                           "Survival"
                                           ),
                                Count = c(15,124,27,17,2,4)
                                )

  avian.outcomes <- 
    avian.outcomes %>%
    mutate(Percent = (Count/158)*100)
  

  

# Heatmap: frequency of each composition metric by carbon outcome
comp_cols <- names(carbon.separatemetrics) |> stringr::str_subset("^Composition\\.metric_")

carbon_metric_outcome_heatmap <- carbon.separatemetrics |>
  select(Rayyan.ID, Carbon.metric, all_of(comp_cols)) |>
  separate_rows(Carbon.metric, sep = ", ") |>
  group_by(Carbon.metric) |>
  summarise(across(all_of(comp_cols), ~sum(., na.rm = TRUE))) |>
  pivot_longer(-Carbon.metric, names_to = "metric", values_to = "n") |>
  mutate(metric = str_remove(metric, "Composition\\.metric_"),
         metric = str_to_title(metric)) |>
  filter(metric != "N/A") |>
  ggplot(aes(x = Carbon.metric, y = fct_reorder(metric, n, .fun = sum), fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n, color = n), size = 3) +
  scale_fill_viridis_c(option = "viridis", direction = 1) +
  scale_color_gradient(low = "white", high = "black", guide = "none") +
  labs(
    x = "Carbon Outcome",
    y = "Composition Metric",
    fill = "# Articles",
    title = "Frequency of Composition Metrics by Carbon Outcome"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

carbon_metric_outcome_heatmap

ggsave("figs/carbon_metric_outcome_heatmap.pdf", plot = carbon_metric_outcome_heatmap,
       width = 8, height = 7, units = "in", dpi = 300)


# Heatmap: frequency of each composition metric by avian outcome
avian_comp_cols <- names(avian.separatemetrics) |> stringr::str_subset("^Composition\\.metric_")

avian_metric_outcome_heatmap <- avian.separatemetrics |>
  select(Rayyan.ID, Bird.domainraw, all_of(avian_comp_cols)) |>
  separate_rows(Bird.domainraw, sep = ", ") |>
  filter(!is.na(Bird.domainraw)) |>
  group_by(Bird.domainraw) |>
  summarise(across(all_of(avian_comp_cols), ~sum(., na.rm = TRUE))) |>
  pivot_longer(-Bird.domainraw, names_to = "metric", values_to = "n") |>
  mutate(metric = str_remove(metric, "Composition\\.metric_"),
         metric = str_to_title(metric)) |>
  filter(metric != "N/A") |>
  ggplot(aes(x = Bird.domainraw, y = fct_reorder(metric, n, .fun = sum), fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n, color = n), size = 3) +
  scale_fill_viridis_c(option = "viridis", direction = 1) +
  scale_color_gradient(low = "white", high = "black", guide = "none") +
  labs(
    x = "Avian Outcome",
    y = "Composition Metric",
    fill = "# Articles",
    title = "Frequency of Composition Metrics by Avian Outcome",
    caption = "N=157, one avian article was only about cavities which could be used as nests and thus did not have a coded avian success outcome"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

avian_metric_outcome_heatmap

ggsave("figs/avian_metric_outcome_heatmap.pdf", plot = avian_metric_outcome_heatmap,
       width = 8, height = 7, units = "in", dpi = 300)

