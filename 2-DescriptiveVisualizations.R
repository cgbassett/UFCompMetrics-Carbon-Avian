#Corey Bassett
#Script for visualizations of UF composition metrics

#packagesneeded
source('0-packages.R')


#bar plots with counts of total
carboncounts_barplot <- counts.carbon.compmetricsonly_df %>%
  ggplot(aes(x = Column, y = Count)) +
  geom_bar(stat = "identity", fill = "#FDE725FF") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Count of carbon articles by composition metric (n=109)", x = "Composition metric", y = "Count of articles") +
  theme_minimal()
print(carboncounts_barplot)

aviancounts_barplot  <- counts.avian.separatemetricsonly_df %>%
  ggplot(aes(x = Column, y = Count)) +
  geom_bar(stat = "identity", fill = "#404788FF") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Count of avian articles by composition metric (n=158)", x = "Composition metric", y = "Count of articles") +
  theme_minimal()
print(aviancounts_barplot)

#bar plots with percent of total

pct.carbon.compmetricsonly_df <- counts.carbon.compmetricsonly_df %>%
  mutate(Percent = (Count / 109) * 100)

carbonpct_barplot <- pct.carbon.compmetricsonly_df %>%
  ggplot(aes(x = Column, y = Percent)) +
  geom_bar(stat = "identity", fill = "#FDE725FF") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  labs(x = "Composition metric", y = "Percent of articles") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10)) +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), hjust = -0.1) #value labels on bars
print(carbonpct_barplot)

pct.avian.compmetricsonly_df <- counts.avian.compmetricsonly_df %>%
  mutate(Percent = (Count / 158) * 100)

avianpct_barplot <- pct.avian.compmetricsonly_df %>%
  ggplot(aes(x = Column, y = Percent)) +
  geom_bar(stat = "identity", fill = "#404788FF") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  labs(
    x = "Composition metric",
    y = "Percent of articles"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))
print(avianpct_barplot)

#side by side bar -- doesn't really work well with two dfs, need to join

sidebyside <- ggplot() +
  geom_bar(data = counts.avian.separatemetricsonly_df, 
           aes(x = Column, y = Count), 
           stat = "identity", 
           position = "dodge",
           color = "blue") +
  geom_bar(data = counts.carbon.compmetricsonly_df, 
           aes(x = Column, y = Count),
           stat = "identity")+
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(sidebyside)

#side by side with cowplot ***Not that helpful, because the metrics are in diff orders
    
    combined_plot <- plot_grid(
      carbonpct_barplot, 
      avianpct_barplot, 
      labels = c("Carbon (n=109)", "Avian (n=158)"),
      ncol = 2,
      align = "v",  # Align vertically for y-axis consistency
      axis = "tb"   # Align top and bottom axes
    )
    
    print(combined_plot)
    
#trying out combining

pct.carbon.compmetricsonly_df$Group <- "Carbon"
pct.avian.compmetricsonly_df$Group <- "Avian"

combined_df <- bind_rows(pct.carbon.compmetricsonly_df, pct.avian.compmetricsonly_df)

combined_df <- combined_df %>%
  left_join(list.compmetrics, by = c("Column" = "Composition.metric"))

combined_barplot <- combined_df %>%
  ggplot(aes(x = Column, y = Percent, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("Carbon" = "#FDE725FF", "Avian" = "#404788FF")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  labs(title = "Percent of Articles by Composition Metric",
       x = "Composition Metric",
       y = "Percent of Articles",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(combined_barplot)

#faceted by category **I like this the best

facetedcombined <- 
  ggplot(combined_df, aes(x = Column, y = Percent, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", size = 0.1) +
  facet_wrap(~ Category.of.composition.metric, scales = "free_x") +
  scale_fill_manual(values = c("Carbon" = "#FDE725FF", "Avian" = "#404788FF")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  labs(title = "Percent of Avian and Carbon articles by Composition Metric and Category",
       x = "Composition Metric",
       y = "Percent of Articles",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(strip.background = element_rect(fill = "grey90", color = NA)) +
  aes(x = fct_reorder(Column, Percent))
facetedcombined
ggsave("figs/faceted_combined_barplot.pdf", plot = facetedcombined, width = 10, height = 8, units = "in", dpi = 300)


#combined, not facted by category
combined_barplot <- combined_df %>%
  ggplot(aes(x = fct_reorder(Column, Percent, .desc = TRUE), 
             y = Percent, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("Carbon" = "#FDE725FF", "Avian" = "#404788FF")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  labs(title = "Percent of Articles by Composition Metric",
       x = "Composition Metric",
       y = "Percent of Articles",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(combined_barplot)
ggsave("figs/combined_barplot.pdf", plot = combined_plot, width = 10, height = 8, units = "in", dpi = 300)
