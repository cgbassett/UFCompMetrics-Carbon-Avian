#Corey Bassett
#Script for visualizations of UF composition metrics

#packagesneeded
source('0-packages.R')


install.packages("cowplot")
library(cowplot)
install.packages("patchwork")
library(patchwork)


#Count of metrics per article AVIAN

avian.separatemetrics_ctperarticle <- avian.separatemetrics %>%
  mutate(count.metricsperarticle = rowSums(across(`Composition.metric_age`:`Composition.metric_vine layer`), na.rm = TRUE))

summary(avian.separatemetrics_ctperarticle$count.metricsperarticle)

avian.ctsparticle.hist <- ggplot(avian.separatemetrics_ctperarticle,
                                 aes(x = count.metricsperarticle)) +
  geom_histogram(binwidth = 1, 
                 aes(y = after_stat(count)), 
                 fill = "#404788FF", color = "black", alpha = 0.7) +
  geom_text(stat = "bin", 
            aes(label = ifelse(after_stat(count) > 0, after_stat(count), "")), 
            vjust = -0.5) +
  labs(title = "Avian (n=158)", 
       x = "Count of Metrics", 
       y = "Number of Articles") +
  scale_x_continuous(breaks = seq(0, max(avian.separatemetrics_ctperarticle$count.metricsperarticle), by = 1)) +
  scale_y_continuous(limits = c(0, 35)) +
  theme_minimal()

print(avian.ctsparticle.hist)

#Count of metrics per article CARBON

carbon.separatemetrics_ctperarticle <- carbon.separatemetrics %>%
  mutate(count.metricsperarticle = rowSums(across(`Composition.metric_age`:`Composition.metric_vertical layer assessment`), na.rm = TRUE))

summary(carbon.separatemetrics_ctperarticle$count.metricsperarticle)

carbon.ctsparticle.hist <- ggplot(carbon.separatemetrics_ctperarticle,
                                  aes(x = count.metricsperarticle)) +
  geom_histogram(binwidth = 1, 
                 aes(y = after_stat(count)), 
                 fill = "#FDE725FF", color = "black", alpha = 0.7) +
  geom_text(
    stat = "bin", 
    aes(label = ifelse(after_stat(count) == 0, "", after_stat(count))),  # Remove zeros
    vjust = -0.5
  ) +
  scale_x_continuous(breaks = seq(0, max(carbon.separatemetrics_ctperarticle$count.metricsperarticle), by = 1)) +
  scale_y_continuous(limits = c(0, 35)) +
  labs(title = "Carbon (n=109)",
       x = "Count of Metrics", y = "Number of Articles") +
  theme_minimal() 

print(carbon.ctsparticle.hist)


# Combine histograms side by side
combined_histograms <- plot_grid(
  carbon.ctsparticle.hist, 
  avian.ctsparticle.hist, 
  ncol = 2,
  align = "v",  # Align vertically for y-axis consistency
  axis = "tb"   # Align top and bottom axes
)

# Add shared title
title <- ggdraw() + draw_label(
  "Count of forest composition metrics per article",
  fontface = 'bold', size = 14
)

final_hist_plot <- plot_grid(title, combined_histograms, ncol = 1, rel_heights = c(0.1, 1))

# Display combined plot
print(final_hist_plot)

#PERCENTAGES of metrics per article

# Carbon histogram (percentage y-axis)
carbon.ctsparticle.hist <- ggplot(carbon.separatemetrics_ctperarticle, 
                                  aes(x = count.metricsperarticle)) +
  geom_histogram(binwidth = 1, 
                 aes(y = after_stat(count / sum(count)) * 100),
                 fill = "#FDE725FF", color = "black", alpha = 0.7) +
  labs(title = "Carbon (n=109)", 
       x = "Count of Metrics", 
       y = "Percentage of Articles") +
  scale_x_continuous(breaks = seq(0, max(carbon.separatemetrics_ctperarticle$count.metricsperarticle), by = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 25)) +
  theme_minimal()

# Avian histogram (percentage y-axis)
avian.ctsparticle.hist <- ggplot(avian.separatemetrics_ctperarticle, 
                                 aes(x = count.metricsperarticle)) +
  geom_histogram(binwidth = 1, 
                 aes(y = after_stat(count / sum(count)) * 100),
                 fill = "#404788FF", color = "black", alpha = 0.7) +
  labs(title = "Avian (n=158)", 
       x = "Count of Metrics", 
       y = "Percentage of Articles") +
  scale_x_continuous(breaks = seq(0, max(avian.separatemetrics_ctperarticle$count.metricsperarticle), by = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 25)) +
  theme_minimal()

# Combine side by side with shared title
combined_hist <- plot_grid(carbon.ctsparticle.hist, avian.ctsparticle.hist,
                           labels = c("A", "B"), ncol = 2, align = "v", axis = "tb")

title <- ggdraw() + draw_label("Histogram of Metrics per Article (Percentage)", 
                               fontface = 'bold', size = 14)

final_plot <- plot_grid(title, combined_hist, ncol = 1, rel_heights = c(0.1, 1))

print(final_plot)