#Corey Bassett
#Script for visualizations of UF composition metrics

#packagesneeded
source('0-packages.R')

#bar plots with counts of total
carboncounts_barplot <- counts.carbon.compmetricsonly_df %>%
  ggplot(aes(x = Column, y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Count of carbon articles by composition metric", x = "Composition metric", y = "Count of articles")
print(carboncounts_barplot)

aviancounts_barplot  <- counts.avian.separatemetricsonly_df %>%
  ggplot(aes(x = Column, y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Count of avian articles by composition metric", x = "Composition metric", y = "Count of articles")
print(aviancounts_barplot)

#bar plots with percent of total *** SOMETHING IS WRONG WITH THE PERCENT CALC
carbonpct_barplot <- counts.carbon.compmetricsonly_df %>%
  ggplot(aes(x = Column, y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Percent of carbon articles by composition metric (n=109)", x = "Composition metric", y = "Percent of articles")
print(carbonpct_barplot)

avianpct_barplot  <- counts.avian.separatemetricsonly_df %>%
  ggplot(aes(x = Column, y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Percent of avian articles by composition metric (n=158)", x = "Composition metric", y = "Percent of articles")
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






