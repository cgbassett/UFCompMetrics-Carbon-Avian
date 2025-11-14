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

#bar plots with percent of total *** SOMETHING IS WRONG WITH THE PERCENT CALC

pct.carbon.compmetricsonly_df <- mutate(counts.carbon.compmetricsonly_df, Percent = (counts.carbon.compmetricsonly_df$Count / 109) *100)

carbonpct_barplot <- pct.carbon.compmetricsonly_df %>%
  ggplot(aes(x = Column, y = Percent)) +
  geom_bar(stat = "identity", fill = "#FDE725FF") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  labs(title = "Percent of carbon articles by composition metric (n=109)", x = "Composition metric", y = "Percent of articles") +
  theme_minimal()
print(carbonpct_barplot)

pct.avian.compmetricsonly_df <- mutate(counts.avian.compmetricsonly_df, Percent = (counts.avian.compmetricsonly_df$Count / 158) *100)

avianpct_barplot  <- pct.avian.compmetricsonly_df %>%
  ggplot(aes(x = Column, y = Percent)) +
  geom_bar(stat = "identity", fill = "#404788FF") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  labs(title = "Percent of avian articles by composition metric (n=158)", x = "Composition metric", y = "Percent of articles") +
  theme_minimal()
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


#Count of metrics per article

avian.separatemetrics_ctperarticle <- avian.separatemetrics %>%
  mutate(count.metricsperarticle = rowSums(across(`Composition.metric_age`:`Composition.metric_vine layer`), na.rm = TRUE))

summary(avian.separatemetrics_ctperarticle$count.metricsperarticle)

avian.ctsparticle.hist <-
  ggplot(avian.separatemetrics_ctperarticle, aes(x = count.metricsperarticle)) +
  geom_histogram(binwidth = 1, fill = "#404788FF", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Count of Metrics per Article (Avian, n=158)", x = "Count of Metrics", y = "Number of Articles") +
  scale_x_continuous(breaks = seq(0, max(avian.separatemetrics_ctperarticle$count.metricsperarticle), by = 1)) +
  scale_y_continuous(limits = c(0, 35)) +
  theme_minimal()
print(avian.ctsparticle.hist)


carbon.separatemetrics_ctperarticle <- carbon.separatemetrics %>%
  mutate(count.metricsperarticle = rowSums(across(`Composition.metric_age`:`Composition.metric_vertical layer assessment`), na.rm = TRUE))

summary(carbon.separatemetrics_ctperarticle$count.metricsperarticle)

carbon.ctsparticle.hist <-
ggplot(carbon.separatemetrics_ctperarticle, aes(x = count.metricsperarticle)) +
  geom_histogram(binwidth = 1, fill = "#FDE725FF", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, max(carbon.separatemetrics_ctperarticle$count.metricsperarticle), by = 1)) +
  scale_y_continuous(limits = c(0, 35)) +
  labs(title = "Histogram of Count of Metrics per Article (Carbon, n=109)", x = "Count of Metrics", y = "Number of Articles") +
  theme_minimal()
print(carbon.ctsparticle.hist)

#metrics by scale of study
avian.urbscalecompmetricsonly <- avian.separatemetrics %>%
  select(Urb.scale, contains("Composition.metric_"))

    #remove "Composition.metric_"
    for ( col in 1:ncol(avian.urbscalecompmetricsonly)){
      colnames(avian.urbscalecompmetricsonly)[col] <-  
        sub("Composition.metric_", "", colnames(avian.urbscalecompmetricsonly)[col])
    }

    #replace NA with 0
    avian.urbscalecompmetricsonly[is.na(avian.urbscalecompmetricsonly)] <- 0
    
    #count frequency of articles by each urb.scale
    avian.urbscalecounts <- avian.urbscalecompmetricsonly %>% count(Urb.scale)
    avian.urbscalecounts <- avian.urbscalecounts %>%
                            mutate(Percent = (n/158)*100)
    
    #summary table
    avian.urbanscale <- avian.urbscalecompmetricsonly %>%
                        group_by(Urb.scale) %>%
                        summarize(across(everything(),sum)) %>%
                        mutate(count.metricsperurbscale = rowSums(across(`age`:`vine layer`), na.rm = TRUE))

    #bubble chart - not great
    ggplot(avian.urbanscale, aes(x = Urb.scale, y = select(`age`:`vine layer` )) +
      geom_point(alpha = 0.5) +
      labs(title = "Bubble Chart of Urban Scale by Forest Comp Metric (Avian)", x = "Urban Scale", y = "Forest Composition Metric") +
      theme_minimal() +
      scale_size_continuous(range = c(3, 15))
    
        
carbon.urbscalecompmetricsonly <- carbon.separatemetrics %>%
   select(Urb.scale, contains("Composition.metric_"))
    
    #remove "Composition.metric_"
    for ( col in 1:ncol(carbon.urbscalecompmetricsonly)){
      colnames(carbon.urbscalecompmetricsonly)[col] <-  
        sub("Composition.metric_", "", colnames(carbon.urbscalecompmetricsonly)[col])
    }
    
    #replace NA with 0
    carbon.urbscalecompmetricsonly[is.na(carbon.urbscalecompmetricsonly)] <- 0
    
    #count frequency of articles by each urb.scale
    carbon.urbscalecounts <- carbon.urbscalecompmetricsonly %>% count(Urb.scale)
    carbon.urbscalecounts <- carbon.urbscalecounts %>%
      mutate(Percent = (n/109)*100)
    
    #summary table
    carbon.urbanscale <- carbon.urbscalecompmetricsonly %>%
      group_by(Urb.scale) %>%
      summarize(across(everything(),sum)) %>%
      subset(select = -c(`N/A`)) %>%
      mutate(count.metricsperurbscale = rowSums(across(`age`:`vertical layer assessment`)))
    
    carbon.urbanscale <- melt(carbon.urbanscale, id.vars = "Urb.scale", variable.name = "Metric", value.name = "Value",)
    
    #bubble chart - is this even useful?
    ggplot(carbon.urbanscale, aes(x = Urb.scale, y = Metric, size = Value )) +
             geom_point(alpha = 0.5) +
             labs(title = "Bubble Chart of Urban Scale by Forest Comp Metric (Carbon)", x = "Urban Scale", y = "Forest Composition Metric") +
             theme_minimal() +
             scale_size_continuous(range = c(3, 15))



