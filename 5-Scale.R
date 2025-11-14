#Corey Bassett
#Script for visualizations of UF composition metrics by urban scale

#packagesneeded
source('0-packages.R')



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
       