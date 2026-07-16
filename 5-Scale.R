#Corey Bassett
#Script for summary tables of UF composition metrics by urban scale

#packages and cleaned data (sources 0-packages.R internally)
source('1-UFcompmetrics-cleaning.R')



#metrics by scale of study (avian)
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

#summary table: total metric uses per urban scale
avian.urbanscale <- avian.urbscalecompmetricsonly %>%
  group_by(Urb.scale) %>%
  summarize(across(everything(),sum)) %>%
  mutate(count.metricsperurbscale = rowSums(across(`age`:`vine layer`), na.rm = TRUE))

avian.urbscalecounts
avian.urbanscale


#metrics by scale of study (carbon)
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

#summary table: total metric uses per urban scale (drop N/A column)
carbon.urbanscale <- carbon.urbscalecompmetricsonly %>%
  group_by(Urb.scale) %>%
  summarize(across(everything(),sum)) %>%
  subset(select = -c(`N/A`)) %>%
  mutate(count.metricsperurbscale = rowSums(across(`age`:`vertical layer assessment`)))

carbon.urbscalecounts
carbon.urbanscale
