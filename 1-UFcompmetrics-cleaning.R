#Corey Bassett
#Script for analysis of UF composition metrics

#packagesneeded
source('0-packages.R')

list.compmetrics <- read.xlsx("ListofUFCompMetrics.xlsx")

#======CARBON======

#loading carbon data and formatting
carbon.data<- read.csv("TEST_UFcompmetrics_carbon_19Nov24.csv")

#cut down to only necessary columns for figures (so far..)
carbon.meta <- carbon.data[ ,c("Full.citation", "Title","Year", "Journal", "Publication.Type.", "Country.of.First.Author",
                               "Study.Country", "Urb.scale", "Year.start", "Year.end", "Comparator", 
                               "Forest.comp", "Rec.included","Rec1", "Rec2", "Rec3", "Carbon.metric","Composition.metric","Initials")]

#remove white space (leading and trailing zeros)
carbon.meta<- carbon.meta %>% 
  mutate(across(where(is.character), str_trim))

#replace empty cells with N/A
carbon.meta <- replace(carbon.meta, carbon.meta=='', NA) 

#separate composition metric column into multiple columns with one metric each
carbon.separatemetrics <- cSplit_e(data = carbon.meta, split.col = "Composition.metric", sep=",", type = "character") 

#select only comp metrics
carbon.compmetricsonly <- select(carbon.separatemetrics, contains("Composition.metric_"))

#replace NA with 0
carbon.compmetricsonly[is.na(carbon.compmetricsonly)] <- 0

# Count the number of 1s in carbon.compmetricsonly
counts.carbon.compmetricsonly <- colSums(carbon.compmetricsonly == 1)
sort.int(counts.carbon.compmetricsonly, decreasing = FALSE, na.last = NA)

# Convert counts to a data frame
counts.carbon.compmetricsonly_df <- data.frame(
  Column = names(counts.carbon.compmetricsonly),
  Count = counts.carbon.compmetricsonly)

#sort
counts.carbon.compmetricsonly_df$Column <- 
  factor(counts.carbon.compmetricsonly_df$Column, 
         levels = counts.carbon.compmetricsonly_df$Column[order(-counts.carbon.compmetricsonly_df$Count)])


# Create the bar chart

carboncounts_barplot <-
  ggplot(counts.carbon.compmetricsonly_df, aes(x = Column, y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Count", x = "Column", y = "Count")








#===AVIAN===

#loading avian data and formatting
avian.data<- read.csv("TEST_UFcompmetrics_avian_21Nov24.csv")

#cut down to only necessary columns for figures (so far..)
avian.meta <- avian.data[,c("Rayyan.ID",
                            "Citation", 
                            "Country.Auth", 
                            "Title", 
                            "Journal", 
                            "Study.country", 
                            "Urb.scale", 
                            "Forest.comp", 
                            "Scale.meas", 
                            "Bird.domainraw",
                            "Bird.category",
                            "Category.multi",
                            "Metrics",
                            "Initials")]
#REPLACE METRICS WITH Composition metric when using full dataset, not TEST


#remove white space (leading and trailing zeros)
avian.meta<- avian.meta %>% 
  mutate(across(where(is.character), str_trim))

#replace empty cells with N/A
avian.meta <- replace(avian.meta, avian.meta=='', NA) 

#separate composition metric column into multiple columns with one metric each
avian.separatemetrics <- cSplit_e(data = avian.meta, split.col = "Metrics", sep=",", type = "character") 

#select only comp metrics
avian.separatemetricsonly <- select(avian.separatemetrics, contains("Metrics_"))

#replace NA with 0
avian.separatemetricsonly[is.na(avian.separatemetricsonly)] <- 0

# Count the number of 1s in avian.separatemetricsonly
counts.avian.separatemetricsonly <- colSums(avian.separatemetricsonly == 1)
sort.int(counts.avian.separatemetricsonly, decreasing = FALSE, na.last = NA)

# Convert counts to a data frame
counts.avian.separatemetricsonly_df <- data.frame(
  Column = names(counts.avian.separatemetricsonly),
  Count = counts.avian.separatemetricsonly)

#sort
counts.avian.separatemetricsonly_df$Column <- 
  factor(counts.avian.separatemetricsonly_df$Column, 
         levels = counts.avian.separatemetricsonly_df$Column[order(-counts.avian.separatemetricsonly_df$Count)])


# Create the bar chart

aviancounts_barplot <-
  ggplot(counts.avian.separatemetricsonly_df, aes(x = Column, y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Count", x = "Column", y = "Count")


