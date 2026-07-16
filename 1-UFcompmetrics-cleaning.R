#Corinne Bassett
#Script for analysis of UF composition metrics

#packagesneeded
source('0-packages.R')

data_dir <- Sys.getenv("DATA_DIR")

list.compmetrics <- read.xlsx(file.path(data_dir, "ListofUFCompMetrics.xlsx"))

#======CARBON======

#loading carbon data and formatting
carbon.data<- read.csv(file.path(data_dir, "Carbon_UFCompMetricData_21Nov24.csv"))

#cut down to only necessary columns for figures
carbon.meta <- carbon.data[ ,c("Rayyan.ID", "Full.citation", "Title","Year", "Journal", "Publication.Type.", "Country.of.First.Author",
                               "Study.Country", "Urb.scale", "Year.start", "Year.end", "Comparator", 
                               "Forest.comp", "Rec.included","Rec1", "Rec2", "Rec3", "Carbon.metric","Composition.metric")]

#remove white space (leading and trailing zeros)
carbon.meta <- carbon.meta %>% 
  mutate(across(where(is.character), str_trim))

#replace empty cells with N/A
carbon.meta <- replace(carbon.meta, carbon.meta=='', NA) 

#separate composition metric column into multiple columns with one metric each
carbon.separatemetrics <- cSplit_e(data = carbon.meta, split.col = "Composition.metric", sep=",", type = "character") 

#Carbon N/A articles to remove are Rayyan IDs 879385490 and 879386384

carbonNAarticles <- c(879385490, 879386384)

carbon.separatemetrics <- carbon.separatemetrics %>%
  filter(!Rayyan.ID %in% carbonNAarticles)

#select only comp metrics
carbon.compmetricsonly <- select(carbon.separatemetrics, contains("Composition.metric_"))

#remove "Composition.metric_"
for ( col in 1:ncol(carbon.compmetricsonly)){
  colnames(carbon.compmetricsonly)[col] <-  
    sub("Composition.metric_", "", colnames(carbon.compmetricsonly)[col])
}

#replace NA with 0
carbon.compmetricsonly[is.na(carbon.compmetricsonly)] <- 0

# Count the number of 1s in carbon.compmetricsonly
counts.carbon.compmetricsonly <- colSums(carbon.compmetricsonly == 1)
sort.int(counts.carbon.compmetricsonly, decreasing = FALSE, na.last = NA)

# Convert counts to a data frame
counts.carbon.compmetricsonly_df <- data.frame(
  Column = names(counts.carbon.compmetricsonly),
  Count = counts.carbon.compmetricsonly)

#Remove N/A row
counts.carbon.compmetricsonly_df <- counts.carbon.compmetricsonly_df %>% filter(!(Column == "N/A"))

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
  labs(title = "Count of carbon articles by composition metric (n=109)", x = "Composition metric", y = "Count of articles")

print(carboncounts_barplot)








#===AVIAN===

#loading avian data and formatting
avian.data<- read.csv(file.path(data_dir, "Avian_UFCompMetricData_21Nov24.csv"))

#cut down to only necessary columns for figures
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
                            "Composition.metric"
                            )]

#remove white space (leading and trailing zeros)
avian.meta<- avian.meta %>% 
  mutate(across(where(is.character), str_trim))

#replace empty cells with N/A
avian.meta <- replace(avian.meta, avian.meta=='', NA) 

#separate composition metric column into multiple columns with one metric each
avian.separatemetrics <- cSplit_e(data = avian.meta, split.col = "Composition.metric", sep=",", type = "character") 

#Avian N/A articles to remove are Rayyan IDs 364026331 and 364026955

avianNAarticles <- c(364026331, 364026955)

avian.separatemetrics <- avian.separatemetrics %>%
  filter(!Rayyan.ID %in% avianNAarticles)

#select only comp metrics
avian.compmetricsonly <- select(avian.separatemetrics, contains("Composition.metric_"))

#remove "Composition.metric_"
for ( col in 1:ncol(avian.compmetricsonly)){
  colnames(avian.compmetricsonly)[col] <-  
    sub("Composition.metric_", "", colnames(avian.compmetricsonly)[col])
}

#replace NA with 0
avian.compmetricsonly[is.na(avian.compmetricsonly)] <- 0

# Count the number of 1s in avian.separatemetricsonly
counts.avian.compmetricsonly <- colSums(avian.compmetricsonly == 1)
sort.int(counts.avian.compmetricsonly, decreasing = FALSE, na.last = NA)

# Convert counts to a data frame
counts.avian.compmetricsonly_df <- data.frame(
  Column = names(counts.avian.compmetricsonly),
  Count = counts.avian.compmetricsonly)

#Remove N/A row
counts.avian.compmetricsonly_df <- counts.avian.compmetricsonly_df %>% filter(!(Column == "N/A"))


#sort
counts.avian.compmetricsonly_df$Column <- 
  factor(counts.avian.compmetricsonly_df$Column, 
         levels = counts.avian.compmetricsonly_df$Column[order(-counts.avian.compmetricsonly_df$Count)])

# Create the bar chart

aviancounts_barplot <-
  ggplot(counts.avian.compmetricsonly_df, aes(x = Column, y = Count)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Count of avian articles by composition metric (n=158)", x = "Composition metric", y = "Count of articles")

print(aviancounts_barplot)




