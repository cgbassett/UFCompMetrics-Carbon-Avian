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
  
