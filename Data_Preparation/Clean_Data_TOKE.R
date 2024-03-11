#### Library
library(dplyr)

####
# Generate 'cleaned dataset' ----
# Start with BBR and load in data and remove unneccesary variables and transform variables to appropriate. 
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Bygning.rdata")
  ## Select variables of interest ----
Var <- c("byg039BygningensSamledeBoligAreal", "byg021BygningensAnvendelse", "byg044ArealIndbyggetUdhus", "byg026Opførelsesår", 
         "byg032YdervæggensMateriale", "byg033Tagdækningsmateriale", "byg056Varmeinstallation", "byg027OmTilbygningsår",
         "grund", "husnummer")

BBR_Bygning_Subset <- subset(BBR_Bygning, select = Var)

      ### Rename variables ----
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Size = byg039BygningensSamledeBoligAreal)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Car_Park = byg021BygningensAnvendelse)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Outbuilding = byg044ArealIndbyggetUdhus)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Opførelsesår = byg026Opførelsesår)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Ydervægge_Materiale = byg032YdervæggensMateriale)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Tagdækning_Materiale = byg033Tagdækningsmateriale)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Varmeinstallation = byg056Varmeinstallation)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Ombygning = byg027OmTilbygningsår)

      ### Create variables  ----
          #### Carport ----
          BBR_Bygning_Subset$Car_Park <- ifelse(BBR_Bygning_Subset$CARPARK == 920, 1, 0)
          
          #### Car garage double ----
          BBR_Bygning_Subset$Car_Park <- ifelse(BBR_Bygning_Subset$CARPARK == 910, 1, 0)
          
          #### Built ----
          BBR_Bygning_Subset$'<1940' <- ifelse(BBR_Bygning_Subset$Opførelsesår < 1940, 1, 0)
          BBR_Bygning_Subset$'1940-1950' <- ifelse(BBR_Bygning_Subset$Opførelsesår < 1950 & 
                                                     BBR_Bygning_Subset$Opførelsesår > 1940 , 1, 0)
          BBR_Bygning_Subset$'1950-1960' <- ifelse(BBR_Bygning_Subset$Opførelsesår < 1960 & 
                                                     BBR_Bygning_Subset$Opførelsesår > 1950 , 1, 0)
          BBR_Bygning_Subset$'1960-1970' <- ifelse(BBR_Bygning_Subset$Opførelsesår < 1970 & 
                                                     BBR_Bygning_Subset$Opførelsesår > 1960 , 1, 0)
          BBR_Bygning_Subset$'1970-1980' <- ifelse(BBR_Bygning_Subset$Opførelsesår < 1980 & 
                                                     BBR_Bygning_Subset$Opførelsesår > 1970 , 1, 0)
          BBR_Bygning_Subset$'1980-1990' <- ifelse(BBR_Bygning_Subset$Opførelsesår < 1990 & 
                                                     BBR_Bygning_Subset$Opførelsesår > 1980 , 1, 0)
          BBR_Bygning_Subset$'1990-2000' <- ifelse(BBR_Bygning_Subset$Opførelsesår < 2000 & 
                                                     BBR_Bygning_Subset$Opførelsesår > 1990 , 1, 0)
          BBR_Bygning_Subset$'2000-2010' <- ifelse(BBR_Bygning_Subset$Opførelsesår < 2010 & 
                                                     BBR_Bygning_Subset$Opførelsesår > 2000 , 1, 0)
          BBR_Bygning_Subset$'2010' <- ifelse(BBR_Bygning_Subset$Opførelsesår > 2010, 1, 0)
          
          #### Materials ----
          BBR_Bygning_Subset$Brick <- ifelse(BBR_Bygning_Subset$Ydervægge_Materiale == 1, 1, 0)
          BBR_Bygning_Subset$Lightweightconcrete <- ifelse(BBR_Bygning_Subset$Ydervægge_Materiale == 2, 1, 0)
          BBR_Bygning_Subset$Wood <- ifelse(BBR_Bygning_Subset$Ydervægge_Materiale == 5, 1, 0)
          
          #### Roof ----
          BBR_Bygning_Subset$Tile <- ifelse(BBR_Bygning_Subset$Tagdækning_Materiale == 5, 1, 0)
          BBR_Bygning_Subset$Thatched <- ifelse(BBR_Bygning_Subset$Tagdækning_Materiale == 7, 1, 0)
          BBR_Bygning_Subset$Fiberasbetos <- ifelse(BBR_Bygning_Subset$Tagdækning_Materiale == 3, 1, 0)
        
          #### Heating ----
          BBR_Bygning_Subset$Electricheating <- ifelse(BBR_Bygning_Subset$Varmeinstallation == 5, 1, 0)
          BBR_Bygning_Subset$Centralheating <- ifelse(BBR_Bygning_Subset$Varmeinstallation == 7, 1, 0)
          BBR_Bygning_Subset$Districtheating <- ifelse(BBR_Bygning_Subset$Varmeinstallation == 3, 1, 0)
          
          
          #### Renovation ----
          BBR_Bygning_Subset$'Renovated_1940-1950' <- ifelse(BBR_Bygning_Subset$Ombygning < 1950 & 
                                                            BBR_Bygning_Subset$Ombygning > 1940 , 1, 0)
          BBR_Bygning_Subset$'Renovated_1950-1960' <- ifelse(BBR_Bygning_Subset$Ombygning < 1960 & 
                                                            BBR_Bygning_Subset$Ombygning > 1950 , 1, 0)
          BBR_Bygning_Subset$'Renovated_1960-1970' <- ifelse(BBR_Bygning_Subset$Ombygning < 1970 & 
                                                            BBR_Bygning_Subset$Ombygning > 1960 , 1, 0)
          BBR_Bygning_Subset$'Renovated_1970-1980' <- ifelse(BBR_Bygning_Subset$Ombygning < 1980 & 
                                                            BBR_Bygning_Subset$Ombygning > 1970 , 1, 0)
          BBR_Bygning_Subset$'Renovated_1980-1990' <- ifelse(BBR_Bygning_Subset$Ombygning < 1990 & 
                                                            BBR_Bygning_Subset$Ombygning > 1980 , 1, 0)
          
          # 
                    
# Cleaning data ----
# This script is made for cleaning the data received from Toke. I imagine to delete columns, outliers, etc. 
# How to clean data # https://www.r-bloggers.com/2021/04/how-to-clean-the-datasets-in-r/


  