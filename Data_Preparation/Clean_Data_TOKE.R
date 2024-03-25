#### Library
library(dplyr)
library(tidyr)

####
# Generate 'cleaned dataset' ----
# Start with BBR and load in data and remove unneccesary variables and transform variables to appropriate. 
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Bygning.rdata")
  ## Select variables of interest ----
      Var <- c("bygning_id","byg039BygningensSamledeBoligAreal", "byg021BygningensAnvendelse", "byg044ArealIndbyggetUdhus", "byg026Opførelsesår", 
         "byg032YdervæggensMateriale", "byg033Tagdækningsmateriale", "byg056Varmeinstallation", "byg027OmTilbygningsår",
         "grund", "husnummer","Geometri_EPSG_25832", "adr_etrs89_oest", "adr_etrs89_nord")

      BBR_Bygning_Subset <- subset(BBR_Bygning, select = Var)
  
    ## Rename variables ----
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Size = byg039BygningensSamledeBoligAreal)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Bygninganvendelse = byg021BygningensAnvendelse)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Outbuilding = byg044ArealIndbyggetUdhus)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Opførelsesår = byg026Opførelsesår)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Ydervægge_Materiale = byg032YdervæggensMateriale)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Tagdækning_Materiale = byg033Tagdækningsmateriale)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Varmeinstallation = byg056Varmeinstallation)
          BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Ombygning = byg027OmTilbygningsår)

    ## Reduce observations by deselecting corporate, farming, etc. ----
    BBR_Bygning_Subset <- subset(BBR_Bygning_Subset, Bygninganvendelse == 110 | Bygninganvendelse == 120 |
                                 Bygninganvendelse == 121 | Bygninganvendelse == 122 | Bygninganvendelse == 130 |
                                 Bygninganvendelse == 131 | Bygninganvendelse == 132 | Bygninganvendelse == 140 |
                                 Bygninganvendelse == 190 | Bygninganvendelse == 510 | Bygninganvendelse == 910 |
                                 Bygninganvendelse == 920 | Bygninganvendelse == 930 | Bygninganvendelse == 940 |
                                 Bygninganvendelse == 950 | Bygninganvendelse == 960)    
                              
          
    ## Create variables  ----
          ### Carport ----
          BBR_Bygning_Subset$Car_Park <- ifelse(BBR_Bygning_Subset$Bygninganvendelse == 920, 1, 0)
          
          ### Car garage ----
          BBR_Bygning_Subset$Garage <- ifelse(BBR_Bygning_Subset$Bygninganvendelse == 910, 1, 0)
          
          ### Terraced House 
          BBR_Bygning_Subset$TerracedHouse <- ifelse(BBR_Bygning_Subset$Bygninganvendelse == 130 |  
                                                       BBR_Bygning_Subset$Bygninganvendelse == 131, 1, 0)
          
          ### Built ----
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
          
          ### Materials ----
          BBR_Bygning_Subset$Brick <- ifelse(BBR_Bygning_Subset$Ydervægge_Materiale == 1, 1, 0)
          BBR_Bygning_Subset$Lightweightconcrete <- ifelse(BBR_Bygning_Subset$Ydervægge_Materiale == 2, 1, 0)
          BBR_Bygning_Subset$Wood <- ifelse(BBR_Bygning_Subset$Ydervægge_Materiale == 5, 1, 0)
          
          ### Roof ----
          BBR_Bygning_Subset$Tile <- ifelse(BBR_Bygning_Subset$Tagdækning_Materiale == 5, 1, 0)
          BBR_Bygning_Subset$Thatched <- ifelse(BBR_Bygning_Subset$Tagdækning_Materiale == 7, 1, 0)
          BBR_Bygning_Subset$Fiberasbetos <- ifelse(BBR_Bygning_Subset$Tagdækning_Materiale == 3, 1, 0)
        
          ### Heating ----
          BBR_Bygning_Subset$Electricheating <- ifelse(BBR_Bygning_Subset$Varmeinstallation == 5, 1, 0)
          BBR_Bygning_Subset$Centralheating <- ifelse(BBR_Bygning_Subset$Varmeinstallation == 7, 1, 0)
          BBR_Bygning_Subset$Districtheating <- ifelse(BBR_Bygning_Subset$Varmeinstallation == 3, 1, 0)
          
          
          ### Renovation ----
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
          save(BBR_Bygning_Subset, file ="~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Subset.Rdata")
          
    ## Identification variable merging ----
       # Find the key to merge between data sets for achieving the variables of interest.
       # BBR_Bygning { Bygningsid, husnummer, grund }
       # BBR_Enhed {Enhed_id, bygning, adresse, adresseidentificerer} 
       # DAWA_Adresse {Adresseid, Jordstykke_Bfenummer, adresse}
       # EJF_Ejerskifte {objectid, id_lokalId}
       # EJF_Handelsoplysning { objectid (Bfenummer), id_lokalId }
       # MAT_Jordstykke {bfe_ejendomsnummer, jordstykke_id}
       # Trades_1992_2021 from Carsten {}
          
      ### Try to merge by 'adresse' for BBR_Enhed and Handel_BFE_Adresse ----
          load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Carsten Bertram/SpecialeTrades_1992-2021.RData")
          load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Enhed.rdata")
          BBR_Enhed_Trade <- merge(BBR_Enhed, trades_1992_2021, by.x = "adresseIdentificerer", by.y = "addressID")
          #Truncate data to sales from 2010
          BBR_Enhed_Trade <- subset(BBR_Enhed_Trade, BBR_Enhed_Trade$dato > "2010-01-01")
           # save(BBR_Enhed_Trade, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Enhed_Trade.Rdata")
          load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Enhed_Trade.Rdata")
        
              ##### Keep useful variables ----
              Var_to_keep_Enhed_Trade <- c("vejnavn", "husnr","adresseIdentificerer", "enhed_id", "kommunekode", "enh020EnhedensAnvendelse",  
                                           "enh023Boligtype", "enh031AntalVærelser", "enh032Toiletforhold", 
                                           "Heating1","Heating2", "Heating3", "Roof1", "Roof2", "Roof3", "etage", "opgang",
                                           "bygning", "vejnavn", "husnr", "postnr", "m2", "dato", "entryAddressID", "nominal_price", 
                                           "year")
              
              BBR_Enhed_Trade <- subset(BBR_Enhed_Trade, select = Var_to_keep_Enhed_Trade)
              
      ### Merge with building ----   
          CLEAN_DATA <- merge(BBR_Bygning_Subset,BBR_Enhed_Trade, by.x = "bygning_id", by.y = "bygning" )    
            # save(CLEAN_DATA, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/CLEAN_DATA.Rdata")
            
          # Assumption: Variables as when it has been renovated, NA changed to '0'
              CLEAN_DATA <- CLEAN_DATA %>% dplyr::mutate(`Renovated_1940-1950` = ifelse(is.na(`Renovated_1940-1950`), 0, `Renovated_1940-1950`),
                                                 `Renovated_1950-1960` = ifelse(is.na(`Renovated_1950-1960`), 0, `Renovated_1950-1960`),
                                                 `Renovated_1960-1970` = ifelse(is.na(`Renovated_1960-1970`), 0, `Renovated_1960-1970`),
                                                 `Renovated_1970-1980` = ifelse(is.na(`Renovated_1970-1980`), 0, `Renovated_1970-1980`),
                                                 `Renovated_1980-1990` = ifelse(is.na(`Renovated_1980-1990`), 0, `Renovated_1980-1990`))
              # save(CLEAN_DATA, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/CLEAN_DATA.Rdata")  
              load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/CLEAN_DATA.Rdata")
              
          # Reduce dimensions in clean data by merging flooded houses and keeping neighbors. But zip codes with no flooding are removed. 
            load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Skader.Rdata")
            Skader <- Clean_Flood_Data
            rm(Clean_Flood_Data)
            # Adresse need to be parsed to be merged later (only identification)
            Skader$adresse1 <- gsub(",.*", "", Skader$Adresse)
            Skader$Vejnr <- stringr::str_extract(Skader$adresse1, "\\d+[A-Za-z]*(-\\d+)?")
            Skader$Vejnavn <- stringr::str_extract(Skader$adresse1, ".*(?=\\s\\d+[A-Za-z]*(-\\d+)?)")

            
          # Assume entities in in building are all affected by flooding, so only merged by housenumber
          Clean_Flood_Data <- merge(CLEAN_DATA, Skader, by.x = c("vejnavn", "husnr"), by.y = c("Vejnavn", "Vejnr"), all.x = TRUE)
          # Assuming df_big is your larger data frame and df_small is your smaller data frame
          # It is assumed that entities within a building is affected on price, such as apartments. 
          
          
          save(Clean_Flood_Data, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Clean_Flood_Data.Rdata")
          load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Skader.Rdata")
         
          
          ## Merge rain data and flooding due to streams ----
          
              ### rename variables from both data frames to be common ----
          load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Skader.Rdata")
                  Clean_Flood_Data <- dplyr::rename(Clean_Flood_Data, size = Size, 
                                                    outhouse = Outbuilding, year_of_built = Opførelsesår, 
                                                    fibercement_asbestos_roof = Fiberasbetos, thatch_roof = Thatched,
                                                    electric_heating = Electricheating, central_heating = Centralheating, 
                                                    toilets = enh032Toiletforhold, rooms = enh031AntalVærelser, sales_date = dato)
                  save(Clean_Flood_Data, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/CLEAN_FLOOD_DATA_RENAMED_VARIABLES.Rdata")
                                                    
              
                  load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Precipitation_Subset.Rdata")
                  Precipitation_subset <- dplyr::rename(Precipitation_subset, adr_etrs89_oest = x, adr_etrs89_nord = y,
                                                        Car_Park = car_park, Wood = wood, bygning_id = bygning)
                  
                  save(Precipitation_subset, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Precipitation_subset_RENAMED_VARIABLES.Rdata")
                  
                  
            ### Merge on common variables 
                  #load enheder to map on buildings
                  load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Enhed.Rdata")
                  BBR_Enhed <- subset(BBR_Enhed, select = c("enhed_id", "bygning"))
                  Precipitation_subset <- merge(BBR_Enhed, Precipitation_subset, by.x = "enhed_id", by.y = "Enhed_id")
                  
                  ALL_DATA_COMBINED <- merge(Clean_Flood_Data, Precipitation_subset, by = "bygning_id", all.x = TRUE)
                  # Unfortunately i have to clean up variables again due to duplicates
                  # vejnavn.x
                  ALL_DATA_COMBINED$vejnavn <- ifelse(is.na(ALL_DATA_COMBINED$vejnavn.x), 
                                              ALL_DATA_COMBINED$vejnavn.y, ALL_DATA_COMBINED$vejnavn.x)
                  # husnr
                  ALL_DATA_COMBINED$husnr <- ifelse(is.na(ALL_DATA_COMBINED$husnr.x), 
                                              ALL_DATA_COMBINED$husnr.y, ALL_DATA_COMBINED$husnr.x)
                  # size
                  ALL_DATA_COMBINED$size <- ifelse(is.na(ALL_DATA_COMBINED$size.x), 
                                              ALL_DATA_COMBINED$size.y, ALL_DATA_COMBINED$size.x)
                  # outhouse
                  ALL_DATA_COMBINED$outhouse <- ifelse(is.na(ALL_DATA_COMBINED$outhouse.x), 
                                              ALL_DATA_COMBINED$outhouse.y, ALL_DATA_COMBINED$outhouse.x)
                  # terraced
                  ALL_DATA_COMBINED$TerracedHouse <- ifelse(is.na(ALL_DATA_COMBINED$TerracedHouse.x), 
                                                       ALL_DATA_COMBINED$TerracedHouse.y, ALL_DATA_COMBINED$TerracedHouse.x)
                  # year_of_built
                  ALL_DATA_COMBINED$year_of_built <- ifelse(is.na(ALL_DATA_COMBINED$year_of_built.x), 
                                              ALL_DATA_COMBINED$year_of_built.y, ALL_DATA_COMBINED$year_of_built.x)
                  # Geometri_EPSG_25832
                  ALL_DATA_COMBINED$Geometri_EPSG_25832 <- ifelse(is.na(ALL_DATA_COMBINED$Geometri_EPSG_25832.x), 
                                              ALL_DATA_COMBINED$Geometri_EPSG_25832.y, ALL_DATA_COMBINED$Geometri_EPSG_25832.x)
                  # adr_etrs89_oest
                  ALL_DATA_COMBINED$adr_etrs89_oest <- ifelse(is.na(ALL_DATA_COMBINED$adr_etrs89_oest.x), 
                                              ALL_DATA_COMBINED$adr_etrs89_oest.y, ALL_DATA_COMBINED$adr_etrs89_oest.x)
                  # adr_etrs89_nord
                  ALL_DATA_COMBINED$adr_etrs89_nord <- ifelse(is.na(ALL_DATA_COMBINED$adr_etrs89_nord.x), 
                                                                ALL_DATA_COMBINED$adr_etrs89_nord.y, ALL_DATA_COMBINED$adr_etrs89_nord.x)
                  # Car_Park
                  ALL_DATA_COMBINED$Car_Park <- ifelse(is.na(ALL_DATA_COMBINED$Car_Park.x), 
                                                                ALL_DATA_COMBINED$Car_Park.y, ALL_DATA_COMBINED$Car_Park.x) 
                  # <1940
                  ALL_DATA_COMBINED$`<1940` <- ifelse(is.na(ALL_DATA_COMBINED$`<1940.x`), 
                                                         ALL_DATA_COMBINED$`<1940.y`, ALL_DATA_COMBINED$`<1940.x`) 
                  # 1950-1960
                  ALL_DATA_COMBINED$`1950-1960` <- ifelse(is.na(ALL_DATA_COMBINED$`1950-1960.x`), 
                                                        ALL_DATA_COMBINED$`1950-1960.y`, ALL_DATA_COMBINED$`1950-1960.x`)
                  # 1960-1970
                  ALL_DATA_COMBINED$`1960-1970` <- ifelse(is.na(ALL_DATA_COMBINED$`1960-1970.x`), 
                                                          ALL_DATA_COMBINED$`1960-1970.y`, ALL_DATA_COMBINED$`1960-1970.x`)
                  # 1970-1980
                  ALL_DATA_COMBINED$`1970-1980` <- ifelse(is.na(ALL_DATA_COMBINED$`1970-1980.x`), 
                                                          ALL_DATA_COMBINED$`1970-1980.y`, ALL_DATA_COMBINED$`1970-1980.x`)
                  # 1980-1990
                  ALL_DATA_COMBINED$`1980-1990` <- ifelse(is.na(ALL_DATA_COMBINED$`1980-1990.x`), 
                                                          ALL_DATA_COMBINED$`1980-1990.y`, ALL_DATA_COMBINED$`1980-1990.x`)
                  # 1990-2000
                  ALL_DATA_COMBINED$`1990-2000` <- ifelse(is.na(ALL_DATA_COMBINED$`1990-2000.x`), 
                                                          ALL_DATA_COMBINED$`1990-2000.y`, ALL_DATA_COMBINED$`1990-2000.x`)
                  # 2000-2010
                  ALL_DATA_COMBINED$`2000-2010` <- ifelse(is.na(ALL_DATA_COMBINED$`2000-2010.x`), 
                                                          ALL_DATA_COMBINED$`2000-2010.y`, ALL_DATA_COMBINED$`2000-2010.x`)
                  # 2010
                  ALL_DATA_COMBINED$`2010` <- ifelse(is.na(ALL_DATA_COMBINED$`2010.x`), 
                                                          ALL_DATA_COMBINED$`2010.y`, ALL_DATA_COMBINED$`2010.x`)
                  # Wood
                  ALL_DATA_COMBINED$Wood <- ifelse(is.na(ALL_DATA_COMBINED$Wood.x), 
                                                       ALL_DATA_COMBINED$Wood.y, ALL_DATA_COMBINED$Wood.y)
                  # Thach roof 
                  ALL_DATA_COMBINED$thatch_roof <- ifelse(is.na(ALL_DATA_COMBINED$thatch_roof.x), 
                                                     ALL_DATA_COMBINED$thatch_roof.y, ALL_DATA_COMBINED$thatch_roof.y)
                  # fibercement_asbestos_roof 
                  ALL_DATA_COMBINED$fibercement_asbestos_roof <- ifelse(is.na(ALL_DATA_COMBINED$fibercement_asbestos_roof.x), 
                                                            ALL_DATA_COMBINED$fibercement_asbestos_roof.y, ALL_DATA_COMBINED$fibercement_asbestos_roof.y)
                  # electric_heating 
                  ALL_DATA_COMBINED$electric_heating <- ifelse(is.na(ALL_DATA_COMBINED$electric_heating.x), 
                                                                          ALL_DATA_COMBINED$electric_heating.y, ALL_DATA_COMBINED$electric_heating.y)
                  # central_heating 
                  ALL_DATA_COMBINED$central_heating <- ifelse(is.na(ALL_DATA_COMBINED$central_heating.x), 
                                                                 ALL_DATA_COMBINED$central_heating.y, ALL_DATA_COMBINED$central_heating.y)
                  # <1940
                  ALL_DATA_COMBINED$`Renovated_1940-1950` <- ifelse(is.na(ALL_DATA_COMBINED$`Renovated_1940-1950.x`), 
                                                        ALL_DATA_COMBINED$`Renovated_1940-1950.y`, ALL_DATA_COMBINED$`Renovated_1940-1950.x`) 
                  # 1950-1960
                  ALL_DATA_COMBINED$`Renovated_1950-1960` <- ifelse(is.na(ALL_DATA_COMBINED$`Renovated_1950-1960.x`), 
                                                          ALL_DATA_COMBINED$`Renovated_1950-1960.y`, ALL_DATA_COMBINED$`Renovated_1950-1960.x`)
                  # 1960-1970
                  ALL_DATA_COMBINED$`Renovated_1960-1970` <- ifelse(is.na(ALL_DATA_COMBINED$`Renovated_1960-1970.x`), 
                                                          ALL_DATA_COMBINED$`Renovated_1960-1970.y`, ALL_DATA_COMBINED$`Renovated_1960-1970.x`)
                  # 1970-1980
                  ALL_DATA_COMBINED$`Renovated_1970-1980` <- ifelse(is.na(ALL_DATA_COMBINED$`Renovated_1970-1980.x`), 
                                                          ALL_DATA_COMBINED$`Renovated_1970-1980.y`, ALL_DATA_COMBINED$`Renovated_1970-1980.x`)
                  # 1980-1990
                  ALL_DATA_COMBINED$`Renovated_1980-1990` <- ifelse(is.na(ALL_DATA_COMBINED$`Renovated_1980-1990.x`), 
                                                          ALL_DATA_COMBINED$`Renovated_1980-1990.y`, ALL_DATA_COMBINED$`Renovated_1980-1990.x`)
                  # enhed_id
                  ALL_DATA_COMBINED$enhed_id <- ifelse(is.na(ALL_DATA_COMBINED$enhed_id.x), 
                                                          ALL_DATA_COMBINED$enhed_id.y, ALL_DATA_COMBINED$enhed_id.x)
                  # rooms
                  ALL_DATA_COMBINED$rooms <- ifelse(is.na(ALL_DATA_COMBINED$rooms.x), 
                                                         ALL_DATA_COMBINED$rooms.y, ALL_DATA_COMBINED$rooms.x)
                  # toilets
                  ALL_DATA_COMBINED$toilets <- ifelse(is.na(ALL_DATA_COMBINED$toilets.x), 
                                                      ALL_DATA_COMBINED$toilets.y, ALL_DATA_COMBINED$toilets.x)
                  # etage
                  ALL_DATA_COMBINED$etage <- ifelse(is.na(ALL_DATA_COMBINED$etage.x), 
                                                        ALL_DATA_COMBINED$etage.y, ALL_DATA_COMBINED$etage.x)
                  # postnr
                  ALL_DATA_COMBINED$postnr <- ifelse(is.na(ALL_DATA_COMBINED$postnr.x), 
                                                      ALL_DATA_COMBINED$postnr.y, ALL_DATA_COMBINED$postnr.x)
                  # sales_date
                  ALL_DATA_COMBINED$sales_date <- ifelse(is.na(ALL_DATA_COMBINED$sales_date.x), 
                                                       ALL_DATA_COMBINED$sales_date.y, ALL_DATA_COMBINED$sales_date.x)
                  # nominal_price
                  ALL_DATA_COMBINED$nominal_price <- ifelse(is.na(ALL_DATA_COMBINED$nominal_price), 
                                            ALL_DATA_COMBINED$price, ALL_DATA_COMBINED$nominal_price)
                  # lightweight_concrete
                  ALL_DATA_COMBINED$lightweight_concrete <- ifelse(is.na(ALL_DATA_COMBINED$lightweight_concrete), 
                                            ALL_DATA_COMBINED$Lightweightconcrete, ALL_DATA_COMBINED$lightweight_concrete)
                  # Tegl
                  ALL_DATA_COMBINED$Tile <- ifelse(is.na(ALL_DATA_COMBINED$Tile), 
                                            ifelse(ALL_DATA_COMBINED$Tagdækning_Materiale == 5, 1, NA), 
                                            ALL_DATA_COMBINED$Tile)
                  
                  
                  ALL_Var_To_Keep <- c("vejnavn", "husnr", "size", "outhouse", "TerracedHouse", "year_of_built", "Geometri_EPSG_25832", 
                                       "adr_etrs89_oest", "adr_etrs89_nord", "Car_Park", "<1940", "1950-1960", "1950-1960",
                                       "1960-1970", "1970-1980", "1980-1990", "1990-2000", "2000-2010", "2010", "Wood", "Brick",
                                       "Lightweightconcrete","Tile","thatch_roof", "fibercement_asbestos_roof", "electric_heating", 
                                       "central_heating", "district_heating","Renovated_1940-1950", "Renovated_1950-1960", "Renovated_1960-1970", "Renovated_1970-1980",
                                       "Renovated_1980-1990", "enhed_id", "rooms", "toilets", "etage", "postnr", "sales_date",
                                       "nominal_price", "Rain", "flooded", "event_dates_1", "tab_1", "event_dates_2", "tab_2", 
                                       "event_dates_9", "tab_9")
                  
                  ALL_DATA_COMBINED <- subset(ALL_DATA_COMBINED, select = ALL_Var_To_Keep)
                  save(ALL_DATA_COMBINED, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ALL_DATA_COMBINED.Rdata")
                  load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ALL_DATA_COMBINED.Rdata")
                  # I merge some variable with a lot of missing observations by taking. This is a temporary merge
                  load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Carsten Bertram/SpecialeTrades_1992-2021.RData")
                              var <- c("vejnavn", "husnr", "postnr", "Heating1", "Heating2", "Heating3", "Roof1", "Roof2", "Roof3") 
                              trades_1992_2021 <-  subset(trades_1992_2021, select = var)
                              ALL_DATA_COMBINED <- merge(ALL_DATA_COMBINED, trades_1992_2021, by.x = c("vejnavn", "husnr", "postnr"), by.y = c("vejnavn", "husnr", "postnr"), all.x = TRUE)
  # Cleaning data ----
      # This script is made for cleaning the data received from Toke. I imagine to delete columns, outliers, etc. 
      # How to clean data # https://www.r-bloggers.com/2021/04/how-to-clean-the-datasets-in-r/

  summary(ALL_DATA_COMBINED)
                  
                                                      
              
  # Hente GEO data ----
    
        ### To handle spatial data, load library ----
              library("sf")
              library("leaflet")
              library("sp")
              
{              # Requires "brew install gdal"
  API_GEO <- "https://api.dataforsyningen.dk/GeoDanmark_60_NOHIST_DAF?service=WMS&request=GetCapabilities&token="
  token <- readline(prompt="Please enter token: ")
  url <- paste0(API_GEO,token)
  response <- httr::GET(url)
  httr::status_code(response)
  
  parameters <- list(service = "WMS", 
                     version = "2.0.0", 
                     request = "GetFeature", 
                     typename = "layername", 
                     outputFormat = "json")
  GEO_DATA <- sf::st_read(url)
  
  # Create the full URL by pasting the base URL and parameters together
  url <- paste(base_url, paste(names(params), parameters, sep = "=", collapse = "&"), sep = "?")
  
  # Make the request
  GEO_DATA <- sf::st_read(url)
              } # Delete later if not useful
              
              # Load the shapefile
                # Højspænding
                Hoejspænding <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/hoejspaendingsledning/hoejspaendingsledning.shp")
                # Jernbane
                Jernbane1 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/jernbane/jernbane_0001/jernbane.shp")
                Jernbane <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/jernbane/jernbane_0000/jernbane.shp")
                
                # Kyst 
                Jernbane0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/kyst/kyst_0000/kyst.shp")
                Jernbane <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/kyst/kyst_0001/kyst.shp")
 
                # skov 
                Skov0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0000/skov.shp")
                Skov1 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0001/skov.shp")
                Skov2 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0002/skov.shp")
                Skov3 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0003/skov.shp")
                Skov4 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0004/skov.shp")
                Skov5 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0005/skov.shp")
                Skov6 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0006/skov.shp")
                Skov7 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0007/skov.shp")
                Skov8 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0008/skov.shp")
                Skov9 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0009/skov.shp")
                Skov10 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0010/skov.shp")
                
                # Soe 
                Soe0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0000/soe.shp")
                Soe1 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0001/soe.shp")
                Soe2 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0002/soe.shp")
                Soe3 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0003/soe.shp")
                Soe4 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0004/soe.shp")
                Soe5 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0005/soe.shp")
                Soe6 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0006/soe.shp")
                Soe7 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0007/soe.shp")
                Soe8 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0008/soe.shp")
                Soe9 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0009/soe.shp")
                Soe10 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0010/soe.shp")
                Soe11 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0011/soe.shp")
                Soe12 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0012/soe.shp")
                Soe13 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0013/soe.shp")
                Soe14 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0014/soe.shp")
                Soe15 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0015/soe.shp")
                Soe16 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0016/soe.shp")
                Soe17 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0017/soe.shp")
                Soe18 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0018/soe.shp")
                
                
                # Togstation
                Togstation <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/togstation/togstation.shp")
                
                # Vaadområde 
                Vaadområde0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vaadomraade/vaadomraade_0000/vaadomraade.shp")
                Vaadområde1 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vaadomraade/vaadomraade_0001/vaadomraade.shp")
                Vaadområde2 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vaadomraade/vaadomraade_0002/vaadomraade.shp")
                
                # Mangler vejkant pga. 160
                # vejkant0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vejkant/vejkant_0000/vejkant.shp")
                
                
                # Vindmølle
                Vindmoelle <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vindmoelle/vindmoelle.shp")
                
                
                
                {
          Handel_BFE_Adresse <- merge(EJF_Handelsoplysning, DAWA_Adresse, by.x = "objectid", by.y = "Jordstykke_Bfenummer")
           #### Remove unneccesary variables ----
                Var_to_keep <- c("objectid", "id_lokalId", "registreringFra", "registreringTil", "afstaaelsesdato", "koebsaftaleDato", 
                                 "samletKoebesum", "valutakode", "bygningerOmfattet", "Adresseid", "Kommunekode", "Kommunenavn", "Vejkode", 
                                 "Vejnavn", "Husnr", "Etage", "Doer", "Postnr", "Postdistrikt", "Jordstykke_SfeEjendomsnr","Jordstykke_EsrEjendomsnr",
                                 "Jordstykke_Matrikelnr", "ETRS89_Oest", "ETRS89_Nord", "Geometri_EPSG_25832")
                Handel_BFE_Adresse <- subset(Handel_BFE_Adresse, select = Var_to_keep)
                
          } #To delete later if not useful 
                

        ## Calc distance
          library(sf)
          library(geosphere)
                geosphere::distGeo(Vindmoelle$geometry, CLEAN_DATA$Geometri_EPSG_25832)   
                # First change the column to a spatial variable column instead of character
                CLEAN_DATA$Geometri_EPSG_25832 <- sf::st_as_sfc(CLEAN_DATA$Geometri_EPSG_25832)
                # Calculate distances (in meters) between linestrings and nearest points
                nearest_distances <- lapply(Vindmoelle$geometry, function(obj_geom) {
                  min(st_distance(CLEAN_DATA[1,12], obj_geom, by_element = TRUE))
                })

                
                # Initialize a new column in CLEAN_DATA to store the nearest geometries
                CLEAN_DATA$nearest_geometry <- vector("list", nrow(CLEAN_DATA))
                
                # For each observation in CLEAN_DATA
                for (i in seq_len(nrow(CLEAN_DATA))) {
                  # Calculate all distances
                  distances <- sapply(Vindmoelle$geometry, function(obj_geom) {
                    st_distance(CLEAN_DATA[i,12], obj_geom, by_element = TRUE)
                  })
                  
                  # Find the index of the minimum distance
                  nearest_index <- which.min(distances)
                  
                  # Get the nearest geometry and add it to the observation
                  CLEAN_DATA$nearest_geometry[[i]] <- Vindmoelle$geometry[[nearest_index]]
                }
                
          
  load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/DAWA_Adresse.Rdata")        
  load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Enhed.Rdata")        
  load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/EJF_Handelsoplysning.Rdata")
          
                    


  

  