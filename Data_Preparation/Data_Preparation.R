# Titles = No tabs 
# Subtitles two tabs 
# Subsubtitles = 4 tabs
# etc. 

# Library ----
library(openxlsx)
library(dplyr)



# Data of precipitation ----
# The following loads in the data from Toke about damages due to precipitation. 

    ## Load data ----
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/fp_events.Rdata")
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev6_Aalborg.Rdata")
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev7_aarhus.Rdata")
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev8_Trekanstomraade.Rdata")
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev1_Koebenhavn.Rdata")
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev1_StorKoebenhavn.Rdata")

        ### Excel printout of head of each sheet ----
        sheet_names <- c("Koebenhavn", "Storkoebenhavn", "Aalborg", "Aarhus", "Trekantsområde", "fp_events")
        data_frames <- list(`ev1_Koebenhavn`, `ev1_StorKoebenhavn`, `ev6_Aalborg`, `ev7_aarhus`, `ev8_Trekanstomraade`,`fp_events` )  # Add other data frames here
        
        # library(openxlsx)
        wb <- createWorkbook()
        file_path <- "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Excel_Summary.xlsx"  # Specify the desired file path
        
        # Add each data frame to a separate sheet
        for (i in seq_along(data_frames)) {
          addWorksheet(wb, sheetName = sheet_names[i])
          writeData(wb, sheet = i, x = summary(data_frames[[i]]))
        }
        
        # Save workbook
        saveWorkbook(wb, file_path, overwrite = TRUE)
        rm(wb)
        rm(file_path)
    
    ## Bind rows of data ----
        Precipitation <- dplyr::bind_rows(fp_events, ev1_Koebenhavn, ev1_StorKoebenhavn, 
                                           ev6_Aalborg, ev7_aarhus, ev8_Trekanstomraade)
        # Remove duplicates
        Precipitation <- Precipitation %>% distinct(.keep_all = TRUE)
        
        # Remove first data frames 
        rm(ev1_Koebenhavn, ev1_StorKoebenhavn, ev6_Aalborg, ev7_aarhus, ev8_Trekanstomraade, fp_events)
        
            ### Statistics ---- 
            # How many unique buildings?
            iNO_UNIQUE_Entities <- length(unique(Precipitation$Enhed_id))
            print(iNO_UNIQUE_Entities)
            # Answer = 155.560
            
            # How many entities have been flooded - Variable of interest 
            iFlooded_Homes <- Precipitation %>%
              filter(total_payout != 0) %>%
              summarise(unique_count = n_distinct(Enhed_id))
            print(iFlooded_Homes)
            # Answer = 7331

    ## Reducing dimension of data frame 
      variables_to_keep <- c("Enhed_id", "x", "y", "kommunekode", "husnr", "etage", "doer", "vejnavn", 
                    "postnr", "kommunenavn", "height", "unit_type_code", "size", "rooms", "bath", 
                    "toilets", "floor", "car_park", "car_park_dobble", "outhouse", "brick", 
                    "lightweight_concrete", "timbered", "wood", "concrete", 
                    "Builtup_roof", "fibercement_asbestos_roof", "cement_roof", "thatch_roof",
                    "district_heating", "central_heating", "heatpump_heating", "electric_heating", 
                    "year_of_built", "major_renovations", "Renovation70s", "Renovation80s", 
                    "Renovation90s", "Renovation00s", "Renovation10s", "Energy_code", "urban_size", 
                    "forest_distance", "forest_size", "coastline_distance", "habour_distance", 
                    "highway_distance", "powerline_distance", "railway_distance", "trainstation_distance", 
                    "lake_distance", "windturbine_distance", "windturbine_height", "market_name", "price",
                    "sales_date", "Bluespot_0cm", "Bluespot_10cm", "Bluespot_20cm", "event_dates_1", 
                    "tab_1", "event_dates_2","tab_2", "event_dates_9", "tab_9", "event_dates_6", "tab_6" ,
                    "event_dates_7", "tab_7","f_sold_after",
                    "flood_0_05yr", "flood_05_1yr", "flood_1yr", "flood_2yr", "flood_3yr", "total_payout",
                    "flooded")      
      Precipitation_subset <- subset(Precipitation, select = variables_to_keep) 
      rm(Precipitation)
      
        ### Building variables ----
        # Spatial variable
        Precipitation_subset$Geometri_EPSG_25832 <- paste0(Precipitation_subset$x, " ", Precipitation_subset$y)
        Precipitation_subset$Geometri_EPSG_25832 <- sf::st_as_sf(Precipitation_subset$Geometri_EPSG_25832)
        Precipitation_subset$Geometri_EPSG_25832 <- sf::st_as_sfc(Precipitation_subset, "Geometri_EPSG_25832")
        my_sf <- st_as_sf(my_data, coords = c("longitude", "latitude"), crs = 4326)
        
        CLEAN_DATA$Geometri_EPSG_25832 <- sf::st_as_sfc(CLEAN_DATA$Geometri_EPSG_25832)
        # Terraced House (rækkehus)
        Precipitation_subset$TerracedHouse <- ifelse(Precipitation_subset$unit_type_code == 131, 1, 0)
        
        # Built year
        Precipitation_subset$'<1940' <-     ifelse(Precipitation_subset$year_of_built < 1940, 1, 0)
        Precipitation_subset$'1940-1950' <- ifelse(Precipitation_subset$year_of_built < 1950 & 
                                            Precipitation_subset$year_of_built > 1940 , 1, 0)
        Precipitation_subset$'1950-1960' <- ifelse(Precipitation_subset$year_of_built < 1960 & 
                                            Precipitation_subset$year_of_built > 1950 , 1, 0)
        Precipitation_subset$'1960-1970' <- ifelse(Precipitation_subset$year_of_built < 1970 & 
                                            Precipitation_subset$year_of_built > 1960 , 1, 0)
        Precipitation_subset$'1970-1980' <- ifelse(Precipitation_subset$year_of_built < 1980 & 
                                            Precipitation_subset$year_of_built > 1970 , 1, 0)
        Precipitation_subset$'1980-1990' <- ifelse(Precipitation_subset$year_of_built < 1990 & 
                                            Precipitation_subset$year_of_built > 1980 , 1, 0)
        Precipitation_subset$'1990-2000' <- ifelse(Precipitation_subset$year_of_built < 2000 & 
                                            Precipitation_subset$year_of_built > 1990 , 1, 0)
        Precipitation_subset$'2000-2010' <- ifelse(Precipitation_subset$year_of_built < 2010 & 
                                            Precipitation_subset$year_of_built > 2000 , 1, 0)
        Precipitation_subset$'2010' <-      ifelse(Precipitation_subset$year_of_built > 2010, 1, 0)
        
        # Renovation 
        Precipitation_subset$'Renovated_1940-1950' <- ifelse(Precipitation_subset$major_renovations < 1950 & 
                                                      Precipitation_subset$major_renovations > 1940 , 1, 0)
        Precipitation_subset$'Renovated_1950-1960' <- ifelse(Precipitation_subset$major_renovations < 1960 & 
                                                      Precipitation_subset$major_renovations > 1950 , 1, 0)
        Precipitation_subset$'Renovated_1960-1970' <- ifelse(Precipitation_subset$major_renovations < 1970 & 
                                                      Precipitation_subset$major_renovations > 1960 , 1, 0)
        Precipitation_subset$'Renovated_1970-1980' <- ifelse(Precipitation_subset$major_renovations < 1980 & 
                                                      Precipitation_subset$major_renovations > 1970 , 1, 0)
        Precipitation_subset$'Renovated_1980-1990' <- ifelse(Precipitation_subset$major_renovations < 1990 & 
                                                      Precipitation_subset$major_renovations > 1980 , 1, 0)
    ### Save Precipitation ----
    save(Precipitation_subset, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Precipitation_subset.Rdata")    
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Precipitation_subset.Rdata")
            
    ## Id for data frame = Enhed_id, adresse (husnr, vejnavn, postnr)
        
# BBR Bygning ----
    ## Load data of BBR_Bygning
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Bygning.rdata")
        
    ## Reduce dimension ----
    Var_BBR_Bygning <- c("bygning_id","byg039BygningensSamledeBoligAreal", "byg021BygningensAnvendelse", "byg044ArealIndbyggetUdhus", "byg026Opførelsesår", 
             "byg032YdervæggensMateriale", "byg033Tagdækningsmateriale", "byg056Varmeinstallation", "byg027OmTilbygningsår",
             "grund", "husnummer","Geometri_EPSG_25832", "adr_etrs89_oest", "adr_etrs89_nord")
    BBR_Bygning_Subset <- subset(BBR_Bygning, select = Var_BBR_Bygning)    
    rm(BBR_Bygning) 
    rm(Var_BBR_Bygning)
    
    ## Rename variables ----
    BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Size = byg039BygningensSamledeBoligAreal)
    BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Bygninganvendelse = byg021BygningensAnvendelse)
    BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Outbuilding = byg044ArealIndbyggetUdhus)
    BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Opførelsesår = byg026Opførelsesår)
    BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Ydervægge_Materiale = byg032YdervæggensMateriale)
    BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Tagdækning_Materiale = byg033Tagdækningsmateriale)
    BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Varmeinstallation = byg056Varmeinstallation)
    BBR_Bygning_Subset <- dplyr::rename(BBR_Bygning_Subset, Ombygning = byg027OmTilbygningsår)
        
        
    ## Reduce observation by limiting real estate
    BBR_Bygning_Subset <- subset(BBR_Bygning_Subset, Bygninganvendelse == 110 | Bygninganvendelse == 120 |
                          Bygninganvendelse == 121 | Bygninganvendelse == 122 | Bygninganvendelse == 130 |
                          Bygninganvendelse == 131 | Bygninganvendelse == 132 | Bygninganvendelse == 140 |
                          Bygninganvendelse == 190 | Bygninganvendelse == 510 | Bygninganvendelse == 910 |
                          Bygninganvendelse == 920 | Bygninganvendelse == 930 | Bygninganvendelse == 940 |
                          Bygninganvendelse == 950 | Bygninganvendelse == 960)      

    ## Create variables ----
    ### Carport ----
    BBR_Bygning_Subset$Car_Park <- ifelse(BBR_Bygning_Subset$Bygninganvendelse == 920, 1, 0)
    
    ### Car garage ----
    BBR_Bygning_Subset$Garage <- ifelse(BBR_Bygning_Subset$Bygninganvendelse == 910, 1, 0)
    
    ### Terraced House ----
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
    
    ## Save and load BBR_Bygning_Subset ----
    save(BBR_Bygning_Subset, file ="~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Subset.Rdata")    
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Subset.Rdata")        
    
#BBR_Enhed 
    ## Load data to see if possible mapping between Precipitation and Trade_Skader ----
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Enhed.Rdata")        
    Var_BBR_Enhed <- c("enhed_id", "adresseIdentificerer", "etage", "opgang", "bygning")
    BBR_Enhed_Subset <- subset(BBR_Enhed, select = Var_BBR_Enhed)
    save(BBR_Enhed_Subset, file ="~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Enhed_Subset.Rdata")    
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Enhed_Subset.Rdata")
    
# Trades 1991-2021 from Carsten ----
    
    ## Load data ----
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Carsten Bertram/SpecialeTrades_1992-2021.RData")
    
    ## Narrow down variables ----
    # Change heating to numeric variable like other df 
    trades_1992_2021$district_heating <- ifelse(trades_1992_2021$Heating1 == "Fjernvarme/blokvarme", 1, 0)
    trades_1992_2021$central_heating <- ifelse(trades_1992_2021$Heating1 == "Centralvarme med én fyringsenhed" | 
                                                trades_1992_2021$Heating1 == "Centralvarme med to fyringsenheder", 1, 0)
    trades_1992_2021$electric_heating <- ifelse(trades_1992_2021$Heating1 == "Elvarme", 1, 0)
    # Change roof to numeric variable like other df 
    trades_1992_2021$Tile <- ifelse(trades_1992_2021$Roof1 == "Tegl", 1, 0)
    trades_1992_2021$Thatched <- ifelse(trades_1992_2021$Roof1 == "Stråtag", 1, 0)
    trades_1992_2021$Fiberasbetos <- ifelse(trades_1992_2021$Roof1 == "Fibercement herunder asbest", 1, 0)
    trades_1992_2021$roofing <- ifelse(trades_1992_2021$Heating1 == "Tagpap med lille hældning" | 
                                            trades_1992_2021$Heating1 == "Tagpap med stor hældning", 1, 0)
    `Var_Trades_1992-2021`  <- c("vejnavn", "husnr", "floor","postnr", "kommunenavn", "m2", "side", "latitude",
                                 "longitude", "nominal_price", "year", "dato", "hustype", "addressID", 
                                 "entryAddressID", "district_heating", "central_heating", "electric_heating",
                                 "Tile", "Thatched", "Fiberasbetos", "roofing")
    `Trades_1992_2021_Subset` <- subset(trades_1992_2021, select = `Var_Trades_1992-2021`)
    save(`Trades_1992_2021_Subset`, file ="~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Trades_1992_2021_Subset.Rdata")
    # load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Trades_1992_2021_Subset.Rdata")
    rm(`Var_Trades_1992-2021`)
    
# Skader, a df that contains damage of flooding ---- 
    ## load data ----
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Skader.Rdata")
    
    ## Reduce 'Skader' of variables
    Var_Skader <- c("vejnavn", "husnr", "bygning_id", "Outbuilding", "Opførelsesår", 
                    "grund", "husnummer", "Geometri_EPSG_25832", "adr_etrs89_oest",
                    "adr_etrs89_nord", "Car_Park", "Garage", "TerracedHouse", "<1940",
                    "1940-1950", "1950-1960", "1960-1970", "1980-1990", "1990-2000",
                    "2000-2010", "2010", "Brick", "Lightweightconcrete", "Wood",
                    "Renovated_1940-1950", "Renovated_1950-1960", "Renovated_1960-1970",
                    "Renovated_1970-1980", "Renovated_1980-1990", "adresseIdentificerer",
                    "enhed_id", "enh031AntalVærelser", "enh032Toiletforhold", "etage",
                    "opgang", "dato", "entryAddressID", "Hændelsesdato", "Selvrisiko",
                    "Tidligere udbetalt byg/løs/afgrd")
    Skader_subset <- subset(Skader, select = Var_Skader)
    save(Skader_subset, file ="~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Skader_Subset.Rdata")
    rm(Skader)
    
    # load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Skader_Subset.Rdata")
    

# Merging ----
    ## Merge Trade (Carsten) and Skader (Toke) by addressID and husnummer, respectively
    Trade_Skader <- merge(Trades_1992_2021_Subset, Skader_subset, by.x = "addressID", by.y = "adresseIdentificerer")
    #
    # Assumption to be on building level. Problem with merging on unique address
    # So far no reason to merge with BBR_Bygning, as it does not add any missing variable. Only maybe to add something BBR_Enhed.
    
        ### Narrow down variables ----
        Var_Trade_Skader <- c("vejnavn.y", "husnr.y")
        Trade_Skader <- Trade_Skader[, !names(Trade_Skader) %in% Var_Trade_Skader]

    
    ## Merge Precipitation on merged data  ----
    # Precipitation only unique identifier is 'Enhed' that maps onto BBR_Enhed_Subset enhed_id
    Precipitation_subset <- merge(Precipitation_subset, BBR_Enhed_Subset, by.x = "Enhed_id", by.y = "enhed_id")
    ## Keep only subset of variables ----
    Var_Precipitation_subset <- c("bygning", "urban_size", "forest_distance", "forest_size", "coastline_distance",
                                  "habour_distance", "highway_distance", "powerline_distance", "railway_distance",
                                  "trainstation_distance", "lake_distance", "windturbine_distance", 
                                  "windturbine_height", "market_name", "Bluespot_0cm", "Bluespot_10cm", 
                                  "Bluespot_20cm", "event_dates_1", "tab_1", "event_dates_2", "tab_2",
                                  "event_dates_9", "tab_9", "event_dates_6", "tab_6", "event_dates_7",
                                  "tab_7", "f_sold_after", "flood_0_05yr", "flood_05_1yr", "flood_1yr",
                                  "flood_2yr", "flood_3yr", "total_payout", "flooded")  
    Precipitation_subset <- subset(Precipitation_subset, select = Var_Precipitation_subset)    
        
    
    ## Merge of Precipitation and Trade_Skader ----
    Total_df <- merge(Trade_Skader, Precipitation_subset, by.x = "bygning_id", by.y = "bygning")
    
    
    
        