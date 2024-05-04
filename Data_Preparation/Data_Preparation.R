# Titles = No tabs 
# Subtitles two tabs 
# Subsubtitles = 4 tabs
# etc. 

# Library ----
library(openxlsx)
library(dplyr)
library(tidyr)
library(sf)
library(sp)
library(rgdal)
library(geos)
library(foreach)
library(doParallel)
library(units)
library(Kendall)
library(doSNOW)
library(tripack)
library(spdep)
library(spData)
library(spDataLarge)
library(pbapply)
library(psych)
library(ggplot2)
library(ggmap)



# Data of precipitation ----
# The following loads in the data from Toke about damages due to precipitation. 

    ## Load data ----
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/fp_events.Rdata")
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev6_Aalborg.Rdata")
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev7_aarhus.Rdata")
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev8_Trekanstomraade.Rdata")
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev1_Koebenhavn.Rdata")
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev1_StorKoebenhavn.Rdata")

        ### Excel printout of head of each sheet ----
        sheet_names <- c("Koebenhavn", "Storkoebenhavn", "Aalborg", "Aarhus", "Trekantsområde", "fp_events")
        data_frames <- list(`ev1_Koebenhavn`, `ev1_StorKoebenhavn`, `ev6_Aalborg`, `ev7_aarhus`, `ev8_Trekanstomraade`,`fp_events` )  # Add other data frames here
        
        # library(openxlsx)
        wb <- createWorkbook()
        file_path <- "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Excel_Summary.xlsx"  # Specify the desired file path
        
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
        Precipitation <- Precipitation %>% dplyr::distinct(.keep_all = TRUE)
        
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
      # variables_to_keep <- c("Enhed_id","vejnavn", "husnr", "postnr", "x", "y", "kommunekode", 
                    #          "husnr", "etage", "doer", "vejnavn", 
                    # "postnr", "kommunenavn", "height", "unit_type_code", "size", "rooms", "bath", 
                    # "toilets", "floor", "car_park", "car_park_dobble", "outhouse", "brick", 
                    # "lightweight_concrete", "timbered", "wood", "concrete", 
                    # "Builtup_roof", "tile_roof","fibercement_asbestos_roof", "cement_roof", "thatch_roof",
                    # "district_heating", "central_heating", "heatpump_heating", "electric_heating", 
                    # "year_of_built", "major_renovations", "Renovation70s", "Renovation80s", 
                    # "Renovation90s", "Renovation00s", "Renovation10s", "Energy_code", "urban_size", 
                    # "forest_distance", "forest_size", "coastline_distance", "habour_distance", 
                    # "highway_distance", "powerline_distance", "railway_distance", "trainstation_distance", 
                    # "lake_distance", "windturbine_distance", "windturbine_height", "market_name", "price",
                    # "sales_date", "Bluespot_0cm", "Bluespot_10cm", "Bluespot_20cm", "event_dates_1", 
                    # "tab_1", "event_dates_2","tab_2", "event_dates_9", "tab_9", "event_dates_6", "tab_6" ,
                    # "event_dates_7", "tab_7","f_sold_after",
                    # "flood_0_05yr", "flood_05_1yr", "flood_1yr", "flood_2yr", "flood_3yr", "total_payout",
                    # "flooded")
      Var_Precipitation_subset <- c("Enhed_id","vejnavn", "husnr", "postnr","x","y", "bygning", 
                                    "unit_type_code","urban_size", "district_heating", 
                                    "central_heating", "heatpump_heating", "year_of_built", 
                                    "major_renovations", "electric_heating", "tile_roof", "thatch_roof", 
                                    "fibercement_asbestos_roof", "outhouse", "year_of_built", "car_park", 
                                    "car_park_dobble", "unit_type_code","brick", "lightweight_concrete", 
                                    "wood", "rooms", "toilets", "forest_distance", "forest_size", 
                                    "coastline_distance","habour_distance", "highway_distance", 
                                    "powerline_distance", "railway_distance","trainstation_distance",
                                    "lake_distance", "windturbine_distance", "windturbine_height", 
                                    "market_name", "Bluespot_0cm", "Bluespot_10cm", "price", 
                                    "Bluespot_20cm", "event_dates","event_dates_1", "tab_1", "event_dates_2", 
                                    "tab_2","event_dates_9", "tab_9", "event_dates_6", "tab_6", "event_dates_7",
                                    "tab_7", "f_sold_after", "flood_0_05yr", "flood_05_1yr", "flood_1yr",
                                    "flood_2yr", "flood_3yr", "total_payout", "flooded")  
      Precipitation_subset <- subset(Precipitation, select = Var_Precipitation_subset) 
      rm(Precipitation)
      
        ### Building variables ----
        # Spatial variable
        Precipitation_subset$Geometri_EPSG_25832 <- paste0(Precipitation_subset$x, " ", Precipitation_subset$y)
        #Precipitation_subset$Geometri_EPSG_25832 <- sf::st_as_sf(Precipitation_subset$Geometri_EPSG_25832)
        #Precipitation_subset$Geometri_EPSG_25832 <- sf::st_as_sfc(Precipitation_subset, "Geometri_EPSG_25832")
        #my_sf <- st_as_sf(my_data, coords = c("longitude", "latitude"), crs = 4326)
      
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
    save(Precipitation_subset, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Precipitation_subset.Rdata")    
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Precipitation_subset.Rdata")
            
    ## Id for data frame = Enhed_id, adresse (husnr, vejnavn, postnr)
        
# BBR Bygning ----
    ## Load data of BBR_Bygning
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Bygning.rdata")
        
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
    save(BBR_Bygning_Subset, file ="~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Subset.Rdata")    
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Subset.Rdata")        
    
#BBR_Enhed 
    ## Load data to see if possible mapping between Precipitation and Trade_Skader ----
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Enhed.Rdata")        
    Var_BBR_Enhed <- c("enhed_id", "adresseIdentificerer", "etage", "opgang", "bygning")
    BBR_Enhed_Subset <- subset(BBR_Enhed, select = Var_BBR_Enhed)
    save(BBR_Enhed_Subset, file ="~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Enhed_Subset.Rdata")    
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Enhed_Subset.Rdata")
    
# Trades 1991-2021 from Carsten ----
    
    ## Load data ----
    load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Carsten Bertram/SpecialeTrades_1992-2021.RData")
    
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
    save(`Trades_1992_2021_Subset`, file ="~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Trades_1992_2021_Subset.Rdata")
    # load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Trades_1992_2021_Subset.Rdata")
    rm(`Var_Trades_1992-2021`)
    
# Skader, a df that contains damage of flooding ---- 
    ## load data ----
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Skader.Rdata")
    
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
    save(Skader_subset, file ="~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Skader_Subset.Rdata")
    
    rm(Skader)
    
    # load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Skader_Subset.Rdata")
    

# Merging ----
    ## Merge Trade (Carsten) and Skader (Toke) by addressID and husnummer, respectively
    Trade_Skader <- merge(Trades_1992_2021_Subset, Skader_subset, by.x = "addressID", by.y = "adresseIdentificerer")
    #
    # Assumption to be on building level. Problem with merging on unique address
    # So far no reason to merge with BBR_Bygning, as it does not add any missing variable. Only maybe to add something BBR_Enhed.
    
        ### Narrow down variables ----
        Var_Trade_Skader <- c("vejnavn.y", "husnr.y")
        Trade_Skader <- Trade_Skader[, !names(Trade_Skader) %in% Var_Trade_Skader]
        
        ### Skader variable to be able to distinguish between flood and precipitation
        Trade_Skader$flooded <- ifelse(!is.na(Trade_Skader$Hændelsesdato) | !is.na(Trade_Skader$`Tidligere udbetalt byg/løs/afgrd`), 2, 0)
    
    ## Merge Precipitation on merged data  ----
    # Precipitation only unique identifier is 'Enhed' that maps onto BBR_Enhed_Subset enhed_id
        load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Precipitation_subset.Rdata")
        # Precipitation_subset <- merge(Precipitation_subset, BBR_Enhed_Subset, by.x = "Enhed_id", by.y = "enhed_id")
    ## Rename variables to fit to Trade_Skader ----
        ### Trade_Skader
        Trade_Skader <- dplyr::rename(Trade_Skader, vejnavn = vejnavn.x, husnr = husnr.x, year_of_built = Opførelsesår,
                                      tile_roof = Tile, thatch_roof = Thatched, fibercement_asbestos_roof = Fiberasbetos,
                                      lightweight_concrete = Lightweightconcrete, wood = Wood, rooms = enh031AntalVærelser,
                                      toilets = enh032Toiletforhold)
        
        
        ### Precipitation
        Precipitation_subset <- dplyr::rename(Precipitation_subset, latitude = x, longitude = y, 
                                              nominal_price = price, Brick = brick, Outbuilding = outhouse, 
                                              Garage = car_park_dobble)
        
        # Toilet from Trade_Skader is not number of toilets like Precitipation
        Trade_Skader$toilets <- NA
        
        
    
    ## Merge of Precipitation and Trade_Skader ----
    #Total_df <- merge(Trade_Skader, Precipitation_subset, by.x = "bygning_id", by.y = "bygning")
    Total_df <- bind_rows(Trade_Skader, Precipitation_subset)
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df.Rdata")
# Finalizing data frame Total_df ----
    ## Remove unnecessary variables ----
    Var_Total_df <- c("vejnavn.y", "husnr.y")
    Total_df <- Total_df[, !names(Total_df) %in% Var_Total_df]
    
    ## display only latest occasion ----
        ### Redefine class for hændelsesdato 
        Total_df$Hændelsesdato <- as.Date(Total_df$Hændelsesdato)
    
    # Take dates to common variable
    # Ifelse command does not keep date format, instead use case_when from dplyr
    Total_df$Hændelsesdato <- dplyr::case_when(
      is.na(Total_df$Hændelsesdato) ~ case_when(
        !is.na(Total_df$event_dates_9) & Total_df$event_dates_9 < Total_df$dato.x ~ Total_df$event_dates_9,
        !is.na(Total_df$event_dates_7) & Total_df$event_dates_7 < Total_df$dato.x ~ Total_df$event_dates_7,
        !is.na(Total_df$event_dates_6) & Total_df$event_dates_6 < Total_df$dato.x ~ Total_df$event_dates_6,
        !is.na(Total_df$event_dates_2) & Total_df$event_dates_2 < Total_df$dato.x ~ Total_df$event_dates_2,
        !is.na(Total_df$event_dates_1) & Total_df$event_dates_1 < Total_df$dato.x ~ Total_df$event_dates_1,
        TRUE ~ as.Date(NA)
      ),
      TRUE ~ Total_df$Hændelsesdato
    )
    
    ### Reduce observations ----
    # if no date of occurances in given zip code then drop all observations with zipcode
    Total_df <- Total_df %>%
      group_by(postnr) %>%
      filter(!all(is.na(Hændelsesdato))) %>%
      ungroup()
    
    # Total_df <- Total_df %>%
    #   group_by(postnr) %>%
    #   filter(sum(!is.na(Hændelsesdato)) >= 150) %>%
    #   ungroup()
    
    # Delete if no observations in district_heating, central heating, roof, etc. 
    Total_df <- Total_df %>% tidyr::drop_na(district_heating, central_heating, electric_heating,
                                            tile_roof, thatch_roof, fibercement_asbestos_roof)
    
    
    # Add to TotalPay if the condition is met
    Total_df$Total_tab <- ifelse (!is.na(Total_df$event_dates_1) & Total_df$event_dates_1 < Total_df$dato.x,
            Total_df$Total_tab <- Total_df$Total_tab + Total_df$tab_1, Total_df$Total_tab <- Total_df$Total_tab) 
    
    Total_df$Total_tab <- ifelse (!is.na(Total_df$event_dates_2) & Total_df$event_dates_2 < Total_df$dato.x,
            Total_df$Total_tab <- Total_df$Total_tab + Total_df$tab_2, Total_df$Total_tab <- Total_df$Total_tab) 
    
    Total_df$Total_tab <- ifelse (!is.na(Total_df$event_dates_6) & Total_df$event_dates_6 < Total_df$dato.x,
            Total_df$Total_tab <- Total_df$Total_tab + Total_df$tab_6, Total_df$Total_tab <- Total_df$Total_tab) 
    
    Total_df$Total_tab <- ifelse (!is.na(Total_df$event_dates_7) & Total_df$event_dates_7 < Total_df$dato.x,
            Total_df$Total_tab <- Total_df$Total_tab + Total_df$tab_7, Total_df$Total_tab <- Total_df$Total_tab)     
    
    Total_df$Total_tab <- ifelse (!is.na(Total_df$event_dates_9) & Total_df$event_dates_9 < Total_df$dato.x,
            Total_df$Total_tab <- Total_df$Total_tab + Total_df$tab_9, Total_df$Total_tab <- Total_df$Total_tab)
    
    ## Add Total_tab to Udbetaling if NA ----
    Total_df$`Tidligere udbetalt byg/løs/afgrd` <- ifelse(is.na(Total_df$`Tidligere udbetalt byg/løs/afgrd`), 
                                                      Total_df$Total_tab, Total_df$`Tidligere udbetalt byg/løs/afgrd`)

    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df2.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df2.Rdata")
    
    ## Reduce to predictor variables ----
    Var_Total_df <- c("year", "hustype", "year_of_built", "bygning_id", "grund", "husnummer",
                       "etage", "car_park",
                      "opgang", "dato.y", "entryAddressID.y", "Selvrisiko", "Enhed_id",
                      "bygning", "unit_type_code", "major_renovations", "year_of_built.1", 
                      "unit_type_code.1", "event_dates", "event_dates_1", "tab_1",
                      "event_dates_2", "tab_2", "event_dates_9", "tab_9", "event_dates_6",
                      "tab_6", "event_dates_7", "tab_7", "f_sold_after", "flood_0_05yr", 
                      "flood_05_1yr", "flood_1yr", "flood_2yr", "flood_3yr", "total_payout",
                      "TotalPay", "Total_tab")
    # "Geometri_EPSG_25832", "adr_etrs89_oest", "adr_etrs89_nord",
    
    Total_df <- Total_df[, !names(Total_df) %in% Var_Total_df]
    rm(Var_Total_df)
    
    #Keep only unique observations
    Total_df <- dplyr::distinct(Total_df)
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df3.Rdata")
    

# Change geographical point variable to spatial variable ----
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df3.Rdata")

    ## Convert latitude and longitude to EPSG system. 
        ## Extract EPSG: 
        # Check data
        filtered_strings <- Total_df$Geometri_EPSG_25832[!grepl("^POINT", Total_df$Geometri_EPSG_25832)]
        # notice not all obs in Geometri_EPSG_25832 start with POINT ()
        # Use gsub to remove POINT from obs and adding at end 
        # Remove "POINT" and parentheses
        Total_df$EPSG <- gsub("POINT|\\(|\\)", "", Total_df$Geometri_EPSG_25832)
        # Retrieve coordinates in each column 
        Total_df$x_coord <- as.numeric(stringr::str_extract(Total_df$EPSG, "[^ ]+"))
        Total_df$y_coord <- as.numeric(stringr::str_extract(Total_df$EPSG, "(?<=\\s)\\d+\\.?\\d*$"))
        # Paste it together with Point in every obs 
        Total_df$Coor <- paste0("POINT (",Total_df$x_coord," ", Total_df$y_coord, ")")
        # Check data again 
        head(Total_df$Coor)
        class(Total_df$Coor)
        filtered_strings2 <- Total_df$Coor[!grepl("^POINT", Total_df$Coor)]
        st_as_sf(Total_df, wkt="Coor", crs = 25832)
        Total_df$Coor <- sf::st_as_sfc(Total_df$Coor)
    
       
    
    ## Save date ----
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df4.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df4.Rdata")


# Load in geographical variables ----
    
    ## Højspænding ----
    powerline_distance <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/hoejspaendingsledning/hoejspaendingsledning.shp")
    
    ## Jernbane ----
    Jernbane1 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/jernbane/jernbane_0001/jernbane.shp")
    Jernbane <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/jernbane/jernbane_0000/jernbane.shp")
    railway_distance <- rbind(Jernbane, Jernbane1)
    rm(Jernbane1)
    
    ## Kyst ----
    Kyst0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/kyst/kyst_0000/kyst.shp")
    Kyst1 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/kyst/kyst_0001/kyst.shp")
    coastline_distance <- rbind(Kyst0, Kyst1)
    rm(Kyst0, Kyst1)
    
    ## skov ----
    Skov0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0000/skov.shp")
    Skov1 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0001/skov.shp")
    Skov2 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0002/skov.shp")
    Skov3 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0003/skov.shp")
    Skov4 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0004/skov.shp")
    Skov5 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0005/skov.shp")
    Skov6 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0006/skov.shp")
    Skov7 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0007/skov.shp")
    Skov8 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0008/skov.shp")
    Skov9 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0009/skov.shp")
    Skov10 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/skov/skov_0010/skov.shp")
    forest_distance <- rbind(Skov0, Skov1, Skov2, Skov3, Skov4, Skov5, Skov6, Skov7, Skov8, Skov9, Skov10)
    rm(Skov0, Skov1, Skov2, Skov3, Skov4, Skov5, Skov6, Skov7, Skov8, Skov9, Skov10)
    forest_distance$area <- sf::st_area(forest_distance)
    forest_distance$area <- as.numeric(forest_distance$area)
    
    # Save time by calculating centroid 
    forest_distance$geometry <- centroid <- sf::st_centroid(forest_distance$geometry)
    
    # Subset based on area > 5000 m2 according to forest definition of FN
    forest_distance <- subset(forest_distance, forest_distance$area > 5000)
    
    # Subdivide 
    seq <- seq(0.00, 0.95, by = 0.05)
    
    #save in list
    list.dfs_forest <- list()
    
    for (i in seq) {
      # Create variable name
      df_name <- paste0("forest_distance_", (i+0.05))
      
      # Subset data 
      df <- subset(forest_distance, 
                   forest_distance$area > quantile(forest_distance$area, probs = i) & 
                     forest_distance$area < quantile(forest_distance$area, probs = (i + 0.05)))
      
      # Set CRS for the subset
      df <- sf::st_set_crs(df, sf::st_crs(Total_df))
      
      # Save data frame in list
      list.dfs_forest[[df_name]] <- df
    }
    rm(forest_distance, df)

    
    ## Soe ----
    Soe0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0000/soe.shp")
    Soe1 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0001/soe.shp")
    Soe2 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0002/soe.shp")
    Soe3 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0003/soe.shp")
    Soe4 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0004/soe.shp")
    Soe5 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0005/soe.shp")
    Soe6 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0006/soe.shp")
    Soe7 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0007/soe.shp")
    Soe8 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0008/soe.shp")
    Soe9 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0009/soe.shp")
    Soe10 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0010/soe.shp")
    Soe11 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0011/soe.shp")
    Soe12 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0012/soe.shp")
    Soe13 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0013/soe.shp")
    Soe14 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0014/soe.shp")
    Soe15 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0015/soe.shp")
    Soe16 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0016/soe.shp")
    Soe17 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0017/soe.shp")
    Soe18 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/soe/soe_0018/soe.shp")
    lake_distance <- rbind(Soe0, Soe1, Soe2, Soe3, Soe4, Soe5, Soe6, Soe7, Soe8, Soe9, 
                 Soe10, Soe11, Soe12, Soe13, Soe14, Soe15, Soe16, Soe17, Soe18)
    rm(Soe0, Soe1, Soe2, Soe3, Soe4, Soe5, Soe6, Soe7, Soe8, Soe9, 
       Soe10, Soe11, Soe12, Soe13, Soe14, Soe15, Soe16, Soe17, Soe18)
    
    lake_distance$area <- sf::st_area(lake_distance)
    lake_distance$area <- as.numeric(lake_distance$area)
    
    lake_distance$geometry <- centroid_points <- sf::st_centroid(lake_distance$geometry)
    
    seq <- seq(0.00, 0.95, by = 0.05)
    
    #save in list
    list.dfs_lake <- list()
    
    for (i in seq) {
      # Create variable name
      df_name <- paste0("lake_distance_", (i+0.025))
      
      # Subset data 
      df <- subset(lake_distance, 
                   lake_distance$area > quantile(lake_distance$area, probs = i) & 
                     lake_distance$area < quantile(lake_distance$area, probs = (i + 0.025)))
      
      # Set CRS for the subset
      df <- sf::st_set_crs(df, sf::st_crs(Total_df))
      
      # Save data frame in list
      list.dfs_lake[[df_name]] <- df
    }
    rm(lake_distance, df)
    
    ## Togstation ----
    trainstation_distance <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/togstation/togstation.shp")
    trainstation_distance <- sf::st_set_crs(trainstation_distance, sf::st_crs(Total_df))
    
    ## Vaadområde ----
    Vaadområde0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vaadomraade/vaadomraade_0000/vaadomraade.shp")
    Vaadområde1 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vaadomraade/vaadomraade_0001/vaadomraade.shp")
    Vaadområde2 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vaadomraade/vaadomraade_0002/vaadomraade.shp")
    Wateryarea <- rbind(Vaadområde0, Vaadområde1,Vaadområde2)
    rm(Vaadområde0, Vaadområde1,Vaadområde2)
    
    lake_distance$area <- sf::st_area(lake_distance)
    lake_distance$area <- as.numeric(lake_distance$area)
    
    lake_distance$geometry <- centroid_points <- sf::st_centroid(lake_distance$geometry)
    
    seq <- seq(0.00, 0.95, by = 0.05)
    
    #save in list
    list.dfs_lake <- list()
    
    for (i in seq) {
      # Create variable name
      df_name <- paste0("lake_distance_", (i+0.025))
      
      # Subset data 
      df <- subset(lake_distance, 
                   lake_distance$area > quantile(lake_distance$area, probs = i) & 
                     lake_distance$area < quantile(lake_distance$area, probs = (i + 0.025)))
      
      # Set CRS for the subset
      df <- sf::st_set_crs(df, sf::st_crs(Total_df))
      
      # Save data frame in list
      list.dfs_lake[[df_name]] <- df
    }
    rm(lake_distance, df)
    
    # Mangler vejkant pga. 160
    # vejkant0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vejkant/vejkant_0000/vejkant.shp")
    
    
    ## Vindmølle ----
    windturbine_distance <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vindmoelle/vindmoelle.shp")
    windturbine_distance <- sf::st_set_crs(windturbine_distance, sf::st_crs(Total_df))
    
    
# Add missing geographical variables ----
    
    
    # Change data frame to spatial object 
    Total_df <- st_as_sf(Total_df)
    # Set crs of Total_df
    Total_df <- sf::st_set_crs(Total_df, sf::st_crs(powerline_distance))
    print(sf::st_crs(Total_df))
    
    # Define column vector of postnr
    kommune_nr <- sort(unique(Total_df$postnr))
    
    ## Powerline distance loop ----
    for (i in kommune_nr) {
      cat(i, "\n")
      start_time <- Sys.time()
      
      # Subset the data
      subset_df <- Total_df[Total_df$postnr == i & is.na(Total_df$powerline_distance), ]
      
      if(nrow(subset_df) > 0) {
        # Calculate distances
        distances <- sf::st_distance(subset_df, powerline_distance)
        
        # Define the 'miin' function, or replace it with an appropriate function
        miin <- function(x) min(x, na.rm = TRUE)
        
        # Assign distances back to the correct rows in Total_df
        Total_df$powerline_distance[Total_df$postnr == i & is.na(Total_df$powerline_distance)] <- apply(distances, 1, miin)
      }
      
      
      cat("Time for municipality", i, ": ", Sys.time() - start_time, "\n")
    }

        ## Railway_distance loop ----
        for (i in kommune_nr) {
          cat(i, "\n")
          start_time <- Sys.time()
          
          # Subset the data
          subset_df <- Total_df[Total_df$postnr == i & is.na(Total_df$railway_distance), ]
          
          if(nrow(subset_df) > 0) {
            # Calculate distances
            distances <- sf::st_distance(subset_df, railway_distance)
            
            # Define the 'miin' function, or replace it with an appropriate function
            miin <- function(x) min(x, na.rm = TRUE)
            
            # Assign distances back to the correct rows in Total_df
            Total_df$railway_distance[Total_df$postnr == i & is.na(Total_df$railway_distance)] <- apply(distances, 1, miin)
          }
          
          
          cat("Time for municipality", i, ": ", Sys.time() - start_time, "\n")
        }

    ## coastline_distance loop ----
    for (i in kommune_nr) {
      cat(i, "\n")
      start_time <- Sys.time()
      
      # Subset the data
      subset_df <- Total_df[Total_df$postnr == i & is.na(Total_df$coastline_distance), ]
      
      if(nrow(subset_df) > 0) {
        # Calculate distances
        distances <- sf::st_distance(subset_df, coastline_distance)
        
        # Define the 'miin' function, or replace it with an appropriate function
        miin <- function(x) min(x, na.rm = TRUE)
        
        # Assign distances back to the correct rows in Total_df
        Total_df$coastline_distance[Total_df$postnr == i & is.na(Total_df$coastline_distance)] <- apply(distances, 1, miin)
      }
      
      
      cat("Time for municipality coastline", i, ": ", Sys.time() - start_time, "\n")
    }
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df5.Rdata")        
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df5.Rdata")
    
    
    ## forest_distance ----
    
    ### 1_5 forest_distance ----
    kommune_nr <- sort(unique(Total_df$postnr))
  
    for (d in 1:5) {
      # Get the data frame at position 'd' in the list
      df <- list.dfs_forest[[d]]
      d_start_time <- Sys.time()
      
      # Add new column to frame
      Total_df[, ncol(Total_df) + 1] <- NA
      #edit name of column
      colnames(Total_df)[ncol(Total_df)] <- paste0("forest_distance_", d)
      
      
      for (i in kommune_nr) {
        cat(i, "\n")
        start_time <- Sys.time()
        
        # Subset the data
        subset_df <- subset(Total_df, postnr == i) 
        
        
        if(nrow(subset_df) > 0) {
          # Calculate distances
          distances <- sf::st_distance(subset_df, df)
          
          # Define the 'miin' function, or replace it with an appropriate function
          miin <- function(x) min(x, na.rm = TRUE)
          
          # Calculate minimum distances
          min_distances <- apply(distances, 1, miin)
          
          # put in existign data frame
          Total_df[Total_df$postnr == i, ncol(Total_df)] <- min_distances
          
        }
        
        cat("Time for municipality Forest", i, ": ", "& it:", d, Sys.time() - start_time, "\n")
      }
      cat("Time for", d, "=", Sys.time() - d_start_time )
    }
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_1_5.Rdata")                
  
    ### 6_10 forest_distance ----
    kommune_nr <- sort(unique(Total_df$postnr))
    
    for (d in 6:10) {
      # Get the data frame at position 'd' in the list
      df <- list.dfs_forest[[d]]
      d_start_time <- Sys.time()
      
      # Add new column to frame
      Total_df[, ncol(Total_df) + 1] <- NA
      #edit name of column
      colnames(Total_df)[ncol(Total_df)] <- paste0("forest_distance_", d)
      
      
      for (i in kommune_nr) {
        cat(i, "\n")
        start_time <- Sys.time()
        
        # Subset the data
        subset_df <- subset(Total_df, postnr == i) 
        
        
        if(nrow(subset_df) > 0) {
          # Calculate distances
          distances <- sf::st_distance(subset_df, df)
          
          # Define the 'miin' function, or replace it with an appropriate function
          miin <- function(x) min(x, na.rm = TRUE)
          
          # Calculate minimum distances
          min_distances <- apply(distances, 1, miin)
          
          # put in existign data frame
          Total_df[Total_df$postnr == i, ncol(Total_df)] <- min_distances
          
        }
        
        cat("Time for municipality Forest", i, ": ", "& it:", d, Sys.time() - start_time, "\n")
      }
      cat("Time for", d, "=", Sys.time() - d_start_time )
    }
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_6_10.Rdata")                
    
    ### 11_15 forest_distance ----
    kommune_nr <- sort(unique(Total_df$postnr))
    
    for (d in 11:15) {
      # Get the data frame at position 'd' in the list
      df <- list.dfs_forest[[d]]
      d_start_time <- Sys.time()
      
      # Add new column to frame
      Total_df[, ncol(Total_df) + 1] <- NA
      #edit name of column
      colnames(Total_df)[ncol(Total_df)] <- paste0("forest_distance_", d)
      
      
      for (i in kommune_nr) {
        cat(i, "\n")
        start_time <- Sys.time()
        
        # Subset the data
        subset_df <- subset(Total_df, postnr == i) 
        
        
        if(nrow(subset_df) > 0) {
          # Calculate distances
          distances <- sf::st_distance(subset_df, df)
          
          # Define the 'miin' function, or replace it with an appropriate function
          miin <- function(x) min(x, na.rm = TRUE)
          
          # Calculate minimum distances
          min_distances <- apply(distances, 1, miin)
          
          # put in existign data frame
          Total_df[Total_df$postnr == i, ncol(Total_df)] <- min_distances
          
        }
        
        cat("Time for municipality Forest", i, ": ", "& it:", d, Sys.time() - start_time, "\n")
      }
      cat("Time for", d, "=", Sys.time() - d_start_time )
    }
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_11_15.Rdata")                
    
    ### 16_20 forest_distance ----
    kommune_nr <- sort(unique(Total_df$postnr))
    
    for (d in 16:20) {
      # Get the data frame at position 'd' in the list
      df <- list.dfs_forest[[d]]
      d_start_time <- Sys.time()
      
      # Add new column to frame
      Total_df[, ncol(Total_df) + 1] <- NA
      #edit name of column
      colnames(Total_df)[ncol(Total_df)] <- paste0("forest_distance_", d)
      
      
      for (i in kommune_nr) {
        cat(i, "\n")
        start_time <- Sys.time()
        
        # Subset the data
        subset_df <- subset(Total_df, postnr == i) 
        
        
        if(nrow(subset_df) > 0) {
          # Calculate distances
          distances <- sf::st_distance(subset_df, df)
          
          # Define the 'miin' function, or replace it with an appropriate function
          miin <- function(x) min(x, na.rm = TRUE)
          
          # Calculate minimum distances
          min_distances <- apply(distances, 1, miin)
          
          # put in existign data frame
          Total_df[Total_df$postnr == i, ncol(Total_df)] <- min_distances
          
        }
        
        cat("Time for municipality Forest", i, ": ", "& it:", d, Sys.time() - start_time, "\n")
      }
      cat("Time for", d, "=", Sys.time() - d_start_time )
    }
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_16_20.Rdata")                
    
    ## lake_distance ----
    
    ### 1_5 lake ----
    kommune_nr <- sort(unique(Total_df$postnr))
    
    for (d in 1:5) {
      # Get the data frame at position 'd' in the list
      df <- list.dfs_lake[[d]]
      d_start_time <- Sys.time()
      
      # Add new column to frame
      Total_df[, ncol(Total_df) + 1] <- NA
      #edit name of column
      colnames(Total_df)[ncol(Total_df)] <- paste0("lake_distance_", d)
      
      
      for (i in kommune_nr) {
        cat(i, "\n")
        start_time <- Sys.time()
        
        # Subset the data
        subset_df <- subset(Total_df, postnr == i) 
        
        
        if(nrow(subset_df) > 0) {
          # Calculate distances
          distances <- sf::st_distance(subset_df, df)
          
          # Define the 'miin' function, or replace it with an appropriate function
          miin <- function(x) min(x, na.rm = TRUE)
          
          # Calculate minimum distances
          min_distances <- apply(distances, 1, miin)
          
          # put in existign data frame
          Total_df[Total_df$postnr == i, ncol(Total_df)] <- min_distances
          
        }
        
        cat("Time for municipality lake", i, ": ", "& it:", d, Sys.time() - start_time, "\n")
      }
      cat("Time for", d, "=", Sys.time() - d_start_time )
    }
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_1_5.Rdata")                
    
    ### 6_10 lake_distance ----
    kommune_nr <- sort(unique(Total_df$postnr))
    
    for (d in 6:10) {
      # Get the data frame at position 'd' in the list
      df <- list.dfs_lake[[d]]
      d_start_time <- Sys.time()
      
      # Add new column to frame
      Total_df[, ncol(Total_df) + 1] <- NA
      #edit name of column
      colnames(Total_df)[ncol(Total_df)] <- paste0("lake_distance_", d)
      
      
      for (i in kommune_nr) {
        cat(i, "\n")
        start_time <- Sys.time()
        
        # Subset the data
        subset_df <- subset(Total_df, postnr == i) 
        
        
        if(nrow(subset_df) > 0) {
          # Calculate distances
          distances <- sf::st_distance(subset_df, df)
          
          # Define the 'miin' function, or replace it with an appropriate function
          miin <- function(x) min(x, na.rm = TRUE)
          
          # Calculate minimum distances
          min_distances <- apply(distances, 1, miin)
          
          # put in existign data frame
          Total_df[Total_df$postnr == i, ncol(Total_df)] <- min_distances
          
        }
        
        cat("Time for municipality lake", i, ": ", "& it:", d, Sys.time() - start_time, "\n")
      }
      cat("Time for", d, "=", Sys.time() - d_start_time )
    }
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_6_10.Rdata")                
    
    ### 11_15 lake_distance ----
    kommune_nr <- sort(unique(Total_df$postnr))
    
    for (d in 11:15) {
      # Get the data frame at position 'd' in the list
      df <- list.dfs_lake[[d]]
      d_start_time <- Sys.time()
      
      # Add new column to frame
      Total_df[, ncol(Total_df) + 1] <- NA
      #edit name of column
      colnames(Total_df)[ncol(Total_df)] <- paste0("lake_distance_", d)
      
      
      for (i in kommune_nr) {
        cat(i, "\n")
        start_time <- Sys.time()
        
        # Subset the data
        subset_df <- subset(Total_df, postnr == i) 
        
        
        if(nrow(subset_df) > 0) {
          # Calculate distances
          distances <- sf::st_distance(subset_df, df)
          
          # Define the 'miin' function, or replace it with an appropriate function
          miin <- function(x) min(x, na.rm = TRUE)
          
          # Calculate minimum distances
          min_distances <- apply(distances, 1, miin)
          
          # put in existign data frame
          Total_df[Total_df$postnr == i, ncol(Total_df)] <- min_distances
          
        }
        
        cat("Time for municipality lake", i, ": ", "& it:", d, Sys.time() - start_time, "\n")
      }
      cat("Time for", d, "=", Sys.time() - d_start_time )
    }
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_11_15.Rdata")                
    
    ### 16_20 lake_distance ----
    kommune_nr <- sort(unique(Total_df$postnr))
    
    for (d in 16:20) {
      # Get the data frame at position 'd' in the list
      df <- list.dfs_lake[[d]]
      d_start_time <- Sys.time()
      
      # Add new column to frame
      Total_df[, ncol(Total_df) + 1] <- NA
      #edit name of column
      colnames(Total_df)[ncol(Total_df)] <- paste0("lake_distance_", d)
      
      
      for (i in kommune_nr) {
        cat(i, "\n")
        start_time <- Sys.time()
        
        # Subset the data
        subset_df <- subset(Total_df, postnr == i) 
        
        
        if(nrow(subset_df) > 0) {
          # Calculate distances
          distances <- sf::st_distance(subset_df, df)
          
          # Define the 'miin' function, or replace it with an appropriate function
          miin <- function(x) min(x, na.rm = TRUE)
          
          # Calculate minimum distances
          min_distances <- apply(distances, 1, miin)
          
          # put in existign data frame
          Total_df[Total_df$postnr == i, ncol(Total_df)] <- min_distances
          
        }
        
        cat("Time for municipality lake", i, ": ", "& it:", d, Sys.time() - start_time, "\n")
      }
      cat("Time for", d, "=", Sys.time() - d_start_time )
    }
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_16_20.Rdata")                
    
    
    ## Trainstation_distance ----
    kommune_nr <- sort(unique(Total_df$postnr))
    
    for (i in kommune_nr) {
      cat(i, "\n")
      start_time <- Sys.time()
      
      # Subset the data
      subset_df <- Total_df[Total_df$postnr == i & is.na(Total_df$trainstation_distance), ]
      
      if(nrow(subset_df) > 0) {
        # Calculate distances
        distances <- sf::st_distance(subset_df, trainstation_distance)
        
        # Define the 'miin' function, or replace it with an appropriate function
        miin <- function(x) min(x, na.rm = TRUE)
        
        # Assign distances back to the correct rows in Total_df
        Total_df$trainstation_distance[Total_df$postnr == i & is.na(Total_df$trainstation_distance)] <- apply(distances, 1, miin)
      }
      
      
      cat("Time for municipality", i, ": ", Sys.time() - start_time, "\n")
    }
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_trainstation.Rdata")                
    
    
    ## Wateryarea ----
    kommune_nr <- sort(unique(Total_df$postnr))
    
    for (i in kommune_nr) {
      cat(i, "\n")
      start_time <- Sys.time()
      
      # Subset the data
      subset_df <- Total_df[Total_df$postnr == i & is.na(Total_df$wateryarea_distance), ]
      
      if(nrow(subset_df) > 0) {
        # Calculate distances
        distances <- sf::st_distance(subset_df, wateryarea_distance)
        
        # Define the 'miin' function, or replace it with an appropriate function
        miin <- function(x) min(x, na.rm = TRUE)
        
        # Assign distances back to the correct rows in Total_df
        Total_df$wateryarea_distance[Total_df$postnr == i & is.na(Total_df$wateryarea_distance)] <- apply(distances, 1, miin)
      }
      
      
      cat("Time for municipality", i, ": ", Sys.time() - start_time, "\n")
    }
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_wateryarea.Rdata")                
    
    
    ## Windmill ----
    kommune_nr <- sort(unique(Total_df$postnr))
    
    for (i in kommune_nr) {
      cat(i, "\n")
      start_time <- Sys.time()
      
      # Subset the data
      subset_df <- Total_df[Total_df$postnr == i & is.na(Total_df$windturbine_distance), ]
      
      if(nrow(subset_df) > 0) {
        # Calculate distances
        distances <- sf::st_distance(subset_df, windturbine_distance)
        
        # Define the 'miin' function, or replace it with an appropriate function
        miin <- function(x) min(x, na.rm = TRUE)
        
        # Assign distances back to the correct rows in Total_df
        Total_df$windturbine_distance[Total_df$postnr == i & is.na(Total_df$windturbine_distance)] <- apply(distances, 1, miin)
      }
      
      
      cat("Time for municipality", i, ": ", Sys.time() - start_time, "\n")
    }
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_windturbine.Rdata")                
    
  # Combine after splitting (Split due to large computational time) ----  
    ## Forest ----
    ### Load df from after powerline, railway_distance, coastline. 
    T1 <- Sys.time()
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df5.Rdata")
    Total_df_5 <- Total_df
    
    # Load first forest
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_1_5.Rdata")                
    Total_df_1_5 <- Total_df
    
    ### Merge 1_5 ----
    Starttime <- Sys.time()
    Total_df_6 <- Total_df_5 %>% 
      dplyr::bind_cols(select(Total_df_1_5, forest_distance_1, forest_distance_2, forest_distance_3, 
                              forest_distance_4, forest_distance_5), by = c("Coor" = "Coor"))
    endtime <- Sys.time()-Starttime
    print(endtime)
    
    ## Take minimum from partitioned forest distance
    Total_df_6 <- Total_df_6 %>% 
      rowwise() %>%
      mutate(forest_distance = min(forest_distance_1, forest_distance_2, forest_distance_3, 
                                   forest_distance_4, forest_distance_5))
    
    # Load second forest 
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_6_10.Rdata")                
    Total_df_6_10 <- Total_df 
    
    ### Merge 6_10 ----
    Starttime <- Sys.time()
    Total_df_6 <- Total_df_6 %>% 
      dplyr::bind_cols(select(Total_df_6_10, forest_distance_6, forest_distance_7, forest_distance_8, 
                              forest_distance_9, forest_distance_10), by = c("Coor" = "Coor"))
    endtime <- Sys.time()-Starttime
    print(endtime)
    
    ## Take minimum from partitioned forest distance
    Total_df_6 <- Total_df_6 %>% 
      rowwise() %>%
      mutate(forest_distance = min(forest_distance, forest_distance_6, forest_distance_7, forest_distance_8, 
                                   forest_distance_9, forest_distance_10))
    
    # Load third forest 
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_11_15.Rdata")                
    Total_df_11_15 <- Total_df
    
    ### Merge 11_15 ----
    Starttime <- Sys.time()
    Total_df_6 <- Total_df_6 %>% 
      dplyr::bind_cols(select(Total_df_11_15, forest_distance_11, forest_distance_12, forest_distance_13, 
                              forest_distance_14, forest_distance_15), by = c("Coor" = "Coor"))
    endtime <- Sys.time()-Starttime
    print(endtime)
    
    ## Take minimum from partitioned forest distance
    Total_df_6 <- Total_df_6 %>% 
      rowwise() %>%
      mutate(forest_distance = min(forest_distance, forest_distance_11, forest_distance_12, forest_distance_13, 
                                   forest_distance_14, forest_distance_15))
    
    # Load fourth forest 
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_16_20.Rdata")                
    Total_df_16_20 <- Total_df  
    
    ### Merge 16_20 ----
    Starttime <- Sys.time()
    Total_df_6 <- Total_df_6 %>% 
      dplyr::bind_cols(select(Total_df_16_20, forest_distance_16, forest_distance_17, forest_distance_18, 
                              forest_distance_19, forest_distance_20), by = c("Coor" = "Coor"))
    endtime <- Sys.time()-Starttime
    print(endtime)
    
    ## Take minimum from partitioned forest distance
    Total_df_6 <- Total_df_6 %>% 
      rowwise() %>%
      mutate(forest_distance = min(forest_distance,  forest_distance_16, forest_distance_17, forest_distance_18, 
                                   forest_distance_19, forest_distance_20))
    
    
    T2 <- Sys.time()-T1
    print(T2)
    rm(Total_df_1_5, Total_df_11_15, Total_df_16_20, Total_df_6_10)
    
    ## Lake ----
    ### Load df from after powerline, railway_distance, coastline. 
    T1 <- Sys.time()
    
    # Load first lake
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_1_5.Rdata")                
    Total_df_1_5 <- Total_df
    
    ### Merge 1_5 ----
    Starttime <- Sys.time()
    Total_df_6 <- Total_df_6 %>% 
      dplyr::bind_cols(select(Total_df_1_5, lake_distance_1, lake_distance_2, lake_distance_3, 
                              lake_distance_4, lake_distance_5), by = c("Coor" = "Coor"))
    endtime <- Sys.time()-Starttime
    print(endtime)
    
    ## Take minimum from partitioned lake distance
    Total_df_6 <- Total_df_6 %>% 
      rowwise() %>%
      mutate(lake_distance = min(lake_distance_1, lake_distance_2, lake_distance_3, 
                                   lake_distance_4, lake_distance_5))
    
    # Load second lake 
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_6_10.Rdata")                
    Total_df_6_10 <- Total_df 
    
    ### Merge 6_10 ----
    Starttime <- Sys.time()
    Total_df_6 <- Total_df_6 %>% 
      dplyr::bind_cols(select(Total_df_6_10, lake_distance_8, 
                              lake_distance_9, lake_distance_10), by = c("Coor" = "Coor"))
    endtime <- Sys.time()-Starttime
    print(endtime)
    
    ## Take minimum from partitioned lake distance
    Total_df_6 <- Total_df_6 %>% 
      rowwise() %>%
      mutate(lake_distance = min(lake_distance, lake_distance_8, 
                                   lake_distance_9, lake_distance_10))
    
    # Load third lake 
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_11_15.Rdata")                
    Total_df_11_15 <- Total_df
    
    ### Merge 11_15 ----
    Starttime <- Sys.time()
    Total_df_6 <- Total_df_6 %>% 
      dplyr::bind_cols(select(Total_df_11_15, lake_distance_11, lake_distance_12, lake_distance_13, 
                              lake_distance_14, lake_distance_15), by = c("Coor" = "Coor"))
    endtime <- Sys.time()-Starttime
    print(endtime)
    
    ## Take minimum from partitioned lake distance
    Total_df_6 <- Total_df_6 %>% 
      rowwise() %>%
      mutate(lake_distance = min(lake_distance, lake_distance_11, lake_distance_12, lake_distance_13, 
                                   lake_distance_14, lake_distance_15))
    
    # Load fourth lake 
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_16_20.Rdata")                
    Total_df_16_20 <- Total_df  
    
    ### Merge 16_20 ----
    Starttime <- Sys.time()
    Total_df_6 <- Total_df_6 %>% 
      dplyr::bind_cols(select(Total_df_16_20, lake_distance_16, lake_distance_17, lake_distance_18, 
                              lake_distance_19, lake_distance_20), by = c("Coor" = "Coor"))
    endtime <- Sys.time()-Starttime
    print(endtime)
    
    ## Take minimum from partitioned lake distance
    Total_df_6 <- Total_df_6 %>% 
      rowwise() %>%
      mutate(lake_distance = min(lake_distance,  lake_distance_16, lake_distance_17, lake_distance_18, 
                                   lake_distance_19, lake_distance_20))
    
    
    T2 <- Sys.time()-T1
    print(T2)    
    
    rm(Total_df_1_5, Total_df_11_15, Total_df_16_20, Total_df_6_10)
    
    save(Total_df_6, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df6.Rdata")        
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df6.Rdata")
    
    # Get rid of all new unnecessary variables
    names <- colnames(Total_df_6[, 1:71])
    Total_df_6 <- subset(Total_df_6, select = names)
  
    ## Trainstation ----    
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_trainstation.Rdata")                
    Starttime <- Sys.time()
    Total_df_6 <- Total_df_6 %>% 
      dplyr::bind_cols(select(Total_df, trainstation_distance), by = c("Coor" = "Coor"))
    endtime <- Sys.time()-Starttime
    print(endtime)
    
    ## Wateryarea ----    
    Starttime <- Sys.time()
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_wateryarea.Rdata")             
    Total_df_6 <- Total_df_6 %>% 
      dplyr::bind_cols(select(Total_df, Wateryarea_distance_1, Wateryarea_distance_2,
                              Wateryarea_distance_3, Wateryarea_distance_4), by = c("Coor" = "Coor"))
    endtime <- Sys.time()-Starttime
    print(endtime)
    
    ## Take minimum from partitioned lake distance
    Total_df_6 <- Total_df_6 %>% 
      rowwise() %>%
      mutate(Wateryarea_distance = min(Wateryarea_distance_1, Wateryarea_distance_2,
                                       Wateryarea_distance_3, Wateryarea_distance_4))
    
    ## Windmill ----    
    Starttime <- Sys.time()
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_windturbine.Rdata")             
    Total_df_6 <- Total_df_6 %>% 
      dplyr::bind_cols(select(Total_df, windturbine_distance, windturbine_height), by = c("Coor" = "Coor"))
    endtime <- Sys.time()-Starttime
    print(endtime)
    
    Total_df_7 <- Total_df_6
    save(Total_df_7, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_7.Rdata")
    colnames(Total_df_7)[71] <- "Coor"
    colnames(Total_df_7)[72] <- "Trainstation_distance"
    
    
    var_remove <- c("trainstation_distance...59", "windturbine_height...62", "windturbine_distance...61", "Coor...73",
                    "by...74", "Wateryarea_distance_1", "Wateryarea_distance_2", "Wateryarea_distance_3", 
                    "Wateryarea_distance_4", "windturbine_height...83", "Coor...84", "by...85", "by...80", "by...74", 
                    "windturbine_distance...82", "Coor...79")
    Total_df_8 <- Total_df_7[, !(names(Total_df_7) %in% var_remove)]
    save(Total_df_8, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_8.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_8.Rdata")

# Spatial variable ----
    Total_df_8 <- sf::st_as_sf(Total_df_8)
    #Retrieve coordinates
    
    kommune_nr <- sort(unique(Total_df_8$postnr))
    Total_df_8$rownumber <- row.names(Total_df_8)
    Total_df_9 <- data.frame(matrix(nrow = 0, ncol = ncol(Total_df_8)))
    colnames(Total_df_9) <- colnames(Total_df_8)
    
    
    for(i in kommune_nr){
      start_time <- Sys.time()
      df_s <- subset(Total_df_8, postnr == i)
      df_s <-  subset(df_s, select = c(Coor, rownumber))
      row.names(df_s) <- df_s$rownumber
      
      df_d <- df_s %>% 
        dplyr::distinct(Coor, .keep_all = TRUE)
      
      rownames <- df_d$rownumber
      
      df_d <- as.data.frame(df_d[,1])
      rownames(df_d) <- rownames
      
      if(nrow(df_d) > 200) {
      # do neighbors
      df_neighbor <- spdep::tri2nb(df_d$Coor)
      
      # Store neighbor in new variable
      df_d[, ncol(df_d) + 1] <- NA
      colnames(df_d)[ncol(df_d)] <- "neighbor"
      
             # Insert values from list to df 
             for (j in 1:length(df_neighbor)){
               output_string <- paste(df_neighbor[[j]], collapse = ", ")
               df_d[j, 2] <- output_string
             }
      
      } else if (nrow(df_d) < 200 & nrow(df_d) > 5) {
        # do neighbors
        df_d$n <- seq(from = 1, to = nrow(df_d))
        df_neighbor <- spdep::knn2nb(spdep::knearneigh(df_d$Coor, k = 5))
        
        
        # Store neighbor in new variable
        df_d[, ncol(df_d) + 1] <- NA
        colnames(df_d)[ncol(df_d)] <- "n1"
        df_d[, ncol(df_d) + 1] <- NA
        colnames(df_d)[ncol(df_d)] <- "n2"
        df_d[, ncol(df_d) + 1] <- NA
        colnames(df_d)[ncol(df_d)] <- "n3"
        df_d[, ncol(df_d) + 1] <- NA
        colnames(df_d)[ncol(df_d)] <- "n4"
        df_d[, ncol(df_d) + 1] <- NA
        colnames(df_d)[ncol(df_d)] <- "n5"
        df_d$rownames <- row.names(df_d)
        df_d[, ncol(df_d) + 1] <- NA
        colnames(df_d)[ncol(df_d)] <- "neighbor"

        
               # Insert values from list to df 
               for (j in 1:length(df_neighbor)){
                 char_vector <- as.character(df_neighbor[[j]])
                 df_d[j, "n1"] <- strsplit(char_vector, " ")[[1]]
                 df_d[j, "n2"] <- strsplit(char_vector, " ")[[2]]
                 df_d[j, "n3"] <- strsplit(char_vector, " ")[[3]]
                 df_d[j, "n4"] <- strsplit(char_vector, " ")[[4]]
                 df_d[j, "n5"] <- strsplit(char_vector, " ")[[5]]
                 
                 df_d[j, "n1"] <- df_d$rownames[match(df_d[j, "n1"], df_d$n)]
                 df_d[j, "n2"] <- df_d$rownames[match(df_d[j, "n2"], df_d$n)]
                 df_d[j, "n3"] <- df_d$rownames[match(df_d[j, "n3"], df_d$n)]
                 df_d[j, "n4"] <- df_d$rownames[match(df_d[j, "n4"], df_d$n)]
                 df_d[j, "n5"] <- df_d$rownames[match(df_d[j, "n5"], df_d$n)]
                 
                 
                 df_d[j, "neighbor"] <- paste0(df_d[j, "n1"], ", ", df_d[j, "n2"], ", ", df_d[j, "n3"], 
                                                              ", ", df_d[j, "n4"], ", ", df_d[j, "n5"])
                   }
        
                df_d <- subset(df_d, select = c(Coor, neighbor))
      } else {
              next
            }
     
       # Merge on existing
      ## Define subset again to bind on rows 
      df_s <- subset(Total_df_8, postnr == i)
      df_s <- dplyr::left_join(df_s, df_d, by = "Coor")
      Total_df_9 <- rbind(Total_df_9, df_s)
      
      cat("Time for municipality", i, ": ", Sys.time() - start_time, "\n")
      
    }
  
    save(Total_df_9, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_9.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_9.Rdata")
    
 ## Neighbor spatial lag variable ----
    Total_df_9$neighbor_list <- strsplit(Total_df_9$neighbor, ", ")
    Total_df_9$lag_price <- NA
    # Convert neighbor column to a list of value
    
    # Function to calculate lag_price
    calculate_lag_price <- function(i) {
      nb_row_numbers <- as.integer(unlist(Total_df_9$neighbor_list[i]))
      nb_sales_prices <- Total_df_9$nominal_price[nb_row_numbers]
      current_sales_price <- Total_df_9$nominal_price[i]
      nb_sales_dates <- Total_df_9$dato.x[nb_row_numbers]
      
      # Filter neighbor prices in relation to observed pris
      valid_prices <- nb_sales_prices[nb_sales_dates < Total_df_9$dato.x[i]]
      
      # Calculate mean if there are valid prices
      if (length(valid_prices) > 0) {
        return(mean(valid_prices))
      } else {
        return(NA)
      }
    }
    
    # Apply 
    Total_df_9$lag_price <- pbapply::pblapply(1:nrow(Total_df_9), calculate_lag_price)
    
    Total_df_10 <- Total_df_9
    
    save(Total_df_10, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_10.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_10.Rdata")
  
    # Group by zip code and then order street name to retrieve information
    Total_df_10 <- Total_df_10 %>%
                    group_by(postnr) %>%
                    arrange(vejnavn)
    
    Total_df_10 <- Total_df_10 %>%
      arrange(sf::st_coordinates(.)[, "X"], sf::st_coordinates(.)[, "Y"])

    Total_df_10$nabolag <- NA
    nabolag <- function(i){
      seq <- seq(from = i-15, to = i + 15, by = 1)
      seq <- seq[seq>0]
      seq <- seq[seq<2032886]
      rows <- paste(seq, collapse = ", ")
      # output_string <- paste(df_neighbor[[j]], collapse = ", ")
      return(rows)
    }
    Total_df_10$nabolag <- pbapply::pblapply(1:nrow(Total_df_10), nabolag)
    Total_df_10$nabolag <- as.character(Total_df_10$nabolag)
    
    ## Apply for missing obs take surrounding neighbors 
    Total_df_10$nabolag_list <- strsplit(Total_df_10$nabolag, ", ")
    Total_df_10$lag_price1 <- NA
    # Convert neighbor column to a list of value
    
    # Function to calculate lag_price
    calculate_lag_price1 <- function(i) {
      nb_row_numbers <- as.integer(unlist(Total_df_10$nabolag_list[i]))
      nb_sales_prices <- Total_df_10$nominal_price[nb_row_numbers]
      current_sales_price <- Total_df_10$nominal_price[i]
      nb_sales_dates <- Total_df_10$dato.x[nb_row_numbers]
      
      # Filter neighbor prices in relation to observed pris
      valid_prices <- nb_sales_prices[nb_sales_dates < Total_df_10$dato.x[i]]
      
      # Calculate mean if there are valid prices
      if (length(valid_prices) > 0) {
        return(mean(valid_prices))
      } else {
        return(NA)
      }
    }
    
    # apply for the rest
    Total_df_10$lag_price1 <- pbapply::pblapply(1:nrow(Total_df_10), calculate_lag_price1)
    
    ## Replace if lag price is na 
    Total_df_10 <- Total_df_10 %>%
      mutate(lag_price = ifelse(is.na(lag_price), lag_price1, lag_price))
    
    Total_df_11 <- Total_df_10
    save(Total_df_11, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_11.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_11.Rdata")
    
    
    ## Difference in Difference variable ----
    #Tre variable for hver oversvømmelse, men hvis event er sket efter salgs skal den være NA
    Events <- as.data.frame(table(Total_df_11$Hændelsesdato))
    Events$Var1 <- as.Date(Events$Var1)
    
    Total_df_11 <- Total_df_11 %>%
      mutate(EV1 = ifelse(dato.x < Events[1,1], 0, 1))
    Total_df_11 <- Total_df_11 %>%
                    rowwise %>%
                    mutate(EV1 = ifelse(is.na(EV1), 0, EV1))
      
    Total_df_11 <- Total_df_11 %>%
      mutate(EV2 = ifelse(dato.x > Events[1,1] & dato.x < Events[2,1], 0, 1))
                     Total_df_11 <- Total_df_11 %>%
                     rowwise %>%
                     mutate(EV2 = ifelse(is.na(EV2), 0, EV2))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(EV3 = ifelse(dato.x > Events[2,1] & dato.x < Events[3,1], 0, 1))
                     Total_df_11 <- Total_df_11 %>%
                       rowwise %>%
                       mutate(EV3 = ifelse(is.na(EV3), 0, EV3))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(EV4 = ifelse(dato.x > Events[3,1] & dato.x < Events[4,1], 0, 1))
    Total_df_11 <- Total_df_11 %>%
                     rowwise %>%
                     mutate(EV4 = ifelse(is.na(EV4), 0, EV4))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(EV5 = ifelse(dato.x > Events[4,1] & dato.x < Events[5,1], 0, 1))
    Total_df_11 <- Total_df_11 %>%
                     rowwise %>%
                     mutate(EV5 = ifelse(is.na(EV5), 0, EV5))
    
    ## Sold after event variable 
    ### EV1 ----
    Total_df_11 <- Total_df_11 %>%
             mutate(Sold_Ev1_0_0.5 = ifelse(dato.x > Events[1,1] & dato.x < Events[1,1] + 365/2, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev1_0_0.5 = ifelse(is.na(Sold_Ev1_0_0.5), 0, Sold_Ev1_0_0.5))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev1_1 = ifelse(dato.x > Events[1,1] & dato.x < Events[1,1] + 365, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev1_1 = ifelse(is.na(Sold_Ev1_1), 0, Sold_Ev1_1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev1_2 = ifelse(dato.x > Events[1,1] & dato.x < Events[1,1] + 365*2, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev1_2 = ifelse(is.na(Sold_Ev1_2), 0, Sold_Ev1_2))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev1_5 = ifelse(dato.x > Events[1,1] & dato.x < Events[1,1] + 365*5, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev1_5 = ifelse(is.na(Sold_Ev1_5), 0, Sold_Ev1_5))
    
    ### EV2 ----
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev2_0_0.5 = ifelse(dato.x > Events[2,1] & dato.x < Events[2,1] + 365/2, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev2_0_0.5 = ifelse(is.na(Sold_Ev2_0_0.5), 0, Sold_Ev2_0_0.5))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev2_1 = ifelse(dato.x > Events[2,1] & dato.x < Events[2,1] + 365, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev2_1 = ifelse(is.na(Sold_Ev2_1), 0, Sold_Ev2_1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev2_2 = ifelse(dato.x > Events[2,1] & dato.x < Events[2,1] + 365*2, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev2_2 = ifelse(is.na(Sold_Ev2_2), 0, Sold_Ev2_2))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev2_5 = ifelse(dato.x > Events[2,1] & dato.x < Events[2,1] + 365*5, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev2_5 = ifelse(is.na(Sold_Ev2_5), 0, Sold_Ev2_5))
    
    
    ### EV3 ----
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev3_0_0.5 = ifelse(dato.x > Events[3,1] & dato.x < Events[3,1] + 365/2, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev3_0_0.5 = ifelse(is.na(Sold_Ev3_0_0.5), 0, Sold_Ev3_0_0.5))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev3_1 = ifelse(dato.x > Events[3,1] & dato.x < Events[3,1] + 365, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev3_1 = ifelse(is.na(Sold_Ev3_1), 0, Sold_Ev3_1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev3_2 = ifelse(dato.x > Events[3,1] & dato.x < Events[3,1] + 365*2, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev3_2 = ifelse(is.na(Sold_Ev3_2), 0, Sold_Ev3_2))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev3_5 = ifelse(dato.x > Events[3,1] & dato.x < Events[3,1] + 365*5, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev3_5 = ifelse(is.na(Sold_Ev3_5), 0, Sold_Ev3_5))
    
    ### EV4 ----
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev4_0_0.5 = ifelse(dato.x > Events[4,1] & dato.x < Events[4,1] + 365/2, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev4_0_0.5 = ifelse(is.na(Sold_Ev4_0_0.5), 0, Sold_Ev4_0_0.5))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev4_1 = ifelse(dato.x > Events[4,1] & dato.x < Events[4,1] + 365, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev4_1 = ifelse(is.na(Sold_Ev4_1), 0, Sold_Ev4_1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev4_2 = ifelse(dato.x > Events[4,1] & dato.x < Events[4,1] + 365*2, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev4_2 = ifelse(is.na(Sold_Ev4_2), 0, Sold_Ev4_2))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev4_5 = ifelse(dato.x > Events[4,1] & dato.x < Events[4,1] + 365*5, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev4_5 = ifelse(is.na(Sold_Ev4_5), 0, Sold_Ev4_5))
    
    ### EV5 ----
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev5_0_0.5 = ifelse(dato.x > Events[5,1] & dato.x < Events[5,1] + 365/2, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev5_0_0.5 = ifelse(is.na(Sold_Ev5_0_0.5), 0, Sold_Ev5_0_0.5))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev5_1 = ifelse(dato.x > Events[5,1] & dato.x < Events[5,1] + 365, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev5_1 = ifelse(is.na(Sold_Ev5_1), 0, Sold_Ev5_1))
    
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev5_2 = ifelse(dato.x > Events[5,1] & dato.x < Events[5,1] + 365*2, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev5_2 = ifelse(is.na(Sold_Ev5_2), 0, Sold_Ev5_2))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev5_5 = ifelse(dato.x > Events[5,1] & dato.x < Events[5,1] + 365*5, 0, 1))
    Total_df_11 <- Total_df_11 %>%
      rowwise %>%
      mutate(Sold_Ev5_5 = ifelse(is.na(Sold_Ev5_5), 0, Sold_Ev5_5))
    
    # combine variables with sold after 
    Total_df_11 <- Total_df_11 %>%
      rowwise() %>%
      mutate(Sold_0_0.5 = sum(Sold_Ev1_0_0.5, Sold_Ev2_0_0.5, Sold_Ev3_0_0.5, Sold_Ev4_0_0.5, Sold_Ev5_0_0.5))
    Total_df_11 <- Total_df_11 %>%
      rowwise() %>%
      mutate(Sold_1 = sum(Sold_Ev1_1, Sold_Ev2_1, Sold_Ev3_1, Sold_Ev4_1, Sold_Ev5_1))
    Total_df_11 <- Total_df_11 %>%
      rowwise() %>%
      mutate(Sold_2 = sum(Sold_Ev1_2, Sold_Ev2_2, Sold_Ev3_2, Sold_Ev4_2, Sold_Ev5_2))
    Total_df_11 <- Total_df_11 %>%
      rowwise() %>%
      mutate(Sold_5 = sum(Sold_Ev1_5, Sold_Ev2_5, Sold_Ev3_5, Sold_Ev4_5, Sold_Ev5_5))

    
    
# Subsetting ---- 
    varDelete <- c("EPSG", "Geometri_EPSG_25832", "y_coord", "x_coord", "latitude", "longitude", 
                   "neighbor_list", "rownumber", "neighbor", "nabolag",
                   "Sold_Ev1_0_0.5", "Sold_Ev2_0_0.5", "Sold_Ev3_0_0.5", "Sold_Ev4_0_0.5", "Sold_Ev5_0_0.5",
                   "Sold_Ev1_1", "Sold_Ev2_1", "Sold_Ev3_1", "Sold_Ev4_1", "Sold_Ev5_1",
                   "Sold_Ev1_2", "Sold_Ev2_2", "Sold_Ev3_2", "Sold_Ev4_2", "Sold_Ev5_2",
                   "Sold_Ev1_5", "Sold_Ev2_5", "Sold_Ev3_5", "Sold_Ev4_5", "Sold_Ev5_5")
    Total_df_11 <- Total_df_11[, !(names(Total_df_11) %in% varDelete)]
    
    Total_df_12 <- Total_df_11
    save(Total_df_12, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_12.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_12.Rdata")
    
    ## Subsetting ----
    # Subsetting last variables to final df
    Total_df_13 <- Total_df_12
    
    ## add nettoprisindeks ----
    Prisindeks <- readxl::read_excel("~/Downloads/Prisindeks.xlsx", col_types = c("date", "numeric"))
    Prisindeks$Dato <- as.Date(Prisindeks$Dato)
    Prisindeks$year <- format(Prisindeks$Dato, "%Y")
    Prisindeks$month <- format(Prisindeks$Dato, "%m")
    
    Total_df_13$year <- format(Total_df_12$dato.x, "%Y")
    Total_df_13$month <- format(Total_df_12$dato.x, "%m")
    
    joined_df <- Total_df_13 %>%
      left_join(Prisindeks, by = c("year", "month"))
    
    
    Total_df_13 <- joined_df %>%
      mutate(indeks = Prisindeks / 100,
             sales_price = indeks * nominal_price,
             Udbetaling = indeks * `Tidligere udbetalt byg/løs/afgrd`)
    

    Total_df_13 <- as.data.frame(Total_df_13)
    Total_df_13 <- Total_df_13 %>% tidyr::drop_na(sales_price)
    
    
    # ## add interest rate at time of purchase
    # obligationsrente <- read_excel("~/Downloads/obligationsrente_fida_uge13-2024.xlsx", 
    #                                                 sheet = "Sheet1", col_types = c("date", "numeric"))
    # obligationsrente$Dato <- as.Date(obligationsrente$Dato)
    # obligationsrente$year <- format(obligationsrente$Dato, "%Y")
    # obligationsrente$month <- format(obligationsrente$Dato, "%m")
    # 
    # monthly_mean <- aggregate(`Lang rente` ~ year + month, data = obligationsrente, FUN = mean)
    # 
    # Total_df_13 <- Total_df_13 %>%
    #   left_join(monthly_mean, by = c("year", "month"))
    
    # 
    varDelete <- c("vejnavn", "husnr", "floor", "kommunenavn", "side", "entryAddressID.x", "roofing", 
                   "adr_etrs89_oest", "adr_etrs89_nord", "nabolag_list",
                   "urban", "lag_price1", "forest_size", "habour_distance", "highway_distance", "market_name", 
                   "year", "month", "Dato.x", "Prisindeks", "indeks", "Dato.y", "Lang rente.x", 
                   "Bluespot_0cm","Bluespot_10cm","Bluespot_20cm", "lag_price")
    
    Total_df_13 <- Total_df_13[, !(names(Total_df_13) %in% varDelete)]
    
    Total_df_13 <- as.data.frame(Total_df_13)
    
    Total_df_13$Car_Garage <- NA
    Total_df_13 <- Total_df_13 %>%
      rowwise() %>%
      mutate(Car_Garage = ifelse(Car_Park == 1 | Garage == 1, 1, 0))
    Total_df_13 <- Total_df_13 %>%
      rowwise() %>%
      mutate(Car_Garage = ifelse(is.na(Car_Garage), 0, Car_Garage))
    
    save(Total_df_13, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_13.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_13.Rdata")
    
     
    
# Clean data of outliers  ---- 
    varDelete <- c("Garage", "Car_Park", "toilets", "urban_size")
    Total_df_14 <- Total_df_13[, !(names(Total_df_13) %in% varDelete)]
    
    #Rename problematic variables such as - " " 
    names(Total_df_14)[names(Total_df_14) == '<1940'] <- 'builtbefore1940'
    names(Total_df_14)[names(Total_df_14) == 'Renovated_1980-1990'] <- 'Renovated_1980_1990'
    names(Total_df_14)[names(Total_df_14) == 'Renovated_1940-1950'] <- 'Renovated_1940_1950'
    names(Total_df_14)[names(Total_df_14) == 'Renovated_1950-1960'] <- 'Renovated_1950_1960'
    names(Total_df_14)[names(Total_df_14) == 'Renovated_1960-1970'] <- 'Renovated_1960_1970'
    names(Total_df_14)[names(Total_df_14) == 'Renovated_1970-1980'] <- 'Renovated_1970_1980'
    names(Total_df_14)[names(Total_df_14) == 'Renovated_1980_1990'] <- 'Renovated_1980_1990'
    names(Total_df_14)[names(Total_df_14) == '1940-1950'] <- 'built_1940_1950'
    names(Total_df_14)[names(Total_df_14) == '1950-1960'] <- 'built_1950_1960'
    names(Total_df_14)[names(Total_df_14) == '1960-1970'] <- 'built_1960_1970'
    names(Total_df_14)[names(Total_df_14) == '1980-1990'] <- 'built_1980_1990'
    names(Total_df_14)[names(Total_df_14) == '1990-2000'] <- 'built_1990_2000'
    names(Total_df_14)[names(Total_df_14) == '2000-2010'] <- 'built_2000_2010'
    names(Total_df_14)[names(Total_df_14) == '1970-1980'] <- 'built_1970_1980'
    names(Total_df_14)[names(Total_df_14) == '2010'] <- 'builtafter_2010'
    
    ## Pricecleaning ---- 
    ## How many house prices are above 25 mil
    prices <- Total_df_14 %>%
      filter(sales_price > 25000000) %>%
      nrow()
    # 3655 houses exceed price, we plot density
    plot(density(Total_df_14$sales_price)) # Too skewed
    Density1 <- density(Total_df_14$sales_price)
    # we take subset and plot
    Price_25 <- subset(Total_df_14, sales_price < 25000000)
    Density2 <- density(Price_25$sales_price)
    plot(density(Price_25$sales_price)) # still too skewed
    # Not enough we remove 1st and 99 percentile. 
    # Check what thresholds are removed 
    p1 <- quantile(Total_df_14$sales_price, 0.01)
    p99 <- quantile(Total_df_14$sales_price, 0.99)
    # we find to be above 99 percentile, price is 9.3 mil
    # we find to be below 1 percentile, price is 72.500 dkk
    # subset again 
    Price_percentile <- subset(Total_df_14, sales_price > p1)
    Price_percentile <- subset(Price_percentile, sales_price < p99)
    Density3 <- density(Price_percentile$sales_price)
    plot(density(Price_percentile$sales_price)) # accept for now 
    Total_df_14 <- Price_percentile
    
    # Save plots for showcasing distribution of price
    # Create the plot
    x <- seq(1, 512)
    df <- data.frame(x = x, Density1 = Density1$y, Density2 = Density2$y, Density3 = Density3$y)
    Nominal_price_plot <- ggplot2::ggplot(df, ggplot2::aes(x)) +
      ggplot2::geom_line(aes(y = Density1, color = "Density 1"), size = 1) +
      ggplot2::geom_line(aes(y = Density2, color = "Density 2"), size = 1) +
      ggplot2::geom_line(aes(y = Density3, color = "Density 3"), size = 1) +
      labs(x = "Frequency", y = "Price", title = "Density for nominal price") +
      scale_color_manual(name = "Legend", values = c("green", "orange", "purple")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),  # Center the title
        legend.background = element_rect(fill = "white", color = "black")
            )
    plot(Nominal_price_plot)
    
    # Save plot 
    ggplot2::ggsave("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Plots/Nominal_price.png", plot = Nominal_price_plot, width = 10, height = 6, dpi = 400)
    
    Summary_statistics <- psych::describe(Total_df_14)
    
    ## Mark variables as factors ----
    Total_df_14 <- Total_df_14 %>%
      rowwise() %>%
      mutate(flooded = ifelse(flooded == 2 | flooded == 1, 1, 0))
    Total_df_14 <- Total_df_14 %>%
      rowwise() %>%
      mutate(coastline_distance = ifelse(coastline_distance == Inf, NA, coastline_distance))
    Total_df_14 <- Total_df_14 %>%
      rowwise() %>%
      mutate(Outbuilding = ifelse(is.na(Outbuilding), 0, Outbuilding))
    Total_df_14 <- Total_df_14 %>%
      rowwise() %>%
      mutate(heatpump_heating = ifelse(is.na(heatpump_heating), 0, heatpump_heating))
    Total_df_14 <- subset(Total_df_14, !is.na(coastline_distance))
    
    # recreate variable built_1970_1980, if not 1 in other built years, then 1 in variable built_1970_1980
    Total_df_14 <- Total_df_14 %>%
      rowwise() %>%
      mutate(built_1970_1980 = ifelse(builtbefore1940 == 0 & built_1940_1950 == 0 & built_1950_1960 == 0 &
                                      built_1960_1970 == 0 & built_1980_1990 == 0 & built_1990_2000 == 0 & 
                                      built_2000_2010 == 0 & builtafter_2010 == 0, 1, 0))
    
    # Delete missing observations of built and bricks etc
    Total_df_14 <- subset(Total_df_14, !is.na(builtbefore1940))
    Total_df_14 <- subset(Total_df_14, !is.na(Brick))
    # Total_df_14 <- subset(Total_df_14, select = - toilets)
    # Total_df_14 <- subset(Total_df_14, select = - urban_size)
    
    # Only missing on m2 and rooms, prepare imputation of mean
    mean_m2 <- mean(Total_df_14$m2, na.rm = TRUE)
    mean_rooms <- mean(Total_df_14$rooms, na.rm = TRUE)
    
    Total_df_14 <- Total_df_14 %>%
      rowwise() %>%
      mutate(m2 = ifelse(is.na(m2), mean_m2, m2))
    
    Total_df_14 <- Total_df_14 %>%
      rowwise() %>%
      mutate(rooms = ifelse(is.na(rooms), mean_rooms, rooms))
    
    
    #adjust sold variables 
    Total_df_14 <- Total_df_14 %>%
      rowwise() %>%
      mutate(Sold_0_0.5 = ifelse(Sold_0_0.5 == 5 | Sold_0_0.5 == 4 | Sold_0_0.5 == 3 | Sold_0_0.5 == 2, 1, Sold_0_0.5))
    
    Total_df_14 <- Total_df_14 %>%
      rowwise() %>%
      mutate(Sold_1 = ifelse(Sold_1 == 5 | Sold_1 == 4 | Sold_1 == 3 | Sold_1 == 2, 1, Sold_1))
    
    Total_df_14 <- Total_df_14 %>%
      rowwise() %>%
      mutate(Sold_2 = ifelse(Sold_2 == 5 | Sold_2 == 4 | Sold_2 == 3 | Sold_2 == 2, 1, Sold_2))
    
    Total_df_14 <- Total_df_14 %>%
      rowwise() %>%
      mutate(Sold_5 = ifelse(Sold_5 == 5 | Sold_5 == 4 | Sold_5 == 3 | Sold_5 == 2, 1, Sold_5))
    
    # save
    save(Total_df_14, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_14.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_14.Rdata")
    
    # Shift to 15
    Total_df_15 <- Total_df_14
    
    # For DF_15 skipped because data is reduced automatically later for unique neighbors
    
    # summary 
    df_Flooded <- subset(Total_df_15, flooded == 1) # Divide to remove some larger data set
    df_Notflooded <- subset(Total_df_15, flooded != 1)
    df_Notflooded <- subset(df_Notflooded, !is.na(postnr))
    # columns_to_exclude <- c("addressID", "enhed_id", "Coor", "Areas") 
    # columns_to_select <- setdiff(names(df_Notflooded), columns_to_exclude)
    # df_Notflooded <- df_Notflooded[complete.cases(df_Notflooded[, columns_to_select]), ]
    summary(df_Notflooded)
    summary(df_Flooded)
    
    Summary_statistics <- psych::describe(df_Notflooded)
    stats <- summary(df_Notflooded)
    
    
    ## Narrow down zip codes 
    results <- Total_df_15 %>%
      group_by(postnr) %>%
      summarize(count = sum(flooded == 1))
    # 545 zipcodes may be to many
    
    # Put zipcodes together by taking first two digits 
    Total_df_15$Areas <- substr(Total_df_14$postnr, 1, 2)
    
    results_flooded <- Total_df_15 %>%
      group_by(Areas) %>%
      summarize(flooded = sum(flooded == 1))
    results_notflooded <- Total_df_15 %>%
      group_by(Areas) %>%
      summarize(notflooded = sum(flooded == 0))
    
    
    Total_df_15$Areas <- as.factor(Total_df_15$Areas)
    
    Total_df_15$rowname <- rownames(Total_df_15)
    Total_df_15_2 <- subset(Total_df_15, select = c("rowname", "Coor"))
    
    
    
    # Save
    save(Total_df_15, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_15.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_15.Rdata")
    
    sf::st_write(Total_df_15_2, "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Total_df_15_v2.shp")
    
    Total_df_15_2 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Total_df_hoejde_v2.shp")
    
    #Merge with exisiting DF_15
    Total_df_15 <- merge(subset(Total_df_15_2, select = - geometry), Total_df_15, by = "rowname")

    
    Total_df_16 <- Total_df_15
    
    Total_df_16$Built <- NA
    Total_df_16 <- Total_df_16 %>% rowwise() %>%
      mutate(Built = ifelse(builtbefore1940 == 1, 1, Built)) %>%
      mutate(Built = ifelse(built_1940_1950 == 1, 2, Built)) %>%
      mutate(Built = ifelse(built_1950_1960 == 1, 3, Built)) %>%
      mutate(Built = ifelse(built_1960_1970 == 1, 4, Built)) %>%
      mutate(Built = ifelse(built_1970_1980 == 1, 5, Built)) %>%
      mutate(Built = ifelse(built_1980_1990 == 1, 6, Built)) %>%
      mutate(Built = ifelse(built_1990_2000 == 1, 7, Built)) %>%
      mutate(Built = ifelse(built_2000_2010 == 1, 8, Built)) %>%
      mutate(Built = ifelse(builtafter_2010 == 1, 9, Built)) 
    
    Total_df_16 <- subset(Total_df_16, select = - c(builtbefore1940, built_1940_1950, built_1950_1960,
                                                    built_1960_1970, built_1970_1980, built_1980_1990,
                                                    built_1990_2000, built_2000_2010, builtafter_2010))
    Total_df_16$Built <- as.factor(Total_df_16$Built)
    
    # check NA for columns 
    na_count <- colSums(is.na(Total_df_16))
    
    #same with renovation
    Total_df_16$Renovated <- 0
    Total_df_16 <- Total_df_16 %>% rowwise() %>%
      mutate(Renovated = ifelse(Renovated_1940_1950 == 1, 1, Renovated)) %>%
      mutate(Renovated = ifelse(Renovated_1950_1960 == 1, 2, Renovated)) %>%
      mutate(Renovated = ifelse(Renovated_1960_1970 == 1, 3, Renovated)) %>%
      mutate(Renovated = ifelse(Renovated_1970_1980 == 1, 4, Renovated)) %>%
      mutate(Renovated = ifelse(Renovated_1980_1990 == 1, 5, Renovated))
    Total_df_16$Renovated <- as.factor(Total_df_16$Renovated)
    
    Total_df_16 <- subset(Total_df_16, select = - c(Renovated_1940_1950, Renovated_1950_1960, Renovated_1960_1970,
                                                    Renovated_1970_1980, Renovated_1980_1990))
    
    
    #Heating
    Total_df_16$Heating <- 0
    Total_df_16 <- Total_df_16 %>% rowwise() %>%
      mutate(Heating = ifelse(district_heating == 1, 1, Heating)) %>%
      mutate(Heating = ifelse(central_heating == 1, 2, Heating)) %>%
      mutate(Heating = ifelse(electric_heating == 1, 3, Heating))
    
    Total_df_16 <- subset(Total_df_16, select = - c(district_heating, 
                                                    central_heating, electric_heating))
    
    Total_df_16$Heating <- as.factor(Total_df_16$Heating)
    
    #Roof
    Total_df_16$Roof <- 0
    Total_df_16 <- Total_df_16 %>% rowwise() %>%
      mutate(Roof = ifelse(tile_roof == 1, 1, Roof)) %>%
      mutate(Roof = ifelse(thatch_roof == 1, 2, Roof)) %>%
      mutate(Roof = ifelse(fibercement_asbestos_roof == 1, 3, Roof))
    
    Total_df_16 <- subset(Total_df_16, select = - c(tile_roof, 
                                                    thatch_roof, fibercement_asbestos_roof))
    
    Total_df_16$Roof <- as.factor(Total_df_16$Roof)
    
    #Builtmaterial
    Total_df_16$BMaterial <- NA
    Total_df_16 <- Total_df_16 %>% rowwise() %>%
      mutate(BMaterial = ifelse(Brick == 1, 1, BMaterial)) %>%
      mutate(BMaterial = ifelse(wood == 1, 2, BMaterial)) %>%
      mutate(BMaterial = ifelse(lightweight_concrete == 1, 3, BMaterial))
    
    Total_df_16 <- subset(Total_df_16, select = - c(Brick, 
                                                    wood, lightweight_concrete))
    
    Total_df_16 <- Total_df_16 %>% rowwise() %>%
      mutate(BMaterial = ifelse(is.na(BMaterial), 0, BMaterial))
    Total_df_16$BMaterial <- as.factor(Total_df_16$BMaterial)
    
    Total_df_16$BMaterial <- as.factor(Total_df_16$BMaterial)
  
    
    #define Total df 20 
    Total_df_17_v2 <- Total_df_16
    
    # Prerequisite for neighbors is to have distinct coordinates 
        # Change sfc multipoint to point
        Total_df_17_v2$Coor <- sf::st_cast(Total_df_17_v2$Coor, "POINT")
        
    Total_df_17_v2 <- Total_df_17_v2 %>%
      dplyr::distinct(Coor, .keep_all = TRUE) 
    
    save(Total_df_17_v2, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_17_v2.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_17_v2.Rdata")
    
    summary_17 <- as.data.frame(summary(Total_df_17_v2))
    # I note following variables are to be deleted 
    # heatpumpp_heating no variation
    Total_df_17_v2 <- subset(Total_df_17_v2, select = - heatpump_heating)
    # Correct height, rooms 
    Height_mean <- mean(Total_df_17_v2$Height)
    Room_mean <- mean(Total_df_17_v2$rooms)
    Total_df_17_v2 <- Total_df_17_v2 %>% rowwise() %>%
          mutate(Height = ifelse(Height <= 0, Height_mean, Height)) %>%
          mutate(rooms = ifelse(rooms <= 0 , Room_mean, rooms))
    
    summary(Total_df_17_v2)
    
    Total_df_18_v2 <- subset(Total_df_17_v2, 
                      select = -c(EV1, EV2, EV3, EV4, EV5, Sold_0_0.5, 
                                      Sold_1, Sold_2, Sold_5, dato.x))
    
    
    #Add interaction variable if sold after event and before next and so on. 
    Events <- as.data.frame(table(Total_df_18_v2$Hændelsesdato))
    Events$Var1 <- as.Date(Events$Var1)
    
    # We skip event 3, only 6 observations
    Total_df_18_v2 <- Total_df_18_v2 %>% rowwise() %>%
      mutate(SA_EV1 = ifelse(Dato < Events[2,1] & Dato > Events[1,1], 1, 0)) %>%
      mutate(SA_EV2 = ifelse(Dato < Events[4,1] & Dato > Events[2,1], 1, 0)) %>%
      mutate(SA_EV3 = ifelse(Dato < Events[5,1] & Dato > Events[4,1], 1, 0)) %>%
      mutate(SA_EV4 = ifelse(Dato < Events[6,1] & Dato > Events[5,1], 1, 0)) %>%
      mutate(SA_EV5 = ifelse(Dato > Events[6,1], 1, 0))
  # Husk at undlade sidste, altså før første event.    
    
  
  # Make lag price of neighbors 
    coords <- st_coordinates(Total_df_18_v2)
    Neighbor <- spdep::tri2nb(coords = coords)
    #Define nb object 
    neighbor_list <- spdep::nb2listw(Neighbor)
    # Add lagged variable to xg boosting
    Total_df_18_v2$Lag_price <- spdep::lag.listw(neighbor_list, Total_df_18_v2$nominal_price)
    lagged_price <- lag.listw(listw, price_vector)
    
    save(Total_df_18_v2, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_18_v2.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_18_v2.Rdata")
    
    
    # Data summery ----
    # Map of inundations in Denmark 
    Flooded <- subset(subset(Total_df_18_v2, select = c(Coor, flooded)), flooded == 1)
    Flooded$Coor <- sf::st_transform(Flooded$Coor, crs = "EPSG:4326") # Change crs to same as map of Denmark 
    Flooded$lon <- sf::st_coordinates(Flooded$Coor)[,1]
    Flooded$lat <- sf::st_coordinates(Flooded$Coor)[,2]
    Not_flooded <- subset(subset(Total_df_18_v2, select = c(Coor, flooded)), flooded != 1)
    Flooded$Coor <- sf::st_transform(Flooded$Coor, crs = "EPSG:4326") # Change crs to same as map of Denmark
    Not_flooded$lon <- sf::st_coordinates(Not_flooded$Coor)[,1]
    Not_flooded$lat <- sf::st_coordinates(Not_flooded$Coor)[,2]
    
    plot(Flooded)
    
    # Retrive map from dataforsyningen. 
    library(leaflet)
    API_GEO <- "https://api.dataforsyningen.dk/topo_skaermkort_DAF?service=WMS&request=GetCapabilities&token="
    token <- readline(prompt="Please enter token: ")
    url <- paste0(API_GEO,token)
    response <- httr::GET(url)
    httr::status_code(response)
  
    # Flooded
    m <- leaflet(data = Flooded) %>%
      setView(lng = 12.6, lat = 55.7, zoom = 6) %>% 
      addWMSTiles(
        baseUrl = url,
        layers = "dtk_skaermkort",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = 0.90)) %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        color = "red",  # All houses are flooded
        fillOpacity = 1,
        radius = 0.1)

    # Not flooded
    m <- leaflet(data = Flooded) %>%
      setView(lng = 12.6, lat = 55.7, zoom = 6) %>% 
      addWMSTiles(
        baseUrl = url,
        layers = "dtk_skaermkort",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = 0.90)) %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        color = "green",  # All houses are flooded
        fillOpacity = 1,
        radius = 0.1)
    
      
  

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # TO BE DELETED LATER IF NOT USEFUL       ----  

    
    # Test for linearity
    matrix <- Total_df_16
    matrix <- sf::st_drop_geometry(matrix)
    matrix <- subset(matrix, select = - c(Areas, enhed_id, addressID))
    results <- plm::detect.lindep(matrix)
    
    
    
    # load in with heights from file 
    Total_df_16 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Total_df_hoejde.shp")
    save(Total_df_16, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_16.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_16.Rdata")
    
    # Load in file ----
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_15.Rdata")
    
    # Export to QGIS ----
    Total_df_15 <- sf::st_as_sf(Total_df_15)
    sf::st_write(Total_df_15, "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Total_df_15.shp")
    
    # Load in with heights ----
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_16.Rdata")
    Total_df_16$Coor <- centroid <- sf::st_centroid(Total_df_16$geometry)
    rm(centroid)
    # # QGIS renamed columns, so we change them back 
    # colnames(Total_df_16)
    # Total_df_16 <- subset(Total_df_16, select = Height)
    # colnames(Total_df_15)[1:51] # The first 31 variables are aligned 
    # colnames(Total_df_16)[1:51] <- colnames(Total_df_15)[1:51]
    # Total_df_16$geometry <- sf::st_as_text(Total_df_16$geometry)
    # Total_df_16 <- sf::st_set_geometry(Total_df_16, Total_df_16$Coor)
    # sf::st_agr(Total_df_16) <- "Coor"
    # Total_df_16 <- subset(Total_df_16, select = -Coor)
    # Total_df_16$Areas <- as.factor(Total_df_16$Areas)
    
    # substitution, i retrrieved heights but keep df15 and map heights from df 16 and save as new df16
    test <- merge(Total_df_15, subset(Total_df_16, select = c("addrsID", "enhed_d", "Height")), by.x = c("addressID", "enhed_id"), by.y = c("addrsID", "enhed_d"))
    test <- test[!duplicated(test), ]
    
    # Test for linearity
    matrix <- Total_df_16_SAR
    matrix <- sf::st_drop_geometry(matrix)
    matrix <- subset(matrix, select = - c(Areas, enhed_id, addressID))
    results <- plm::detect.lindep(matrix)
    
    # Remove heatpump_heating, and sold variables 
    matrix <- subset(matrix, select = - c(heatpump_heating, Sold_0_0.5, Sold_1, Sold_2))
    results <- plm::detect.lindep(matrix)
    # No linear dependent columns detected, we delete the variable above from data set and save in new df
    Total_df_17 <- subset(Total_df_16, select = - c(heatpump_heating, Sold_0_0.5, Sold_1, Sold_2))
    
    save(Total_df_17, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_17.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_17.Rdata")
    
    # Change built variable to factor 
    Total_df_17$Built <- NA
    Total_df_17 <- Total_df_17 %>% rowwise() %>%
      mutate(Built = ifelse(builtbefore1940 == 1, 1, Built)) %>%
      mutate(Built = ifelse(built_1940_1950 == 1, 2, Built)) %>%
      mutate(Built = ifelse(built_1950_1960 == 1, 3, Built)) %>%
      mutate(Built = ifelse(built_1960_1970 == 1, 4, Built)) %>%
      mutate(Built = ifelse(built_1970_1980 == 1, 5, Built)) %>%
      mutate(Built = ifelse(built_1980_1990 == 1, 6, Built)) %>%
      mutate(Built = ifelse(built_1990_2000 == 1, 7, Built)) %>%
      mutate(Built = ifelse(built_2000_2010 == 1, 8, Built)) %>%
      mutate(Built = ifelse(builtafter_2010 == 1, 9, Built)) 
    
    Total_df_17 <- subset(Total_df_17, select = - c(builtbefore1940, built_1940_1950, built_1950_1960,
                                                    built_1960_1970, built_1970_1980, built_1980_1990,
                                                    built_1990_2000, built_2000_2010, builtafter_2010))
    Total_df_17$Built <- as.factor(Total_df_17$Built)
    
    
    #same with renovation
    Total_df_17$Renovated <- 0
    Total_df_17 <- Total_df_17 %>% rowwise() %>%
      mutate(Renovated = ifelse(Renovated_1940_1950 == 1, 1, Renovated)) %>%
      mutate(Renovated = ifelse(Renovated_1950_1960 == 1, 2, Renovated)) %>%
      mutate(Renovated = ifelse(Renovated_1960_1970 == 1, 3, Renovated)) %>%
      mutate(Renovated = ifelse(Renovated_1970_1980 == 1, 4, Renovated)) %>%
      mutate(Renovated = ifelse(Renovated_1980_1990 == 1, 5, Renovated))
    Total_df_17$Renovated <- as.factor(Total_df_17$Renovated)
    
    Total_df_17 <- subset(Total_df_17, select = - c(Renovated_1940_1950, Renovated_1950_1960, Renovated_1960_1970,
                                                    Renovated_1970_1980, Renovated_1980_1990))
    
    
    #Heating
    Total_df_17$Heating <- 0
    Total_df_17 <- Total_df_17 %>% rowwise() %>%
      mutate(Heating = ifelse(district_heating == 1, 1, Heating)) %>%
      mutate(Heating = ifelse(central_heating == 1, 2, Heating)) %>%
      mutate(Heating = ifelse(electric_heating == 1, 3, Heating))
    
    Total_df_17 <- subset(Total_df_17, select = - c(district_heating, 
                                            central_heating, electric_heating))
                          
    Total_df_17$Heating <- as.factor(Total_df_17$Heating)
    
    #Roof
    Total_df_17$Roof <- 0
    Total_df_17 <- Total_df_17 %>% rowwise() %>%
      mutate(Roof = ifelse(tile_roof == 1, 1, Roof)) %>%
      mutate(Roof = ifelse(thatch_roof == 1, 2, Roof)) %>%
      mutate(Roof = ifelse(fibercement_asbestos_roof == 1, 3, Roof))
    
    Total_df_17 <- subset(Total_df_17, select = - c(tile_roof, 
                                        thatch_roof, fibercement_asbestos_roof))
    
    Total_df_17$Roof <- as.factor(Total_df_17$Roof)
    
    #Builtmaterial
    Total_df_17$BMaterial <- NA
    Total_df_17 <- Total_df_17 %>% rowwise() %>%
      mutate(BMaterial = ifelse(Brick == 1, 1, BMaterial)) %>%
      mutate(BMaterial = ifelse(wood == 1, 2, BMaterial)) %>%
      mutate(BMaterial = ifelse(lightweight_concrete == 1, 3, BMaterial))
    
    Total_df_17 <- subset(Total_df_17, select = - c(Brick, 
                                            wood, lightweight_concrete))
    
    Total_df_17$BMaterial <- as.factor(Total_df_17$BMaterial)
    
    #Event
    Total_df_17$Event <- 0
    Total_df_17 <- Total_df_17 %>% rowwise() %>%
      mutate(Event = ifelse(EV1 == 1, 1, Event)) %>%
      mutate(Event = ifelse(EV2 == 1, 2, Event)) %>%
      mutate(Event = ifelse(EV3 == 1, 3, Event)) %>%
      mutate(Event = ifelse(EV4 == 1, 4, Event)) %>%
      mutate(Event = ifelse(EV5 == 1, 5, Event))
    
    Total_df_17 <- subset(Total_df_17, select = - c(EV1, EV2, EV3, EV4,EV5))
    
    Total_df_17$Event <- as.factor(Total_df_17$Event)
    
    
    Total_df_18 <- Total_df_17
    
    
    save(Total_df_18, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_18.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_18.Rdata")
    
    results <- Total_df_18 %>%
      group_by(postnr) %>%
      summarize(count = sum(flooded == 1))
    
    r50 <- subset(results, count > 50) # 172 zip codes
    r100 <- subset(results, count > 100) # 82
    r250 <- subset(results, count > 250) # 10
    
    zipcodes <- as.vector(r50$postnr)
    
    # Now 886.312 observations, that is half of before. Hopefully easier to do computations
    Total_df_18 <- Total_df_18[Total_df_18$postnr %in% zipcodes, ]
    
    Total_df_19 <- Total_df_18
    
    save(Total_df_19, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_19.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_19.Rdata")
    
    
    
    
    plot(Total_df_15$nominal_price)

    
    
    # hist(Total_df_13$nominal_price, main = "Histogram of Count Variable")
    
    
        
# Descriptives ----
    library(gt)
    library(gtExtras)
    
    colnames <- c("nominal_price", "m2", "district_heating", "central_heating", "electric_heating",
                  "tile_roof", "thatch_roof", "fibercement_asbestos_roof", "roofing", "Outbuilding",
                  "Car_Park", "Garage", "TerracedHouse", "<1940", "1940-1950", "1950-1960", "1960-1970",
                  "1970-1980", "1980-1990", "1980-1990", "1990-2000", "2000-2010", "2010", "Brick", 
                  "lightweight_concrete","wood", "Renovated_1940-1950", "Renovated_1950-1960", 
                  "Renovated_1960-1970", "Renovated_1970-1980", "Renovated_1980-1990", "rooms", "toilets", 
                  "Tidligere udbetalt byg/løs/afgrd", "flooded", "urban_size", "heatpump_heating", "forest_distance", 
                  "coastline_distance", "powerline_distance", "railway_distance", "lake_distance", "Trainstation_distance",
                  "Wateryarea_distance", "lag_price", "Bluespot_0cm", "Bluespot_10cm", "Bluespot_20cm")
    
    subsetTotal_df_13 <- subset(Total_df_13, select = colnames)
    subsetTotal_df_13 <- as.data.frame(subsetTotal_df_13)
    subsetTotal_df_13 <- subset(subsetTotal_df_13, select = -Coor)
    subsetTotal_df_13$lag_price <- as.numeric(subsetTotal_df_13$lag_price)
    
    df_descriptives <- as.data.frame(matrix(NA, nrow = 0, ncol = 7))
    colnames(df_descriptives) <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "Plot")
    
    for (i in colnames(subsetTotal_df_13)){
      start_time <- Sys.time()
      df_descriptives[i,1] <- quantile(subsetTotal_df_13[,i], probs = 0, na.rm = TRUE)
      df_descriptives[i,2] <- quantile(subsetTotal_df_13[,i], probs = 0.25, na.rm = TRUE)
      df_descriptives[i,3] <- quantile(subsetTotal_df_13[,i], probs = 0.5, na.rm = TRUE)
      df_descriptives[i,4] <- mean(subsetTotal_df_13[,i])
      df_descriptives[i,5] <- quantile(subsetTotal_df_13[,i], probs = 0.75, na.rm = TRUE)
      df_descriptives[i,6] <- quantile(subsetTotal_df_13[,i], probs = 1, na.rm = TRUE)
      
    
      cat("Time", i, ": ", Sys.time() - start_time, "\n")
    }
    
    gt_tbl <- gt(df_descriptives)
    
    gt_tbl <- gt_tbl %>%
      tab_header(
        title = "My Custom Table",
        subtitle = "Data from my_df"
      ) 
    
    print(gt_tbl)
    
# Plot ----    
    plot_name <- subset(Total_df_8, select = c(Coor, flooded))
    #plot_name <- subset(plot_name, flooded == 1)
    plot_name <- sf::st_as_sf(plot_name)
    
    plot(plot_name)  
    
    
    
    
    
    
    
