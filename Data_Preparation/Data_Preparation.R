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
        
        ### Skader variable to be able to distinguish between flood and precipitation
        Trade_Skader$flooded <- ifelse(!is.na(Trade_Skader$Hændelsesdato) | !is.na(Trade_Skader$`Tidligere udbetalt byg/løs/afgrd`), 2, 0)
    
    ## Merge Precipitation on merged data  ----
    # Precipitation only unique identifier is 'Enhed' that maps onto BBR_Enhed_Subset enhed_id
        load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Precipitation_subset.Rdata")
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
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df.Rdata")
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

    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df2.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df2.Rdata")
    
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
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df3.Rdata")
    

# Change geographical point variable to spatial variable ----
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df3.Rdata")

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
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df4.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df4.Rdata")


# Load in geographical variables ----
    
    ## Højspænding ----
    powerline_distance <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/hoejspaendingsledning/hoejspaendingsledning.shp")
    
    ## Jernbane ----
    Jernbane1 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/jernbane/jernbane_0001/jernbane.shp")
    Jernbane <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/jernbane/jernbane_0000/jernbane.shp")
    railway_distance <- rbind(Jernbane, Jernbane1)
    rm(Jernbane1)
    
    ## Kyst ----
    Kyst0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/kyst/kyst_0000/kyst.shp")
    Kyst1 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/kyst/kyst_0001/kyst.shp")
    coastline_distance <- rbind(Kyst0, Kyst1)
    rm(Kyst0, Kyst1)
    
    ## skov ----
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
    trainstation_distance <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/togstation/togstation.shp")
    trainstation_distance <- sf::st_set_crs(trainstation_distance, sf::st_crs(Total_df))
    
    ## Vaadområde ----
    Vaadområde0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vaadomraade/vaadomraade_0000/vaadomraade.shp")
    Vaadområde1 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vaadomraade/vaadomraade_0001/vaadomraade.shp")
    Vaadområde2 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vaadomraade/vaadomraade_0002/vaadomraade.shp")
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
    # vejkant0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vejkant/vejkant_0000/vejkant.shp")
    
    
    ## Vindmølle ----
    windturbine_distance <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vindmoelle/vindmoelle.shp")
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
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df5.Rdata")        
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df5.Rdata")
    
    
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
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_1_5.Rdata")                
  
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
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_6_10.Rdata")                
    
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
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_11_15.Rdata")                
    
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
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_16_20.Rdata")                
    
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
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_1_5.Rdata")                
    
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
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_6_10.Rdata")                
    
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
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_11_15.Rdata")                
    
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
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_16_20.Rdata")                
    
    
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
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_trainstation.Rdata")                
    
    
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
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_wateryarea.Rdata")                
    
    
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
    
    save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_windturbine.Rdata")                
    
  # Combine after splitting (Split due to large computational time) ----  
    ## Forest ----
    ### Load df from after powerline, railway_distance, coastline. 
    T1 <- Sys.time()
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df5.Rdata")
    Total_df_5 <- Total_df
    
    # Load first forest
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_1_5.Rdata")                
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
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_6_10.Rdata")                
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
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_11_15.Rdata")                
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
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_forest_16_20.Rdata")                
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
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_1_5.Rdata")                
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
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_6_10.Rdata")                
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
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_11_15.Rdata")                
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
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_lake_16_20.Rdata")                
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
    
    save(Total_df_6, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df6.Rdata")        
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df6.Rdata")
    
    # Get rid of all new unnecessary variables
    names <- colnames(Total_df_6[, 1:71])
    Total_df_6 <- subset(Total_df_6, select = names)
  
    ## Trainstation ----    
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_trainstation.Rdata")                
    Starttime <- Sys.time()
    Total_df_6 <- Total_df_6 %>% 
      dplyr::bind_cols(select(Total_df, trainstation_distance), by = c("Coor" = "Coor"))
    endtime <- Sys.time()-Starttime
    print(endtime)
    
    ## Wateryarea ----    
    Starttime <- Sys.time()
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_wateryarea.Rdata")             
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
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_windturbine.Rdata")             
    Total_df_6 <- Total_df_6 %>% 
      dplyr::bind_cols(select(Total_df, windturbine_distance, windturbine_height), by = c("Coor" = "Coor"))
    endtime <- Sys.time()-Starttime
    print(endtime)
    
    Total_df_7 <- Total_df_6
    save(Total_df_7, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_7.Rdata")
    colnames(Total_df_7)[71] <- "Coor"
    colnames(Total_df_7)[72] <- "Trainstation_distance"
    
    
    var_remove <- c("trainstation_distance...59", "windturbine_height...62", "windturbine_distance...61", "Coor...73",
                    "by...74", "Wateryarea_distance_1", "Wateryarea_distance_2", "Wateryarea_distance_3", 
                    "Wateryarea_distance_4", "windturbine_height...83", "Coor...84", "by...85", "by...80", "by...74", 
                    "windturbine_distance...82", "Coor...79")
    Total_df_8 <- Total_df_7[, !(names(Total_df_7) %in% var_remove)]
    save(Total_df_8, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_8.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_8.Rdata")

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
  
    save(Total_df_9, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_9.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_9.Rdata")
    
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
    
    save(Total_df_10, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_10.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_10.Rdata")
  
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
    save(Total_df_11, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_11.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_11.Rdata")
    
    
    ## Difference in Difference variable ----
    #Tre variable for hver oversvømmelse, men hvis event er sket efter salgs skal den være NA
    Events <- as.data.frame(table(Total_df_11$Hændelsesdato))
    Events$Var1 <- as.Date(Events$Var1)
    
    Total_df_11 <- Total_df_11 %>%
      mutate(EV1 = ifelse(dato.x < Events[1,1], 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(EV2 = ifelse(dato.x > Events[1,1] & dato.x < Events[2,1], 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(EV3 = ifelse(dato.x > Events[2,1] & dato.x < Events[3,1], 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(EV4 = ifelse(dato.x > Events[3,1] & dato.x < Events[4,1], 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(EV5 = ifelse(dato.x > Events[4,1] & dato.x < Events[5,1], 0, 1))
    
    ## Sold after event variable 
    ### EV1 ----
    Total_df_11 <- Total_df_11 %>%
             mutate(Sold_Ev1_0_0.5 = ifelse(dato.x > Events[1,1] & dato.x < Events[1,1] + 365/2, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev1_1 = ifelse(dato.x > Events[1,1] & dato.x < Events[1,1] + 365, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev1_2 = ifelse(dato.x > Events[1,1] & dato.x < Events[1,1] + 365*2, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev1_5 = ifelse(dato.x > Events[1,1] & dato.x < Events[1,1] + 365*5, 0, 1))
    
    ### EV2 ----
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev2_0_0.5 = ifelse(dato.x > Events[2,1] & dato.x < Events[2,1] + 365/2, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev2_1 = ifelse(dato.x > Events[2,1] & dato.x < Events[2,1] + 365, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev2_2 = ifelse(dato.x > Events[2,1] & dato.x < Events[2,1] + 365*2, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev2_5 = ifelse(dato.x > Events[2,1] & dato.x < Events[2,1] + 365*5, 0, 1))
    
    ### EV3 ----
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev3_0_0.5 = ifelse(dato.x > Events[3,1] & dato.x < Events[3,1] + 365/2, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev3_1 = ifelse(dato.x > Events[3,1] & dato.x < Events[3,1] + 365, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev3_2 = ifelse(dato.x > Events[3,1] & dato.x < Events[3,1] + 365*2, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev3_5 = ifelse(dato.x > Events[3,1] & dato.x < Events[3,1] + 365*5, 0, 1))
    
    ### EV4 ----
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev4_0_0.5 = ifelse(dato.x > Events[4,1] & dato.x < Events[4,1] + 365/2, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev4_1 = ifelse(dato.x > Events[4,1] & dato.x < Events[4,1] + 365, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev4_2 = ifelse(dato.x > Events[4,1] & dato.x < Events[4,1] + 365*2, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev4_5 = ifelse(dato.x > Events[4,1] & dato.x < Events[4,1] + 365*5, 0, 1))
    
    ### EV5 ----
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev5_0_0.5 = ifelse(dato.x > Events[5,1] & dato.x < Events[5,1] + 365/2, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev5_1 = ifelse(dato.x > Events[5,1] & dato.x < Events[5,1] + 365, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev5_2 = ifelse(dato.x > Events[5,1] & dato.x < Events[5,1] + 365*2, 0, 1))
    
    Total_df_11 <- Total_df_11 %>%
      mutate(Sold_Ev5_5 = ifelse(dato.x > Events[5,1] & dato.x < Events[5,1] + 365*5, 0, 1))
    
    summary(Total_df_11$Sold_Ev1_0_0.5)
    
# Subsetting ---- 
    varDelete <- c("EPSG", "Geometri_EPSG_25832", "y_coord", "x_coord", "latitude", "longitude", 
                   "neighbor_list", "rownumber", "neighbor", "nabolag")
    Total_df_11 <- Total_df_11[, !(names(Total_df_11) %in% varDelete)]
    
    Total_df_12 <- Total_df_11
    save(Total_df_12, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_12.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_12.Rdata")
    
    ## Subsetting ----
    # Subsetting last variables to final df
    Total_df_13 <- Total_df_12
    
    ## add nettoprisindeks ----
    Prisindeks <- read_excel("~/Downloads/Prisindeks.xlsx", col_types = c("date", "numeric"))
    Prisindeks$Dato <- as.Date(Prisindeks$Dato)
    Prisindeks$year <- format(Prisindeks$Dato, "%Y")
    Prisindeks$month <- format(Prisindeks$Dato, "%m")
    
    Total_df_12$year <- format(Total_df_12$dato.x, "%Y")
    Total_df_12$month <- format(Total_df_12$dato.x, "%m")
    
    joined_df <- Total_df_12 %>%
      left_join(Prisindeks, by = c("year", "month"))
    
    Total_df_13 <- joined_df %>%
      mutate(indeks = Prisindeks / 100,
             sales_price = indeks * nominal_price)
    
    Total_df_13 <- as.data.frame(Total_df_13)
    Total_df_13 <- Total_df_13 %>% tidyr::drop_na(sales_price)
    
    
    ## add interest rate at time of purchase
    obligationsrente <- read_excel("~/Downloads/obligationsrente_fida_uge13-2024.xlsx", 
                                                    sheet = "Sheet1", col_types = c("date", "numeric"))
    obligationsrente$Dato <- as.Date(obligationsrente$Dato)
    obligationsrente$year <- format(obligationsrente$Dato, "%Y")
    obligationsrente$month <- format(obligationsrente$Dato, "%m")
    
    monthly_mean <- aggregate(`Lang rente` ~ year + month, data = obligationsrente, FUN = mean)
  
    Total_df_13 <- Total_df_13 %>%
      left_join(monthly_mean, by = c("year", "month"))
    
    
    varDelete <- c("vejnavn", "husnr", "floor", "kommunenavn", "side", "entryAddressID.x", 
                   "dato.x", "adr_etrs89_oest", "adr_etrs89_nord", "Hændelsesdato", "Coor", "nabolag_list",
                   "urban", "lag_price1", "forest_size", "habour_distance", "highway_distance", "market_name", 
                   "year", "month", "Dato.x", "Prisindeks", "indeks", "sales_price", "Dato.y", "Lang rente.x")
    
    Total_df_13 <- Total_df_12[, !(names(Total_df_12) %in% varDelete)]
    
    Total_df_13$lag_price <- as.numeric(Total_df_13$lag_price)
    Total_df_13 <- as.data.frame(Total_df_13)
    
    save(Total_df_13, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_13.Rdata")
    load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_13.Rdata")
    
    
    
    
    # hist(Total_df_13$nominal_price, main = "Histogram of Count Variable")
    
# Clean data of outliers  ---- 
    # Univariate outlier ------------------------------------------------------
    plot(Total_df_13$nominal_price)
    
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
    
    
    
    
    
    
    