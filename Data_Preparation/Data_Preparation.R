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
    forest_distance$area <- st_area(forest_distance)
    forest_distance$area <- as.numeric(forest_distance$area)
    # Subset based on area > 5000 m2 according to forest definition of FN
    forest_distance <- subset(forest_distance, forest_distance$area > 5000)
    summary(forest_distance$area)
    # Min <- units::set_units(5000, m^2)
    seq <- seq(0.00, 0.95, by = 0.05)
    
    #save in list
    list.dfs <- list()
    
    for (i in seq) {
      # Create variable name
      df_name <- paste0("forest_distance_", (i+0.05))
      
      # Subset data 
      df <- subset(forest_distance, 
                   area > quantile(forest_distance$area, probs = i) & 
                     area < quantile(forest_distance$area, probs = (i + 0.05)))
      
      # Set CRS for the subset
      df <- sf::st_set_crs(df, sf::st_crs(Total_df))
      
      # Save data frame in list
      list.dfs[[df_name]] <- df
    }
    rm(forest_distance, df)

    
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
    lake_distance <- rbind(Soe0, Soe1, Soe2, Soe3, Soe4, Soe5, Soe6, Soe7, Soe8, Soe9, 
                 Soe10, Soe11, Soe12, Soe13, Soe14, Soe15, Soe16, Soe17, Soe18)
    rm(Soe0, Soe1, Soe2, Soe3, Soe4, Soe5, Soe6, Soe7, Soe8, Soe9, 
       Soe10, Soe11, Soe12, Soe13, Soe14, Soe15, Soe16, Soe17, Soe18)
    
    # Togstation
    trainstation_distance <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/togstation/togstation.shp")
    
    # Vaadområde 
    Vaadområde0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vaadomraade/vaadomraade_0000/vaadomraade.shp")
    Vaadområde1 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vaadomraade/vaadomraade_0001/vaadomraade.shp")
    Vaadområde2 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vaadomraade/vaadomraade_0002/vaadomraade.shp")
    Vandområde <- rbind(Vaadområde0, Vaadområde1,Vaadområde2)
    rm(Vaadområde0, Vaadområde1,Vaadområde2)
    
    # Mangler vejkant pga. 160
    # vejkant0 <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vejkant/vejkant_0000/vejkant.shp")
    
    
    # Vindmølle
    windturbine_distance <- sf::read_sf("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Lokationer/Unzipped/vindmoelle/vindmoelle.shp")
    
    
    
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
    
    
    ## forest_distance ----
    kommune_nr <- sort(unique(Total_df$postnr))
    
    
    for (d in 1:length(list.dfs)) {
      # Get the data frame at position 'd' in the list
      df <- list.dfs[[d]]
      
      for (i in kommune_nr) {
        cat(i, "\n")
        start_time <- Sys.time()
        
        # Subset the data
        subset_df <- Total_df[Total_df$postnr == i, ]
        
        if(nrow(subset_df) > 0) {
          # Calculate distances
          distances <- sf::st_distance(subset_df, df)
          
          # Define the 'miin' function, or replace it with an appropriate function
          miin <- function(x) min(x, na.rm = TRUE)
          
          # Calculate minimum distances
          min_distances <- apply(distances, 1, miin)
          
          # Replace values in Total_df$forest_distance if min_distances is less
          Total_df$forest_distance[Total_df$postnr == i] <- ifelse(min_distances < Total_df$forest_distance[Total_df$postnr == i], min_distances, Total_df$forest_distance[Total_df$postnr == i])
        }
        
        cat("Time for municipality Forest", i, ": ", Sys.time() - start_time, "\n")
      }
    }
    
    

    registerDoParallel(cores = 6)
    
    # Use foreach to loop over list.dfs in parallel
    grand_list <- foreach(df = list.dfs, .packages = "sf") %dopar% {
      
      # Initialize an empty list to store the results of the inner loop
      inner_results <- list()
      
      # zip_code
      postnr <- sort(unique(Total_df$postnr))
      
      
      # Use a regular for loop to iterate over postnr
      for(i in postnr) {
        cat(i, "\n")
        start_time <- Sys.time()
        
        # Subset the data
        subset_df <- Total_df[Total_df$postnr == i, ]
        
        if(nrow(subset_df) > 0) {
          # Calculate distances
          distances <- sf::st_distance(subset_df, df)
          
          # Define the 'miin' function, or replace it with an appropriate function
          miin <- function(x) min(x, na.rm = TRUE)
          
          # Calculate minimum distances
          min_distances <- apply(distances, 1, miin)
          
          # Store minimum distances in a new column
          subset_df$min_distances <- min_distances
        }
        
        end_time <- Sys.time()
        print(paste("Time for municipality Forest", i, ": ", end_time - start_time))
        
        # Store the updated subset_df in the inner_results list
        inner_results[[i]] <- subset_df
      }
      
      # Combine the results of the inner loop using do.call
      do.call(rbind, inner_results)
      
    }
    
    
    save(Total_df_updated, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_updated.Rdata")            
    

    
save(Total_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df5.Rdata")        
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df5.Rdata")
# Clean data of outliers  ---- 
    
    
    
    
    
    
    
    
    