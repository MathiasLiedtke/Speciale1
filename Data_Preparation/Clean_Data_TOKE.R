#### Library
library(dplyr)

####
# Generate 'cleaned dataset' ----
# Start with BBR and load in data and remove unneccesary variables and transform variables to appropriate. 
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Bygning.rdata")
  ## Select variables of interest ----
      Var <- c("bygning_id","byg039BygningensSamledeBoligAreal", "byg021BygningensAnvendelse", "byg044ArealIndbyggetUdhus", "byg026Opførelsesår", 
         "byg032YdervæggensMateriale", "byg033Tagdækningsmateriale", "byg056Varmeinstallation", "byg027OmTilbygningsår",
         "grund", "husnummer")

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
          
          ### Car garage double ----
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
          BBR_Enhed_Trade <- merge(BBR_Enhed, trades_1992_2021, by.x = "adresseIdentificerer", by.y = "addressID")
          #Truncate data to sales from 2010
          BBR_Enhed_Trade <- subset(BBR_Enhed_Trade, BBR_Enhed_Trade$dato > "2010-01-01")
           # save(BBR_Enhed_Trade, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Enhed_Trade.Rdata")
        
              ##### Keep useful variables ----
              Var_to_keep_Enhed_Trade <- c("adresseIdentificerer", "enhed_id", "kommunekode", "enh020EnhedensAnvendelse",  
                                           "enh023Boligtype", "enh031AntalVærelser", "enh032Toiletforhold", "etage", "opgang",
                                           "bygning", "vejnavn", "husnr", "postnr", "m2", "dato", "entryAddressID", "nominal_price", 
                                           "year")
              
              BBR_Enhed_Trade <- subset(BBR_Enhed_Trade, select = Var_to_keep_Enhed_Trade)
              
      ### Merge with building ----   
          CLEAN_DATA <- merge(BBR_Bygning_Subset,BBR_Enhed_Trade, by.x = "bygning_id", by.y = "bygning" )    
            # save(CLEAN_DATA, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/CLEAN_DATA.Rdata")
            
          # Assumption: Variables as when it has been renovated, NA changed to '0'
              CLEAN_DATA <- CLEAN_DATA %>% mutate(`Renovated_1940-1950` = ifelse(is.na(`Renovated_1940-1950`), 0, `Renovated_1940-1950`),
                                                 `Renovated_1950-1960` = ifelse(is.na(`Renovated_1950-1960`), 0, `Renovated_1950-1960`),
                                                 `Renovated_1960-1970` = ifelse(is.na(`Renovated_1960-1970`), 0, `Renovated_1960-1970`),
                                                 `Renovated_1970-1980` = ifelse(is.na(`Renovated_1970-1980`), 0, `Renovated_1970-1980`),
                                                 `Renovated_1980-1990` = ifelse(is.na(`Renovated_1980-1990`), 0, `Renovated_1980-1990`))
              #save(CLEAN_DATA, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/CLEAN_DATA.Rdata")  
              
              Var_to_keep3 <- c("")
              
              
              
    ## Hente GEO data ----
              API_GEO <- "https://api.dataforsyningen.dk/GeoDanmark_60_NOHIST_DAF?service=WMS&request=GetCapabilities&token="
              token <- readline(prompt="Please enter token: ")
              url <- paste0(API_GEO,token)
              response <- httr::GET(url)
              httr::status_code(response)
  
    
        ### To handle spatial data, load library ----
              library("sf")
              library("leaflet")
              library("sp")
              
              # Requires "brew install gdal"

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
              
              

 {
          Handel_BFE_Adresse <- merge(EJF_Handelsoplysning, DAWA_Adresse, by.x = "objectid", by.y = "Jordstykke_Bfenummer")
           #### Remove unneccesary variables ----
                Var_to_keep <- c("objectid", "id_lokalId", "registreringFra", "registreringTil", "afstaaelsesdato", "koebsaftaleDato", 
                                 "samletKoebesum", "valutakode", "bygningerOmfattet", "Adresseid", "Kommunekode", "Kommunenavn", "Vejkode", 
                                 "Vejnavn", "Husnr", "Etage", "Doer", "Postnr", "Postdistrikt", "Jordstykke_SfeEjendomsnr","Jordstykke_EsrEjendomsnr",
                                 "Jordstykke_Matrikelnr", "ETRS89_Oest", "ETRS89_Nord", "Geometri_EPSG_25832")
                Handel_BFE_Adresse <- subset(Handel_BFE_Adresse, select = Var_to_keep)
          } #To delete later if not useful 

          
  load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/DAWA_Adresse.Rdata")        
  load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Enhed.Rdata")        
  load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/EJF_Handelsoplysning.Rdata")
          
                    
# Cleaning data ----
# This script is made for cleaning the data received from Toke. I imagine to delete columns, outliers, etc. 
# How to clean data # https://www.r-bloggers.com/2021/04/how-to-clean-the-datasets-in-r/


  