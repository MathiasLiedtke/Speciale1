# Data from Toke ----

## Packages -----
library(openxlsx)
library(dplyr)

## Loading in the data from repository ----
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/fp_events.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev6_Aalborg.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev7_aarhus.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev8_Trekanstomraade.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev1_Koebenhavn.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev1_StorKoebenhavn.Rdata")

## Variables in excel ----
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

## Add dataframes in one
CitiesCombines <- dplyr::bind_rows(ev1_Koebenhavn, ev1_StorKoebenhavn, ev6_Aalborg, 
                                   ev7_aarhus, ev8_Trekanstomraade)

## Data descriptives ----
# How many unique buildings?
iNO_UNIQUE_Entities <- length(unique(CitiesCombines$Enhed_id))
print(iNO_UNIQUE_Entities)
# Answer = 155.560

# How many entities have been flooded - Variable of interest 
iFlooded_Homes <- CitiesCombines %>%
  filter(total_payout != 0) %>%
  summarise(unique_count = n_distinct(Enhed_id))
print(iFlooded_Homes)
# Answer = 7331

# Addition of more data from Toke----
#   This is in csv format instead, following changes to Rdata
wb <- createWorkbook()
file_path <- "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Overblik_data.xlsx"
  
  #Bygning
    # BBR_Bygning <- read.csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Bygning.csv")
    # save(BBR_Bygning, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Bygning.Rdata" )
    # rm(BBR_Bygning)
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Bygning.Rdata")
    # BBR_Bygning_Head <- head(BBR_Bygning)
    # addWorksheet(wb, sheetName = "BBR_Bygning_Head")
    # writeData(wb, sheet = "BBR_Bygning_Head", x = head(BBR_Bygning))
    # # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    # addWorksheet(wb, sheetName = "BBR_Bygning_Summary")
    # writeData(wb, sheet = "BBR_Bygning_Summary", x = summary(BBR_Bygning))
rm(BBR_Bygning)
                        ###### Statistics on this
                        # How many unique obs? 
i_Unique_Building <- length(unique(BBR_Bygning$bygning_id))
                            # Answer = 4.869.043

  #Enhed
    # BBR_Enhed <- read.csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Enhed.csv")
    # save(BBR_Enhed, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Enhed.Rdata" )
    # rm(BBR_Enhed)
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Enhed.Rdata")
    addWorksheet(wb, sheetName = "BBR_Enhed_Head")
    writeData(wb, sheet = "BBR_Enhed_Head", x = head(BBR_Enhed))
    # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    addWorksheet(wb, sheetName = "BBR_Enhed_Summary")
    writeData(wb, sheet = "BBR_Enhed_Summary", x = summary(BBR_Enhed))
    rm(BBR_Enhed)

  #Grund
    # BBR_Grund <- read.csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Grund.csv")
    # save(BBR_Grund, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Grund.Rdata" )
    # rm(BBR_Grund)
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Grund.Rdata")
    addWorksheet(wb, sheetName = "BBR_Grund_Head")
    writeData(wb, sheet = "BBR_Grund_Head", x = head(BBR_Grund))
    # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    addWorksheet(wb, sheetName = "BBR_Grund_Summary")
    writeData(wb, sheet = "BBR_Grund_Summary", x = summary(BBR_Grund))
    rm(BBR_Grund)


  #Opgang
    # BBR_Opgang <- read.csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Opgang.csv")
    # save(BBR_Opgang, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Opgang.Rdata" )
    # rm(BBR_Opgang)
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Opgang.Rdata")
    addWorksheet(wb, sheetName = "BBR_Opgang_Head")
    writeData(wb, sheet = "BBR_Opgang_Head", x = head(BBR_Opgang))
    # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    addWorksheet(wb, sheetName = "BBR_Opgang_Summary")
    writeData(wb, sheet = "BBR_Opgang_Summary", x = summary(BBR_Opgang))
    rm(BBR_Opgang)
    
    
  #Etage
    # BBR_Etage <- read.csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Etage.csv")
    # save(BBR_Etage, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Etage.Rdata" )
    # rm(BBR_Etage)
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/BBR_Etage.Rdata")
    addWorksheet(wb, sheetName = "BBR_Etage_Head")
    writeData(wb, sheet = "BBR_Etage_Head", x = head(BBR_Etage))
    # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    addWorksheet(wb, sheetName = "BBR_Etage_Summary")
    writeData(wb, sheet = "BBR_Etage_Summary", x = summary(BBR_Etage))
    rm(BBR_Etage)

  #MAT_Jordstykke
    # MAT_Jordstykke <- read.csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/MAT_Jordstykke.csv")
    # save(MAT_Jordstykke, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/MAT_Jordstykke.Rdata" )
    # rm(MAT_Jordstykke)
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/MAT_Jordstykke.Rdata")
    addWorksheet(wb, sheetName = "MAT_Jordstykke_Head")
    writeData(wb, sheet = "MAT_Jordstykke_Head", x = head(MAT_Jordstykke))
    # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    addWorksheet(wb, sheetName = "MAT_Jordstykke_Summary")
    writeData(wb, sheet = "MAT_Jordstykke_Summary", x = summary(MAT_Jordstykke))
    rm(MAT_Jordstykke)

  #DAWA_Adgangsadresse
    # DAWA_Adgangsadresse <- read.csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/DAWA_Adgangsadresse.csv")
    # save(DAWA_Adgangsadresse, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/DAWA_Adgangsadresse.Rdata" )
    # rm(DAWA_Adgangsadresse)
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/DAWA_Adgangsadresse.Rdata")
    addWorksheet(wb, sheetName = "DAWA_Adgangsadresse_Head")
    writeData(wb, sheet = "DAWA_Adgangsadresse_Head", x = head(DAWA_Adgangsadresse))
    # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    addWorksheet(wb, sheetName = "DAWA_Adgangsadresse_Summary")
    writeData(wb, sheet = "DAWA_Adgangsadresse_Summary", x = summary(DAWA_Adgangsadresse))
    rm(DAWA_Adgangsadresse)
    
  #DAWA_Adresse
    # DAWA_Adresse <- read.csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/DAWA_Adresse.csv")
    # save(DAWA_Adresse, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/DAWA_Adresse.Rdata" )
    # rm(DAWA_Adresse)
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/DAWA_Adresse.Rdata")
    addWorksheet(wb, sheetName = "DAWA_Adresse_Head")
    writeData(wb, sheet = "DAWA_Adresse_Head", x = head(DAWA_Adresse))
    # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    addWorksheet(wb, sheetName = "DAWA_Adresse_Summary")
    writeData(wb, sheet = "DAWA_Adresse_Summary", x = summary(DAWA_Adresse))
    rm(DAWA_Adresse)
    
  #EJF_Ejerskifte
    # EJF_Ejerskifte <- read.csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/EJF_Ejerskifte.csv")
    # save(EJF_Ejerskifte, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/EJF_Ejerskifte.Rdata" )
    # rm(EJF_Ejerskifte)
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/EJF_Ejerskifte.Rdata")
    addWorksheet(wb, sheetName = "EJF_Ejerskifte_Head")
    writeData(wb, sheet = "EJF_Ejerskifte_Head", x = head(EJF_Ejerskifte))
    # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    addWorksheet(wb, sheetName = "EJF_Ejerskifte_Summary")
    writeData(wb, sheet = "EJF_Ejerskifte_Summary", x = summary(EJF_Ejerskifte))
    rm(EJF_Ejerskifte)
    
  #EJF_Handelsoplysning
    # EJF_Handelsoplysning <- read.csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/EJF_Handelsoplysning.csv")
    # save(EJF_Handelsoplysning, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/EJF_Handelsoplysning.Rdata" )
    # rm(EJF_Handelsoplysning)
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/EJF_Handelsoplysning.Rdata")
    # addWorksheet(wb, sheetName = "EJF_Handelsoplysning_Head")
    # writeData(wb, sheet = "EJF_Handelsoplysning_Head", x = head(EJF_Handelsoplysning))
    # # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    # addWorksheet(wb, sheetName = "EJF_Handelsoplysning_Summary")
    # writeData(wb, sheet = "EJF_Handelsoplysning_Summary", x = summary(EJF_Handelsoplysning))
    # rm(EJF_Handelsoplysning)
                                  ###### Statistics on this
                                  # How many unique lokalid and therefor transactions? 
                                  iTransactions <- length(unique(EJF_Handelsoplysning$id_lokalId))
                                  # Answer = 6.792.861
                                  # When is the first transaction and last?
                                  iTransactionDate <- as.Date.character(EJF_Handelsoplysning$afstaaelsesdato)
    
# saveWorkbook(wb, file_path, overwrite = TRUE)

wb <- createWorkbook()
file_path <- "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Overbliks_Skader.xlsx"

    Data_til_KU_Toke_Emil_Panduro <- readxl::read_excel("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/Data til KU - Toke Emil Panduro.xlsx")
    save(Skader, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Skader.Rdata")
    # save(EJF_Handelsoplysning, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/EJF_Handelsoplysning.Rdata" )
    # rm(Data_til_KU_Toke_Emil_Panduro)
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/Stormrådsskader.Rdata")
    # addWorksheet(wb, sheetName = "Skaderåd_Head")
    # writeData(wb, sheet = "Skaderåd_Head", x = head(Data_til_KU_Toke_Emil_Panduro))
    # # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    # addWorksheet(wb, sheetName = "Skaderåd_Summary")
    # writeData(wb, sheet = "Skaderåd_Summary", x = summary(Data_til_KU_Toke_Emil_Panduro))
    # rm(Data_til_KU_Toke_Emil_Panduro)



saveWorkbook(wb, file_path, overwrite = TRUE)


# Heavy pricipitation  ----
  ## Load the data ----
  load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev1_Koebenhavn.rdata")
  load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev1_StorKoebenhavn.rdata")
  load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev7_aarhus.Rdata")
  load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev8_Trekanstomraade.Rdata")
  load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/ev6_Aalborg.Rdata")    


  ## Binding the data frames ----
  Precipitation <- rbind(ev1_Koebenhavn, ev1_StorKoebenhavn)
  # combining with less variables, using dplyr
  Precipitation <- dplyr::bind_rows(Precipitation, ev6_Aalborg)
  Precipitation <- dplyr::bind_rows(Precipitation, ev7_aarhus)
  Precipitation <- dplyr::bind_rows(Precipitation, ev8_Trekanstomraade)
  # Add variable to show it is precipitation
  Precipitation$Rain <- 1
  
  # save dataset 
  save(Precipitation, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_precipitation.Rdata")
  
  ## Reducing the dataframe of variables ----
  # variables of interest 
      # From other data set, clean 
      # [1] "vejnavn"                          "husnr"                            "bygning_id"                      
      #"Size"                             "Bygninganvendelse"                "Outbuilding"                     
      #"Opførelsesår"                     "Ydervægge_Materiale"              "Tagdækning_Materiale"            
      # "Varmeinstallation"                "Ombygning"                        "grund"                           
      # "husnummer"                        "Geometri_EPSG_25832"              "adr_etrs89_oest"                 
      # "adr_etrs89_nord"                  "Car_Park"                         "Garage"                          
      # "TerracedHouse"                    "<1940"                            "1940-1950"                       
      # "1950-1960"                        "1960-1970"                        "1970-1980"                       
      # "1980-1990"                        "1990-2000"                        "2000-2010"                       
      # "2010"                             "Brick"                            "Lightweightconcrete"             
      # "Wood"                             "Tile"                             "Thatched"                        
      # "Fiberasbetos"                     "Electricheating"                  "Centralheating"                  
      # "Districtheating"                  "Renovated_1940-1950"              "Renovated_1950-1960"             
      # "Renovated_1960-1970"              "Renovated_1970-1980"              "Renovated_1980-1990"             
      # "adresseIdentificerer"             "enhed_id"                         "kommunekode"                     
      # "enh020EnhedensAnvendelse"         "enh023Boligtype"                  "enh031AntalVærelser"             
      # "enh032Toiletforhold"              "etage"                            "opgang"                          
      # "vejnavn.1"                        "husnr.1"                          "postnr"                          
      # "m2"                               "dato"                             "entryAddressID"                  
      # "nominal_price"                    "year"                             "SagsID"                          
      # "Kommune"                          "Ejendomsnr"                       "Postnummer"                      
      # "Adresse"                          "Oplyst bygningskategori"          "Hændelsesdato"                   
      # "Hændelsesnavn"                    "Sagsfase"                         "Dagsværdi Løsøre"                
      # "Dagsværdi bygning"                "Dagsværdi afgrøder"               "Selvrisiko"                      
      # "Tidligere udbetalt byg/løs/afgrd" "Udbetalt genhusning"              "adresse1"                        
  
  
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
                         "flooded", "Rain")
  
  Precipitation_subset <- subset(Precipitation, select = variables_to_keep)
  Precipitation_subset$Geometri_EPSG_25832 <- paste0(Precipitation_subset$x, " ", Precipitation_subset$y)
  #Precipitation_subset$Geometri_EPSG_25832 <- sf::st_as_sfc(Precipitation_subset$Geometri_EPSG_25832, crs = 25832)
  
  # Build of some variable to merge with other data frame 
  Precipitation_subset$TerracedHouse <- ifelse(Precipitation_subset$unit_type_code == 131, 1, 0) 
  
  
  # Built year
  Precipitation_subset$'<1940' <- ifelse(Precipitation_subset$year_of_built < 1940, 1, 0)
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
  Precipitation_subset$'2010' <- ifelse(Precipitation_subset$year_of_built > 2010, 1, 0)
  
  # Renovated
  ### Renovation ----
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
  
  
  
  
  save(Precipitation_subset, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Precipitation_Subset.Rdata")
  
  
  
  
  
  
  
