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
    addWorksheet(wb, sheetName = "BBR_Bygning_Head")
    writeData(wb, sheet = "BBR_Bygning_Head", x = head(BBR_Bygning))
    # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    addWorksheet(wb, sheetName = "BBR_Bygning_Summary")
    writeData(wb, sheet = "BBR_Bygning_Summary", x = summary(BBR_Bygning))
    rm(BBR_Bygning)

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
    addWorksheet(wb, sheetName = "EJF_Handelsoplysning_Head")
    writeData(wb, sheet = "EJF_Handelsoplysning_Head", x = head(EJF_Handelsoplysning))
    # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    addWorksheet(wb, sheetName = "EJF_Handelsoplysning_Summary")
    writeData(wb, sheet = "EJF_Handelsoplysning_Summary", x = summary(EJF_Handelsoplysning))
    rm(EJF_Handelsoplysning)
      
    
saveWorkbook(wb, file_path, overwrite = TRUE)

wb <- createWorkbook()
file_path <- "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Overbliks_Skader.xlsx"

    # Data_til_KU_Toke_Emil_Panduro <- read_excel("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/Data til KU - Toke Emil Panduro.xlsx")   
    # save(Data_til_KU_Toke_Emil_Panduro, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/Stormrådsskader.Rdata")
    # rm(Data_til_KU_Toke_Emil_Panduro)
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/mathias/Stormrådsskader.Rdata")
    addWorksheet(wb, sheetName = "Skaderåd_Head")
    writeData(wb, sheet = "Skaderåd_Head", x = head(Data_til_KU_Toke_Emil_Panduro))
    # BBB_Bygning_Summary <- as.data.frame(summary(BBR_Bygning))
    addWorksheet(wb, sheetName = "Skaderåd_Summary")
    writeData(wb, sheet = "Skaderåd_Summary", x = summary(Data_til_KU_Toke_Emil_Panduro))
    rm(Data_til_KU_Toke_Emil_Panduro)

saveWorkbook(wb, file_path, overwrite = TRUE)
    