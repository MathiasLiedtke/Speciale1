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
