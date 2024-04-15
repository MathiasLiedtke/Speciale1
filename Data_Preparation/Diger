
library(sf)

# Read in height
for (i in 0:10) {
  if (i < 10) {
    read <- paste0("/Users/mathiasliedtke/GeoDanmark60_SHP_20240310201607/Tema/dige/dige_000", i, "/dige.shp")
  } else if (i >= 10) {
    read <- paste0("/Users/mathiasliedtke/GeoDanmark60_SHP_20240310201607/Tema/dige/dige_00", i, "/dige.shp")
  }
  dige <- sf::read_sf(read)
  dige <- subset(dige, select = c(objectid, eksisterer, geometry))
  dige <- subset(dige, eksisterer == 1)
  
  if (i == 0) {
    Dige_Total <- dige
  } else {
    Dige_Total <- rbind(Dige_Total, dige)  
  }
  
  
  progress_percent <- i / 10 * 100
  cat("We are at iteration", i, "meaning we are", progress_percent, "%\n", "through!")
  
}

save(Dige_Total, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Dige_Total.Rdata")







