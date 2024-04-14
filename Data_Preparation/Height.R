
library(sf)

# Read in height

for (i in 0:585) {
  if (i < 10) {
  read <- paste0("/Users/mathiasliedtke/GeoDanmark60_SHP_20240310201607/Tema/bygning/bygning_000", i, "/bygning.shp")
  } else if (i < 100 & i > 10) {
  read <- paste0("/Users/mathiasliedtke/GeoDanmark60_SHP_20240310201607/Tema/bygning/bygning_00", i, "/bygning.shp")
  } else {
  read <- paste0("/Users/mathiasliedtke/GeoDanmark60_SHP_20240310201607/Tema/bygning/bygning_0", i, "/bygning.shp")
  }
  bygning <- sf::read_sf(read)
  assign(bygning, paste0("bygning", i))
  
  data<-read_excel(filename)
  numzone<-nrow(unique(data[c("zone")]))
  
  zones = list()
  for(i in 1:numzone) 
  {
    zonename<-paste("Zone",numzone, sep = "")
    zonename
    zones[[zonename]]<-data %>% filter(zone==numzone) 
  }
  
}
bygning0 <- sf::read_sf("/Users/mathiasliedtke/GeoDanmark60_SHP_20240310201607/Tema/bygning/bygning_0000/bygning.shp")
bygning0 <- subset(bygning0, select = geometry)
bygning0$centroid <- sf::st_centroid(bygning0$geometry)
mHeight <- matrix(nrow = nrow(bygning0), ncol = 1)
colnames(mHeight) <- "Height"
for (i in 1:nrow(bygning0)) {
  heights <- bygning0[[1]][[i]][[1]]  
  height <- mean(heights[, 3])       
  mHeight[i, 1] <- height
}
bygning0 <- cbind(bygning0, mHeight)


bygning0 <- sf::read_sf("/Users/mathiasliedtke/GeoDanmark60_SHP_20240310201607/Tema/bygning/bygning_0000/bygning.shp")
bygning0 <- subset(bygning0, select = geometry)
bygning0$centroid <- sf::st_centroid(bygning0$geometry)
heights <- bygning0[[1]][[2]][[1]] 
height <- mean(heights[,3])

