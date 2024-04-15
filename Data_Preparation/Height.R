
library(sf)

# Read in height
# for iteration 0, run iteration 0 manually and store in Bygning_Total
Bygning_Total <- bygning

for (i in 1:585) {
  if (i < 10) {
  read <- paste0("/Users/mathiasliedtke/GeoDanmark60_SHP_20240310201607/Tema/bygning/bygning_000", i, "/bygning.shp")
  } else if (i < 100 & i >= 10) {
  read <- paste0("/Users/mathiasliedtke/GeoDanmark60_SHP_20240310201607/Tema/bygning/bygning_00", i, "/bygning.shp")
  } else {
  read <- paste0("/Users/mathiasliedtke/GeoDanmark60_SHP_20240310201607/Tema/bygning/bygning_0", i, "/bygning.shp")
  }
  bygning <- sf::read_sf(read)
  bygning <- subset(bygning, select = geometry)
  bygning$centroid <- sf::st_centroid(bygning$geometry)
  mHeight <- matrix(nrow = nrow(bygning), ncol = 1)
  colnames(mHeight) <- "Height"
  for (j in 1:nrow(bygning)) {
    heights <- bygning[[1]][[j]][[1]]  
    height <- mean(heights[, 3])       
    mHeight[j, 1] <- height
    
  }
  # Add height to bygning
  bygning <- cbind(bygning, mHeight)
  
  Bygning_Total <- rbind(Bygning_Total, bygning)
  
  progress_percent <- i / 585 * 100
  cat("We are at iteration", i, "meaning we are", progress_percent, "%\n", "through!")
  # Rename to dynamically 
  # assign(paste("bygning", i, sep = '_'), bygning)
}

