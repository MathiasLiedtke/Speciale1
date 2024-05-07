

# 
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df.Rdata")
# 3982075 observations. 

# How many entities 
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/BBR_Bygning.rdata")
      # How many buildings and entities 
      Buildings <- length(unique(BBR_Bygning$bygning_id))
      # 4869043
      # How many are private residences 
      ## Reduce observation by limiting real estate
      Residences <- count(BBR_Bygning, byg021BygningensAnvendelse == 110 | byg021BygningensAnvendelse == 120 |
                            byg021BygningensAnvendelse == 121 | byg021BygningensAnvendelse == 122 | byg021BygningensAnvendelse == 130 |
                            byg021BygningensAnvendelse == 131 | byg021BygningensAnvendelse == 132 | byg021BygningensAnvendelse == 140 |
                            byg021BygningensAnvendelse == 190 | byg021BygningensAnvendelse == 510 | byg021BygningensAnvendelse == 910 |
                            byg021BygningensAnvendelse == 920 | byg021BygningensAnvendelse == 930 | byg021BygningensAnvendelse == 940 |
                            byg021BygningensAnvendelse == 950 | byg021BygningensAnvendelse == 960) 

      
      # Density of fixed price
      Density_Sales_price <- density(Total_df_18_v2$sales_price)
      x <- seq(1, 512)
      df <- data.frame(x = x, Density_Sales_price = Density_Sales_price$y)
      Nominal_price_plot <- ggplot2::ggplot(df, ggplot2::aes(x)) +
        ggplot2::geom_line(aes(y = Density_Sales_price, color = "Density 1"), size = 1) +
        labs(x = "Frequency", y = "Price", title = "Density for nominal price") +
        scale_color_manual(name = "Legend", values = "green") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),  # Center the title
          legend.background = element_rect(fill = "white", color = "black")
        )
      plot(Nominal_price_plot)
      
      
      
  
  # Summary statistics of variables 
      # Descriptives ----
      library(gt)
      library(gtExtras)
      
      Total_df_18_v2_df <- sf::st_drop_geometry(Total_df_18_v2)
      
      colnames <- c("sales_price", "Height", "m2", "Outbuilding", "TerracedHouse", "rooms", "Hændelsesdato",
                    "Udbetaling", "flooded", "forest_distance", "coastline_distance","powerline_distance", 
                    "railway_distance", "coastline_distance", "Trainstation_distance", "Wateryarea_distance", 
                    "Car_Garage", "Built", "Renovated", "Heating", "Roof", "BMaterial")
      
      summary_statistics <- data.frame(matrix(data = NA, nrow = 0, ncol = 9))
      Colnames <- c("Type of variable", "Min", "1st quartile", "Median", "Mean", "3rd quartile", "Max", "# Obs", "Distribution")
      colnames(summary_statistics) <- Colnames
      
                # Price 
                   i = "sales_price" 
                   summary_statistics[1,2] <- quantile(Total_df_18_v2_df[,i], probs = 0, na.rm = TRUE)
                   summary_statistics[1,3] <- quantile(Total_df_18_v2_df[,i], probs = 0.25, na.rm = TRUE)
                   summary_statistics[1,4] <- quantile(Total_df_18_v2_df[,i], probs = 0.5, na.rm = TRUE)
                   summary_statistics[1,5] <- colMeans(Total_df_18_v2_df[,i])
                   summary_statistics[1,6] <- quantile(Total_df_18_v2_df[,i], probs = 0.75, na.rm = TRUE)
                   summary_statistics[1,7] <- quantile(Total_df_18_v2_df[,i], probs = 1, na.rm = TRUE)
                   summary_statistics[1,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics) <- "Sales Price"
                   
                # M2 
                   i = "m2" 
                   summary_statistics[2,2] <- quantile(Total_df_18_v2_df[,i], probs = 0, na.rm = TRUE)
                   summary_statistics[2,3] <- quantile(Total_df_18_v2_df[,i], probs = 0.25, na.rm = TRUE)
                   summary_statistics[2,4] <- quantile(Total_df_18_v2_df[,i], probs = 0.5, na.rm = TRUE)
                   summary_statistics[2,5] <- colMeans(Total_df_18_v2_df[,i])
                   summary_statistics[2,6] <- quantile(Total_df_18_v2_df[,i], probs = 0.75, na.rm = TRUE)
                   summary_statistics[2,7] <- quantile(Total_df_18_v2_df[,i], probs = 1, na.rm = TRUE)
                   summary_statistics[2,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[2] <- "# M2"   
                  
                # rooms 
                   i = "rooms" 
                   summary_statistics[3,2] <- quantile(Total_df_18_v2_df[,i], probs = 0, na.rm = TRUE)
                   summary_statistics[3,3] <- quantile(Total_df_18_v2_df[,i], probs = 0.25, na.rm = TRUE)
                   summary_statistics[3,4] <- quantile(Total_df_18_v2_df[,i], probs = 0.5, na.rm = TRUE)
                   summary_statistics[3,5] <- colMeans(Total_df_18_v2_df[,i])
                   summary_statistics[3,6] <- quantile(Total_df_18_v2_df[,i], probs = 0.75, na.rm = TRUE)
                   summary_statistics[3,7] <- quantile(Total_df_18_v2_df[,i], probs = 1, na.rm = TRUE)
                   summary_statistics[3,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[3] <- "# Rooms" 
                   
                # Outbuilding 
                   i = "Outbuilding" 
                   j = 4
                   summary_statistics[j,2] <- quantile(Total_df_18_v2_df[,i], probs = 0, na.rm = TRUE)
                   summary_statistics[j,3] <- quantile(Total_df_18_v2_df[,i], probs = 0.25, na.rm = TRUE)
                   summary_statistics[j,4] <- quantile(Total_df_18_v2_df[,i], probs = 0.5, na.rm = TRUE)
                   summary_statistics[j,5] <- colMeans(Total_df_18_v2_df[,i])
                   summary_statistics[j,6] <- quantile(Total_df_18_v2_df[,i], probs = 0.75, na.rm = TRUE)
                   summary_statistics[j,7] <- quantile(Total_df_18_v2_df[,i], probs = 1, na.rm = TRUE)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "#M2 Outbuilding" 
                   
                # Car Garage  
                   i = "Car_Garage" 
                   j = 5
                   summary_statistics[j,2] <- quantile(Total_df_18_v2_df[,i], probs = 0, na.rm = TRUE)
                   summary_statistics[j,3] <- quantile(Total_df_18_v2_df[,i], probs = 0.25, na.rm = TRUE)
                   summary_statistics[j,4] <- quantile(Total_df_18_v2_df[,i], probs = 0.5, na.rm = TRUE)
                   summary_statistics[j,5] <- colMeans(Total_df_18_v2_df[,i])
                   summary_statistics[j,6] <- quantile(Total_df_18_v2_df[,i], probs = 0.75, na.rm = TRUE)
                   summary_statistics[j,7] <- quantile(Total_df_18_v2_df[,i], probs = 1, na.rm = TRUE)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "#M2 Carport/Garage"
                   
                # Built before 1940 
                   i = "Built" 
                   j = 6
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[1]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Built < 1940"
                   
                   j = 7
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[2]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Built 1940-1950"
                   
                   j = 8
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[3]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Built 1950-1960"
                   
                   j = 9
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[4]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Built 1960-1970"
                   
                   j = 10
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[5]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Built 1970-1980"
                   
                   j = 11
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[6]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Built 1980-1990"
                   
                   j = 12
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[7]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Built 1990-2000"
                   
                   j = 13
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[8]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Built 2000-2010"
                   
                   j = 14
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[9]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Built > 2010"
                  
                # Renovated
                   i = "Renovated" 
                   j = 15
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[1]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Renovated 1940-1950"
                   
                   j = 16
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[2]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Renovated 1950-1960"
                   
                   j = 17
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[3]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Renovated 1960-1970"
                   
                   j = 18
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[4]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Renovated 1970-1980"
                   
                   j = 19
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[5]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Renovated 1980-1990"
                   
                    
                # Roof
                   i = "Roof" 
                   j = 20
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[2]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Tile Roof"
                   
                   j = 21
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[3]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Thatch Roof"
                   
                   j = 22
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[4]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Fibercement asbestos Roof"
                   
                   j = 23
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[1]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Other (Roof)"
                   
                # Heating
                   i = "Heating" 
                   j = 24
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[2]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "District Heating"
                   
                   j = 25
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[3]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Central Heating"
                   
                   j = 26
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[4]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Electric Heating"
                   
                   j = 27
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[1]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Other (Heating)"
                   
                # Builtmaterials
                   i = "BMaterial" 
                   j = 24
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[2]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Brick"
                   
                   j = 25
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[3]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Wood"
                   
                   j = 26
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[4]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Lightweight concrete"
                   
                   j = 27
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[1]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Other (Built material)"
                   
                # Terraced house
                   i = "TerracedHouse" 
                   j = 28
                   summary_statistics[j,5] <- table(Total_df_18_v2_df[,i])[2]/nrow(Total_df_18_v2_df)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Terraced House 1/0"
                
                   
                # Height 
                   i = "Height" 
                   j = 29
                   summary_statistics[j,2] <- quantile(Total_df_18_v2_df[,i], probs = 0, na.rm = TRUE)
                   summary_statistics[j,3] <- quantile(Total_df_18_v2_df[,i], probs = 0.25, na.rm = TRUE)
                   summary_statistics[j,4] <- quantile(Total_df_18_v2_df[,i], probs = 0.5, na.rm = TRUE)
                   summary_statistics[j,5] <- colMeans(Total_df_18_v2_df[,i])
                   summary_statistics[j,6] <- quantile(Total_df_18_v2_df[,i], probs = 0.75, na.rm = TRUE)
                   summary_statistics[j,7] <- quantile(Total_df_18_v2_df[,i], probs = 1, na.rm = TRUE)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Elavation"
                   
                # forest 
                   i = "forest_distance" 
                   j = 30
                   summary_statistics[j,2] <- quantile(Total_df_18_v2_df[,i], probs = 0, na.rm = TRUE)
                   summary_statistics[j,3] <- quantile(Total_df_18_v2_df[,i], probs = 0.25, na.rm = TRUE)
                   summary_statistics[j,4] <- quantile(Total_df_18_v2_df[,i], probs = 0.5, na.rm = TRUE)
                   summary_statistics[j,5] <- colMeans(Total_df_18_v2_df[,i])
                   summary_statistics[j,6] <- quantile(Total_df_18_v2_df[,i], probs = 0.75, na.rm = TRUE)
                   summary_statistics[j,7] <- quantile(Total_df_18_v2_df[,i], probs = 1, na.rm = TRUE)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Distance to forest"
                   
                # coast line 
                   i = "coastline_distance" 
                   j = 31
                   summary_statistics[j,2] <- quantile(Total_df_18_v2_df[,i], probs = 0, na.rm = TRUE)
                   summary_statistics[j,3] <- quantile(Total_df_18_v2_df[,i], probs = 0.25, na.rm = TRUE)
                   summary_statistics[j,4] <- quantile(Total_df_18_v2_df[,i], probs = 0.5, na.rm = TRUE)
                   summary_statistics[j,5] <- colMeans(Total_df_18_v2_df[,i])
                   summary_statistics[j,6] <- quantile(Total_df_18_v2_df[,i], probs = 0.75, na.rm = TRUE)
                   summary_statistics[j,7] <- quantile(Total_df_18_v2_df[,i], probs = 1, na.rm = TRUE)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Distance to Coast"
                   
                # Powerline 
                   i = "powerline_distance" 
                   j = 32
                   summary_statistics[j,2] <- quantile(Total_df_18_v2_df[,i], probs = 0, na.rm = TRUE)
                   summary_statistics[j,3] <- quantile(Total_df_18_v2_df[,i], probs = 0.25, na.rm = TRUE)
                   summary_statistics[j,4] <- quantile(Total_df_18_v2_df[,i], probs = 0.5, na.rm = TRUE)
                   summary_statistics[j,5] <- colMeans(Total_df_18_v2_df[,i])
                   summary_statistics[j,6] <- quantile(Total_df_18_v2_df[,i], probs = 0.75, na.rm = TRUE)
                   summary_statistics[j,7] <- quantile(Total_df_18_v2_df[,i], probs = 1, na.rm = TRUE)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Distance to Powerline"
                   
                # railway 
                   i = "railway_distance" 
                   j = 33
                   summary_statistics[j,2] <- quantile(Total_df_18_v2_df[,i], probs = 0, na.rm = TRUE)
                   summary_statistics[j,3] <- quantile(Total_df_18_v2_df[,i], probs = 0.25, na.rm = TRUE)
                   summary_statistics[j,4] <- quantile(Total_df_18_v2_df[,i], probs = 0.5, na.rm = TRUE)
                   summary_statistics[j,5] <- colMeans(Total_df_18_v2_df[,i])
                   summary_statistics[j,6] <- quantile(Total_df_18_v2_df[,i], probs = 0.75, na.rm = TRUE)
                   summary_statistics[j,7] <- quantile(Total_df_18_v2_df[,i], probs = 1, na.rm = TRUE)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Distance to Railway"
                   
                # Trainstaion 
                   i = "Trainstation_distance" 
                   j = 34
                   summary_statistics[j,2] <- quantile(Total_df_18_v2_df[,i], probs = 0, na.rm = TRUE)
                   summary_statistics[j,3] <- quantile(Total_df_18_v2_df[,i], probs = 0.25, na.rm = TRUE)
                   summary_statistics[j,4] <- quantile(Total_df_18_v2_df[,i], probs = 0.5, na.rm = TRUE)
                   summary_statistics[j,5] <- colMeans(Total_df_18_v2_df[,i])
                   summary_statistics[j,6] <- quantile(Total_df_18_v2_df[,i], probs = 0.75, na.rm = TRUE)
                   summary_statistics[j,7] <- quantile(Total_df_18_v2_df[,i], probs = 1, na.rm = TRUE)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Distance to Train station"
                   
                # lake 
                   i = "lake_distance" 
                   j = 35
                   summary_statistics[j,2] <- quantile(Total_df_18_v2_df[,i], probs = 0, na.rm = TRUE)
                   summary_statistics[j,3] <- quantile(Total_df_18_v2_df[,i], probs = 0.25, na.rm = TRUE)
                   summary_statistics[j,4] <- quantile(Total_df_18_v2_df[,i], probs = 0.5, na.rm = TRUE)
                   summary_statistics[j,5] <- colMeans(Total_df_18_v2_df[,i])
                   summary_statistics[j,6] <- quantile(Total_df_18_v2_df[,i], probs = 0.75, na.rm = TRUE)
                   summary_statistics[j,7] <- quantile(Total_df_18_v2_df[,i], probs = 1, na.rm = TRUE)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Distance to Lake"
                   
                   # wateryarea 
                   i = "Wateryarea_distance" 
                   j = 36
                   summary_statistics[j,2] <- quantile(Total_df_18_v2_df[,i], probs = 0, na.rm = TRUE)
                   summary_statistics[j,3] <- quantile(Total_df_18_v2_df[,i], probs = 0.25, na.rm = TRUE)
                   summary_statistics[j,4] <- quantile(Total_df_18_v2_df[,i], probs = 0.5, na.rm = TRUE)
                   summary_statistics[j,5] <- colMeans(Total_df_18_v2_df[,i])
                   summary_statistics[j,6] <- quantile(Total_df_18_v2_df[,i], probs = 0.75, na.rm = TRUE)
                   summary_statistics[j,7] <- quantile(Total_df_18_v2_df[,i], probs = 1, na.rm = TRUE)
                   summary_statistics[j,8] <- nrow(Total_df_18_v2_df)
                   rownames(summary_statistics)[j] <- "Distance to wet area"
                   
      
                   
              
      
      gt_tbl <- gt(summary_statistics)
      
      
      gt_tbl <- gt_tbl %>%
        tab_header(
          title = "Summary of variables",
        ) %>% 
        
      
      print(gt_tbl)
      
      
      write.csv(Total_df_18_v2_df, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Deskrivtive/Data.csv", row.names = TRUE)
      write.csv(summary_statistics, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Deskrivtive/summary_statistics.csv", row.names = TRUE)
      
      Data_Descriptives <- read_excel("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Deskrivtive/Data_Descriptives.xlsx")
      
      Density_Height <- density(Data_Descriptives$Height)
      x <- seq(1, 512)
      df <- data.frame(x = x, Density1 = Density_Height$y)
      
      Densities <- matrix(data = NA, nrow = length(x), ncol = ncol(Data_Descriptives))
      colnames(Densities) <- colnames(Data_Descriptives)
      colnames <- colnames(Densities)
      
      Densities[,1] <- df[,2]
      
      Density_m2 <- density(Data_Descriptives$m2)
      df <- data.frame(x = x, Density1 = Density_m2$y)
      Densities[,2] <- df[,2]
      
      Density_nominal_price <- density(Data_Descriptives$nominal_price)
      df <- data.frame(x = x, Density1 = Density_nominal_price$y)
      Densities[,3] <- df[,2]
      
      Density_Outbuilding <- density(Data_Descriptives$Outbuilding)
      df <- data.frame(x = x, Density1 = Density_Outbuilding$y)
      Densities[,4] <- df[,2]
      
      Density_rooms <- density(Data_Descriptives$rooms)
      df <- data.frame(x = x, Density1 = Density_rooms$y)
      Densities[,5] <- df[,2]
      
      Density_forest_distance <- density(Data_Descriptives$forest_distance)
      df <- data.frame(x = x, Density1 = Density_forest_distance$y)
      Densities[,8] <- df[,2]
      
      Density_coastline_distance <- density(Data_Descriptives$coastline_distance)
      df <- data.frame(x = x, Density1 = Density_coastline_distance$y)
      Densities[,9] <- df[,2]
      
      Density_powerline_distance <- density(Data_Descriptives$powerline_distance)
      df <- data.frame(x = x, Density1 = Density_powerline_distance$y)
      Densities[,10] <- df[,2]
      
      Density_railway_distance <- density(Data_Descriptives$railway_distance)
      df <- data.frame(x = x, Density1 = Density_railway_distance$y)
      Densities[,11] <- df[,2]
      
      Density_lake_distance <- density(Data_Descriptives$lake_distance)
      df <- data.frame(x = x, Density1 = Density_lake_distance$y)
      Densities[,12] <- df[,2]
      
      Density_Trainstation_distance <- density(Data_Descriptives$Trainstation_distance)
      df <- data.frame(x = x, Density1 = Density_Trainstation_distance$y)
      Densities[,13] <- df[,2]
      
      Density_Wateryarea_distance <- density(Data_Descriptives$Wateryarea_distance)
      df <- data.frame(x = x, Density1 = Density_Wateryarea_distance$y)
      Densities[,14] <- df[,2]
      
      Density_sales_price <- density(Data_Descriptives$sales_price)
      df <- data.frame(x = x, Density1 = Density_sales_price$y)
      Densities[,15] <- df[,2]
      
      Density_Udbetaling <- density(Data_Descriptives$Udbetaling)
      df <- data.frame(x = x, Density1 = Density_Udbetaling$y)
      Densities[,15] <- df[,2]
      
      
      write.csv(Densities, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Deskrivtive/Densities.csv", row.names = TRUE)

      
      
      
      # Map of Denmark with different sales prices. 
    # hent postnr 
      library(leaflet)
      API_GEO <- "https://api.dataforsyningen.dk/DAGI_10MULTIGEOM_GMLSFP_DAF?service=WFS&request=GetCapabilities&token="
      token <- readline(prompt="Please enter token: ")
      url <- paste0(API_GEO,token)
      response <- httr::GET(url)
      httr::status_code(response)
      
      # Define the typename for the feature you want to load
      typename <- "Postnummerinddeling"
      
      wfs_client <- WFSClient$new(url, serviceVersion = "2.0.0")
      
      # Build the URL for the GetFeature request
      request_url <- httr::parse_url(wfs_client$url)
      request_url$query <- list(
        service = "wfs",
        version = "2.0.0",
        request = "GetFeature",
        typenames = typename,
        srsName = "EPSG:25832",
        token = "043c0c7bbb4086890a5c6ef6dd2075e4"
      )
      
      # Make the GetFeature request and read the response into a simple feature object
      feature <- sf::read_sf(httr::build_url(request_url))
      
      postnr <- subset(feature, select = c(navn, postnummer, geometri))
      postnr <- sf::st_zm(postnr)
      
      mean_postnr <- Total_df_18_v2 %>%
        group_by(postnr) %>%
        summarise(mean_price = mean(sales_price, na.rm = TRUE))
      
      
      mean_postnr <- as(mean_postnr, "Spatial")
      postnr <- sf::st_zm(postnr)
      postnr <- as(postnr, "Spatial")
      
      summary_postnr <- merge(postnr, mean_postnr, by.x = postnummer, by.y = postnr)

      
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
      
      
    
      
      Total_df_18_v2 <- Total_df_18_v2 %>%
        group_by(postnr) %>%
        summarise(mean_price = mean(sales_price, na.rm = TRUE))
      
      pal <- colorNumeric(palette = "viridis", domain = Total_df_18_v2$mean_price)
      
      leaflet(Total_df_18_v2) %>%
        addTiles() %>%
        addCircleMarkers(~sf::st_coordinates(Coor)[,1], ~st_coordinates(Coor)[,2], radius = ~mean_price/100000,
                         color = ~pal(mean_price), stroke = FALSE, fillOpacity = 0.5) %>%
        addLegend("bottomright", pal = pal, values = ~mean_price,
                  title = "Mean Price",
                  opacity = 1)
      
      
      
      