####
####



# Library ----
library(stats) # For linear regression estimation, functions as lm
library(tidyverse) # use to arrange sf objects
library(GWmodel) # For Geographically weighted models
library(spgwr) # For Geographically weighted models
library(rgdal)
library(maptools)
library(spdep) #SAR model
library(spatialreg) #SAR model
library(xgboost) # For xgboosting
library(tree) # visualize trees 
library(dplyr) # for practical functions
library(lmtest) # test for heteroscedasticity
library(sandwich)

------------------------------------------------------------------------
  
  # Load in data ----

load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_18_v2.Rdata")

Total_df <-  Total_df_18_v2
Total_df <- subset(Total_df, select = - `Tidligere udbetalt byg/løs/afgrd`)
Total_df$sales_price <- log(Total_df$sales_price)
Total_df <- subset(Total_df, postnr<5000)
postnr <- subset(Total_df, in_both == TRUE)
rm(Total_df_18_v2)
------------------------------------------------------------------------
  
  
  # Partition of training and data set ----
# Create training sets ----
# Partition 60% for training 
p <- 0.6    
iT <- p*nrow(Total_df)
# iT <- 10000
## Train 1
set.seed(13)
train_seq_1 <- sample(nrow(Total_df), size = iT, replace = FALSE)
train_set_1 <- Total_df[train_seq_1, , drop = FALSE]
## Test 1
test_set_1 <- Total_df[-train_seq_1, , drop = FALSE]


p <- 0.4    
iT <- p*nrow(Total_df)
## Train 2
set.seed(200)
train_seq_2 <- sample(nrow(Total_df), size = iT, replace = FALSE)
train_set_2 <- Total_df[train_seq_2, , drop = FALSE]
## Test 2
test_set_2 <- Total_df[-train_seq_2, , drop = FALSE]

------------------------------------------------------------------------
  
  # Preliminary analysis ----
## Test for spatial autocorrelation ----
### Train Set 1  ----
T1 <- Sys.time()
train_set_1 <- sf::st_as_sf(train_set_1)
train_set_1 <- as(train_set_1, "Spatial")
points_train_1 <- sp::coordinates(train_set_1)
# Neighbor_train1_CPH <- spdep::tri2nb(points_train_1)  #When calculate neighbor
T2 <- Sys.time() - T1 # 20 min
# save(Neighbor_train1_CPH, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_train1_CPH.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_train1_CPH.Rdata")

### Train Set 2 ----
T3 <- Sys.time()
train_set_2 <- sf::st_as_sf(train_set_2)
train_set_2 <- as(train_set_2, "Spatial")
points_train_2 <- sp::coordinates(train_set_2)
# Neighbor_train2_CPH <- spdep::tri2nb(points_train_2)  #When calculate neighbor
T4 <- Sys.time() - T3 # 20 min
# save(Neighbor_train2_CPH, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_train2_CPH.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_train2_CPH.Rdata")


### Test Set 1  ----
T5 <- Sys.time()
test_set_1 <- sf::st_as_sf(test_set_1)
test_set_1 <- as(test_set_1, "Spatial")
points_test_1 <- sp::coordinates(test_set_1)
# Neighbor_test1_CPH <- spdep::tri2nb(points_test_1)  #When calculate neighbor
T6 <- Sys.time() - T5 # 20 min
# save(Neighbor_test1_CPH, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_test1_CPH.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_test1_CPH.Rdata")

### Test Set 2 ----
T7 <- Sys.time()
test_set_2 <- sf::st_as_sf(test_set_2)
test_set_2 <- as(test_set_2, "Spatial")
points_test_2 <- sp::coordinates(test_set_2)
# Neighbor_test2_CPH <- spdep::tri2nb(points_test_2)  #When calculate neighbor
T8 <- Sys.time() - T7 # 20 min
# save(Neighbor_test2_CPH, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_test2_CPH.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_test2_CPH.Rdata")

### Moran and geary test ----
#### Neighbor_train1_CPH ----
# Test for normality
data <- train_set_1$sales_price
par(mfrow = c(2,1)) # Set up two windows for plots
qqnorm(data, main = 'Q-Q Plot for Normality (Train set 1)', xlab = 'Theoretical Dist',
       ylab = 'Sample dist', col = 'steelblue')
qqline(data, col = 'red', lwd = 2, lty = 2)

# Seems fairly normal in distribution, continue to tests 
Neighbor_train1_weight <- spdep::nb2listw(Neighbor_train1_CPH) # Define nb2listw object 
Moran <- spdep::moran.test(train_set_1$sales_price, listw = Neighbor_train1_weight)
Geary <- spdep::geary.test(train_set_1$sales_price, Neighbor_train1_weight)    
# Sign of auto-correlation 
# Test for normality
#### Neighbor_train2 ----
data <- train_set_2$sales_price
qqnorm(data, main = 'Q-Q Plot for Normality (Train set 2)', xlab = 'Theoretical Dist',
       ylab = 'Sample dist', col = 'steelblue')
qqline(data, col = 'red', lwd = 2, lty = 2)

# Seems fairly normal in distribution, continue to tests 
Neighbor_train2_weight <- spdep::nb2listw(Neighbor_train2_CPH) # Define nb2listw object 
Moran <- spdep::moran.test(train_set_2$sales_price, listw = Neighbor_train2_weight)
Geary <- spdep::geary.test(train_set_2$sales_price, Neighbor_train2_weight)    
# Sign of auto-correlation 

# Analysis ----

## Regression ----  
### Linear regression with zip codes ----
#### Train set 1 ----  
pred_lm <- colnames(subset(Total_df, 
                           select = - c(rowname, nominal_price, sales_price, 
                                        addressID, enhed_id, flooded, SA_EV1, SA_EV2, 
                                        SA_EV3, SA_EV4, SA_EV5, postnr, Dato, Hændelsesdato, 
                                        Coor, Lag_price, in_both, in_both_skader, Udbetaling))) #Variables to not include as predictors

Formula <- as.formula(paste("sales_price ~", 
                            paste(c(pred_lm[1:19], "flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5"), collapse=" + ")))

lm_areas_lm_t1 <- stats::lm(formula = Formula, train_set_1) # Estimate model
summary_lm_t1 <- summary(lm_areas_lm_t1)[["coefficients"]]

# See if heteroscedistic
plot(lm_areas_lm_t1$fitted.values, lm_areas_lm_t1$residuals)
#add a horizontal line at y=0 
abline(0,0)
# Does not look like heteroscedasticity
# Breuch pagan test 
bptest(lm_areas_lm_t1)
# But test says so
summary_lm_t1_robust <- lmtest::coeftest(lm_areas_lm_t1, 
                                         vcov = vcovHC(lm_areas_lm_t1, type = 'HC0'))

#RMSE on training data 
rmse_training_Lm <- sqrt(sum((lm_areas_lm_t1$residuals)^2)/nrow(train_set_1)) # 0.6049038


# Spatial autocorrelation in error terms? 
moran_lm_zealand <- spdep::moran.test(lm_areas_lm_t1$residuals, listw = Neighbor_train1_weight)

#### Test ----
test_set_1$yhat <- stats::predict.lm(lm_areas_lm_t1, newdata = test_set_1) 
RMSE_LM <- sqrt(sum((test_set_1$yhat-test_set_1$sales_price)^2)/nrow(test_set_1)) #0.6914937
MSE_LM <- sum((test_set_1$yhat-test_set_1$sales_price)^2)/nrow(test_set_1) #0.4781635
MAE_LM <- sum(abs(test_set_1$yhat-test_set_1$sales_price))/nrow(test_set_1) #0.5263211
# 0.3422699

#### Train set 2 ----
pred_lm <- colnames(subset(Total_df, 
                           select = - c(rowname, nominal_price, sales_price, 
                                        addressID, enhed_id, flooded, SA_EV1, SA_EV2, 
                                        SA_EV3, SA_EV4, SA_EV5, postnr, Dato, Hændelsesdato, 
                                        Coor, Lag_price, in_both, in_both_skader, Udbetaling))) #Variables to not include as predictors

Formula <- as.formula(paste("sales_price ~", 
                            paste(c(pred_lm[1:22], "flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5"), collapse=" + ")))

lm_areas_lm_t2 <- stats::lm(formula = Formula, train_set_2) # Estimate model
summary_lm_t2 <- summary(lm_areas_lm_t2)[["coefficients"]]

# See if heteroscedistic
plot(lm_areas_lm_t2$fitted.values, lm_areas_lm_t2$residuals)
#add a horizontal line at y=0 
abline(0,0)
# Does not look like heteroscedasticity
# Breuch pagan test 
bptest(lm_areas_lm_t2)
# But test says so
summary_lm_t2_robust <- coeftest(lm_areas_lm_t2, 
                                 vcov = vcovHC(lm_areas_lm_t2, type = 'HC0'))



### SAR ----
#### Train set 1 ----  
# I think the train set should be changed to a regular data frame. 
train_set_1_dataframe <- sf::st_drop_geometry(train_set_1)
train_set_1_dataframe$Udbetaling <- log(train_set_1_dataframe$Udbetaling)

# Predictors
pred_sar <- colnames(subset(Total_df, 
                           select = - c(rowname, nominal_price, sales_price, 
                                        addressID, enhed_id, flooded, SA_EV1, SA_EV2, 
                                        SA_EV3, SA_EV4, SA_EV5, postnr, Dato, Hændelsesdato, 
                                        Coor, Lag_price, in_both, in_both_skader, Udbetaling)))

# Formula 
Formula <- as.formula(paste("sales_price ~", 
                            paste(c(pred_sar[1:19], "flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5"), collapse=" + ")))

# Model Estimation
Time <- Sys.time()
SAR_DF_t1 <- spatialreg::lagsarlm(formula = Formula, data = train_set_1_dataframe,
                                  listw = Neighbor_train1_weight)
Stoptime <- Sys.time() - Time  # 1.346548 hours
save(SAR_DF_t1, file ="/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/SAR_DF_t1_ZEALAND.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/SAR_DF_t1_ZEALAND.Rdata")
# Check data        
# Standard errors 
# See if heteroscedistic
plot(SAR_DF_t1$fitted.values, SAR_DF_t1$residuals)
#add a horizontal line at y=0 
abline(0,0)
# Does not look like heteroscedasticity
# Breuch pagan test 
bptest.Sarlm(SAR_DF_t1)
# But test says so


#Sign of heteroscedasticity 
Formula <- as.formula(paste("sales_price ~ Height + m2 + Outbuilding + 
            TerracedHouse + rooms + flooded + forest_distance + coastline_distance + railway_distance + 
            lake_distance + Trainstation_distance + Wateryarea_distance + Car_Garage + Built + 
            Renovated + Heating + Roof + BMaterial + powerline_distance + 
            flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5")) 
Formula <- as.formula(paste("sales_price ~ Height + m2 + Outbuilding +
                            TerracedHouse + rooms + flooded + forest_distance + coastline_distance + railway_distance +
                            lake_distance + Trainstation_distance + Wateryarea_distance + Car_Garage + Built +
                            Renovated + Heating + Roof + BMaterial + powerline_distance + 
                              flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV4 + flooded*SA_EV5"))

Time <- Sys.time()
SAR_DF_t1_het <- sphet::spreg(formula = Formula, data = train_set_1_dataframe,
                              listw = Neighbor_train1_weight, model = "lag", het = TRUE)
Stoptime <- Sys.time() - Time
summary_sar_t1 <- summary(SAR_DF_t1_het)

moran_SAR <- spdep::moran.test(SAR_DF_t1_het$residuals, listw = Neighbor_train1_weight)

#### Test set 1 ----
# A stupid way, but cannot think of something else
test_SAR <- Total_df[-train_seq_1, , drop = FALSE]
test_SAR <- test_SAR %>%
  rowwise() %>%
  mutate(yhat = 1.0504e+00 +
           8.7024e-05 * Height +
           2.1203e-04 * m2 -
           8.3913e-05 * Outbuilding +
           1.0133e-02 * TerracedHouse +
           1.8906e-03 * rooms +
           6.4249e-04 * flooded +
           1.0882e-06 * forest_distance -
           1.3636e-07 * coastline_distance +
           1.2560e-06 * railway_distance +
           6.1133e-06 * lake_distance -
           1.3045e-06 * Trainstation_distance -
           8.4306e-07 * Wateryarea_distance +
           ifelse(Built == 2, 4.3150e-03,
                  ifelse(Built == 3, 9.5223e-03,
                         ifelse(Built == 4, 9.9116e-03,
                                ifelse(Built == 5, 1.2459e-02,
                                       ifelse(Built == 6, 1.8372e-02,
                                              ifelse(Built == 7, 6.9712e-03,
                                                     ifelse(Built == 8, 1.7024e-02,
                                                            ifelse(Built == 9, -3.4261e-03, 0)))))))) +
           ifelse(Renovated == 1, 4.7904e-03,
                  ifelse(Renovated == 2, 4.9739e-03,
                         ifelse(Renovated == 3, 3.0252e-03,
                                ifelse(Renovated == 4, 6.6126e-04, 3.5224e-03)))) +
           ifelse(Heating == 1, 9.6215e-03,
                  ifelse(Heating == 2, 1.0645e-02, 1.5045e-03)) +
           ifelse(Roof == 1, 3.3845e-03,
                  ifelse(Roof == 2, 7.0370e-03, -1.9786e-03)) +
           ifelse(BMaterial == 1, 3.7496e-03,
                  ifelse(BMaterial == 2, -1.8021e-03, -1.3316e-03)) +
           2.0075e-06 * powerline_distance +
           SA_EV1 * 3.1455e-02 +
           SA_EV2 * 4.0247e-02 +
           SA_EV4 * 4.7357e-02 +
           SA_EV5 * 5.7797e-02 +
           flooded * (SA_EV1 * 1.0401e-02 +
                        SA_EV2 * 1.2123e-03 +
                        SA_EV4 * -2.9103e-04 +
                        SA_EV5 * 9.8722e-04) +
           5.6826e-01 * log(Lag_price))

          # RMSE 
          RMSE_SAR <- sqrt(sum((test_SAR$yhat-test_SAR$sales_price)^2)/nrow(test_SAR)) #
          MSE_LM <- sum((test_SAR$yhat-test_SAR$sales_price)^2)/nrow(test_SAR) #
          MAE_LM <- sum(abs(test_SAR$yhat-test_SAR$sales_price))/nrow(test_SAR) #
          
          # train rmse
          SAR_TRAIN_RMSE <- sqrt(sum((SAR_DF_t1_het$residuals)^2)/length(SAR_DF_t1_het$residuals))
          


#### Train set 2 ----
# I think the train set should be changed to a regular data frame. 
train_set_2_dataframe <- sf::st_drop_geometry(train_set_2) 

# Predictors
  pred_sar <- colnames(subset(Total_df, 
                              select = - c(rowname, nominal_price, sales_price, 
                                           addressID, enhed_id, flooded, SA_EV1, SA_EV2, 
                                           SA_EV3, SA_EV4, SA_EV5, postnr, Dato, Hændelsesdato, 
                                           Coor, Lag_price, in_both, in_both_skader, Udbetaling)))
# Formula 
Formula <- as.formula(paste("sales_price ~", 
                            paste(c(pred_sar[1:22], "flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5"), collapse=" + ")))

# Model Estimation
Time <- Sys.time()
SAR_DF_t2 <- spatialreg::lagsarlm(formula = Formula, data = train_set_2_dataframe,
                                  listw = Neighbor_train2_weight, method = "LU", zero.policy = TRUE)
Stoptime <- Sys.time() - Time  # 1.346548 hours
# save(SAR_DF_t2, file ="/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/SAR_DF_t2.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/SAR_DF_t2.Rdata")
# Check data        
# Standard errors 
# See if heteroscedistic
plot(SAR_DF_t1$fitted.values, SAR_DF_t1$residuals)
#add a horizontal line at y=0 
abline(0,0)
# Does not look like heteroscedasticity
# Breuch pagan test 
bptest.Sarlm(SAR_DF_t1)
# But test says so


#Sign of heteroscedasticity 
Formula <- as.formula(paste("sales_price ~ Height + m2 + Outbuilding + 
            TerracedHouse + rooms + flooded + forest_distance + coastline_distance + railway_distance + 
            lake_distance + Trainstation_distance + Wateryarea_distance + Car_Garage + Built + 
            Renovated + Heating + Roof + BMaterial +
            powerline_distance + flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5")) 
Time <- Sys.time()
SAR_DF_t2_het <- sphet::spreg(formula = Formula, data = train_set_2_dataframe,
                              listw = Neighbor_train2_weight, model = "lag", het = TRUE)
Stoptime <- Sys.time() - Time
summary_sar_t2 <- summary(SAR_DF_t2_het)

### GWR ----
#### Train set 1 ----  
pred_GWR <- colnames(subset(Total_df, select = - c(rowname,
                                                   nominal_price, sales_price, addressID, enhed_id, 
                                                   Areas, Lag_price, flooded, SA_EV1, SA_EV2, SA_EV3, 
                                                   SA_EV4, SA_EV5, postnr, Dato, Hændelsesdato, Coor)))

Formula_GWR <- formula(paste("sales_price ~", 
                             paste(c(pred_GWR[1:19],"flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5"), collapse=" + ")))

coords <- sf::st_coordinates(train_set_1) # Retrive coordinates
train_set_1_data <- as(train_set_1_df, "data.frame")
train_set_1_df <- droplevels(train_set_1_df)
train_set_1_sp <- as(train_set_1, "Spatial")
train_set_1_sp$sales_price <- log(train_set_1_sp$sales_price)
train_set_1$sales_price <- log(train_set_1$sales_price)

# For spgwr package 
Starttime_BW_spgwr <- Sys.time()
# bw <- spgwr::gwr.sel(formula = Formula_GWR, data = train_set_1_df, coords = coords, 
# RMSE=TRUE, adapt = TRUE)
bw = 0.999933893038648
# Took 10.2 days to run.
Endtime_BW_spgwr <- Sys.time()-Starttime_BW_spgwr

# Use bandwidth to calculate gwr 
Startime.GWR <- Sys.time()
gwr.model <- spgwr::gwr(formula = Formula_GWR, coords = coords, data = train_set_1, bandwidth = bw)
Endtime_GWR <- Sys.time()-Startime.GWR

#### Train set 2 ----       

## XGBoost ---- 
## Train set + test  1 ----
### Train set ----
# The data frame is not suitable as data for xgbosting, remove attributes from df
train_set_1_xg <- train_set_1 # make a new data frame 
# Function to remove attributes
one_entry <- function(x) {
  for (i in length(x)) attr(x[[i]], "names") <- NULL
  return(x)
}
train_set_1_xg <- lapply(train_set_1_xg, FUN=one_entry) 
train_set_1_xg$Coor <- sf::st_as_text(train_set_1_xg$Coor)
TEST <- matrix(ncol = length(train_set_1_xg), nrow = length(train_set_1_xg[[1]])) #define matrix that contains value

# Print values into data frame without attributes
for (i in seq_along(train_set_1_xg)) {
  TEST[, i] <- train_set_1_xg[[i]]
  print(i)
}
DF <- TEST # rename df 
rm(TEST)
colnames(DF) <- names(train_set_1_xg)

# Delete character variables to change to numeric matrix 
train_set_1_xg <- DF
rm(DF)
train_set_1_xg <- subset(train_set_1_xg, select = -c(addressID, enhed_id, Coor))
train_set_1_xg <- as.data.frame(train_set_1_xg)

# Change to numeric
library(dplyr)
train_set_1_xg <- train_set_1_xg %>% mutate_if(is.character, as.numeric)

train_set_1_xg$Areas <- as.factor(train_set_1_xg$Areas)
train_set_1_xg$Built <- as.factor(train_set_1_xg$Built)
train_set_1_xg$Renovated <- as.factor(train_set_1_xg$Renovated)
train_set_1_xg$BMaterial <- as.factor(train_set_1_xg$BMaterial)
train_set_1_xg$Roof <- as.factor(train_set_1_xg$Roof)
train_set_1_xg$Heating <- as.factor(train_set_1_xg$Heating)

### Test ----
test_set_1_xg <- test_set_1
one_entry <- function(x) {
  for (i in length(x)) attr(x[[i]], "names") <- NULL
  return(x)
}
test_set_1_xg <- lapply(test_set_1_xg, FUN=one_entry)
test_set_1_xg$Coor <- sf::st_as_text(test_set_1_xg$Coor)
TEST <- matrix(ncol = length(test_set_1_xg), nrow = length(test_set_1_xg[[1]]))

for (i in seq_along(test_set_1_xg)) {
  TEST[, i] <- test_set_1_xg[[i]]
  print(i)
}
DF <- TEST
rm(TEST)
colnames(DF) <- names(test_set_1_xg)

# Delete character variables to change to numeric matrix 
test_set_1_xg <- DF
rm(DF)
test_set_1_xg <- subset(test_set_1_xg, select = -c(addressID, enhed_id, Coor))
test_set_1_xg <- as.data.frame(test_set_1_xg)

# Change to numeric
library(dplyr)
test_set_1_xg <- test_set_1_xg %>% mutate_if(is.character, as.numeric)

test_set_1_xg$Areas <- as.factor(test_set_1_xg$Areas)
test_set_1_xg$Built <- as.factor(test_set_1_xg$Built)
test_set_1_xg$Renovated <- as.factor(test_set_1_xg$Renovated)
test_set_1_xg$BMaterial <- as.factor(test_set_1_xg$BMaterial)
test_set_1_xg$Roof <- as.factor(test_set_1_xg$Roof)
test_set_1_xg$Heating <- as.factor(test_set_1_xg$Heating)


### Run model ----                
# Define predictors 
PredictorVariables_Areas <- colnames(subset(train_set_1_xg, select = - c(nominal_price, sales_price, rowname, postnr, 
                                                                         Hændelsesdato, Dato, Lag_price, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5, Udbetaling, in_both, in_both_skader)))
PredictorVariables_Lag_price <- colnames(subset(train_set_1_xg, select = - c(nominal_price, sales_price, rowname, postnr, 
                                                                             Hændelsesdato, Dato, Areas, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5,Udbetaling, in_both, in_both_skader)))


train_x = data.matrix(train_set_1_xg[, PredictorVariables_Areas]) # for one with areas
train_x = data.matrix(train_set_1_xg[, PredictorVariables_Lag_price]) # for one with lag price
train_y = train_set_1_xg[,"sales_price"]

# For test set 
test_x = data.matrix(test_set_1_xg[, PredictorVariables_Areas])
test_x = data.matrix(test_set_1_xg[, PredictorVariables_Lag_price])
test_y = test_set_1_xg[,"sales_price"]

# Define training and test sets 
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

# Watchlist that evaluates the performance 
watchlist = list(train=xgb_train, test=xgb_test)

# Fit model 
XGB_AREA_ZEALAND <-  xgb.train(data = xgb_train, max.depth = 25, watchlist=watchlist, nrounds = 250) # 59902.649660
XGB_LAGPRRICE_ZEALAND <- xgb.train(data = xgb_train, max.depth = 25, watchlist=watchlist, nrounds = 250) # 59902.649660
save(XGB_AREA_ZEALAND, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/XGB_AREA_ZEALAND.RData")
load(file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/XGB_AREA_ZEALAND.RData")
save(XGB_LAGPRRICE_ZEALAND, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/XGB_LAGPRRICE_ZEALAND.RData")
load(file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/XGB_LAGPRRICE_ZEALAND.RData")

library(xgboost)
pred_xg_area <- predict(XGB_AREA_ZEALAND, test_x)
mae_xg_area <- sum(abs(pred_xg_area-test_set_1_xg$sales_price))/length(pred_xg_area) # 0.6117743
pred_xg_lag <- predict(XGB_LAGPRRICE_ZEALAND, test_x)
mae_xg_lag <- sum(abs(pred_xg_lag-test_set_1_xg$sales_price))/length(pred_xg_lag) #0.6173684

# Test on errors
XG_ERROR_Moran <- moran.test(pred_xg_area-test_set_1_xg$sales_price, listw=spdep::nb2listw(Neighbor_test1))
XG_ERROR_Moran <- moran.test(pred_xg_lag-test_set_1_xg$sales_price, listw=spdep::nb2listw(Neighbor_test1))

# Moran on train set 
pred_xg_lag <- predict(XGB_LAGPRRICE_ZEALAND, train_x)
XG_error_train_moran <- moran.test((pred_xg_lag-train_y), listw=Neighbor_train1_weight)

pred_xg_area <- predict(XGB_AREA_ZEALAND, train_x)
XG_error_train_moran <- moran.test((pred_xg_area-train_y), listw=Neighbor_train1_weight)
pred_xg_area_nom <- exp(pred_xg_area)
rmse_nom_XGB <- sqrt(sum((pred_xg_area_nom-exp(train_set_1_xg$sales_price))^2)/length(pred_xg_area_nom))


# Importance matrix 
importance_matrix <- xgb.importance(
  feature_names = colnames(xgb_train), 
  model = XGB_LAGPRRICE_ZEALAND
)
importance_matrix
xgb.plot.importance(importance_matrix)
library(Ckmeans.1d.dp)
(gg <- xgb.ggplot.importance(importance_matrix, measure = "Importance", rel_to_first = TRUE))
gg + ggplot2::ylab("Importance")




## Train set 2 ----
# The data frame is not suitable as data for xgbosting, remove attributes from df
train_set_2_xg <- train_set_2 # make a new data frame 
# Function to remove attributes
one_entry <- function(x) {
  for (i in length(x)) attr(x[[i]], "names") <- NULL
  return(x)
}
train_set_2_xg <- lapply(train_set_2_xg, FUN=one_entry) 
train_set_2_xg$Coor <- sf::st_as_text(train_set_2_xg$Coor)
TEST <- matrix(ncol = length(train_set_2_xg), nrow = length(train_set_2_xg[[1]])) #define matrix that contains value

# Print values into data frame without attributes
for (i in seq_along(train_set_2_xg)) {
  TEST[, i] <- train_set_2_xg[[i]]
  print(i)
}
DF <- TEST # rename df 
rm(TEST)
colnames(DF) <- names(train_set_2_xg)

# Delete character variables to change to numeric matrix 
train_set_2_xg <- DF
rm(DF)
train_set_2_xg <- subset(train_set_2_xg, select = -c(addressID, enhed_id, Coor))
train_set_2_xg <- as.data.frame(train_set_2_xg)

# Change to numeric
library(dplyr)
train_set_2_xg <- train_set_2_xg %>% mutate_if(is.character, as.numeric)

train_set_2_xg$Areas <- as.factor(train_set_2_xg$Areas)
train_set_2_xg$Built <- as.factor(train_set_2_xg$Built)
train_set_2_xg$Renovated <- as.factor(train_set_2_xg$Renovated)
train_set_2_xg$BMaterial <- as.factor(train_set_2_xg$BMaterial)
train_set_2_xg$Roof <- as.factor(train_set_2_xg$Roof)
train_set_2_xg$Heating <- as.factor(train_set_2_xg$Heating)

### Test ----
test_set_2_xg <- test_set_2
one_entry <- function(x) {
  for (i in length(x)) attr(x[[i]], "names") <- NULL
  return(x)
}
test_set_2_xg <- lapply(test_set_2_xg, FUN=one_entry)
test_set_2_xg$Coor <- sf::st_as_text(test_set_2_xg$Coor)
TEST <- matrix(ncol = length(test_set_2_xg), nrow = length(test_set_2_xg[[1]]))

for (i in seq_along(test_set_2_xg)) {
  TEST[, i] <- test_set_2_xg[[i]]
  print(i)
}
DF <- TEST
rm(TEST)
colnames(DF) <- names(test_set_2_xg)

# Delete character variables to change to numeric matrix 
test_set_2_xg <- DF
rm(DF)
test_set_2_xg <- subset(test_set_2_xg, select = -c(addressID, enhed_id, Coor))
test_set_2_xg <- as.data.frame(test_set_2_xg)

# Change to numeric
library(dplyr)
test_set_2_xg <- test_set_2_xg %>% mutate_if(is.character, as.numeric)

test_set_2_xg$Areas <- as.factor(test_set_2_xg$Areas)
test_set_2_xg$Built <- as.factor(test_set_2_xg$Built)
test_set_2_xg$Renovated <- as.factor(test_set_2_xg$Renovated)
test_set_2_xg$BMaterial <- as.factor(test_set_2_xg$BMaterial)
test_set_2_xg$Roof <- as.factor(test_set_2_xg$Roof)
test_set_2_xg$Heating <- as.factor(test_set_2_xg$Heating)


### Run model ----                
# Define predictors 
PredictorVariables_Areas <- colnames(subset(train_set_2_xg, select = - c(nominal_price, sales_price, rowname, postnr, 
                                                                         Hændelsesdato, Dato, Lag_price, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5)))
PredictorVariables_Lag_price <- colnames(subset(train_set_2_xg, select = - c(nominal_price, sales_price, rowname, postnr, 
                                                                             Hændelsesdato, Dato, Areas, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5)))

train_x = data.matrix(train_set_2_xg[, PredictorVariables_Areas]) # for one with areas
train_x = data.matrix(train_set_2_xg[, PredictorVariables_Lag_price]) # for one with lag price
train_y = train_set_2_xg[,"sales_price"]

# For test set 
test_x = data.matrix(test_set_2_xg[, PredictorVariables_Areas])
test_x = data.matrix(test_set_2_xg[, PredictorVariables_Lag_price])
test_y = test_set_2_xg[,"sales_price"]

# Define training and test sets 
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

# Watchlist that evaluates the performance 
watchlist = list(train=xgb_train, test=xgb_test)

# Fit model 
XGB_AREA_ZEALAND_t2 <-  xgb.train(data = xgb_train, max.depth = 25, watchlist=watchlist, nrounds = 250) # 59902.649660
XGB_LAGPRRICE_ZEALAND_t2 <- xgb.train(data = xgb_train, max.depth = 25, watchlist=watchlist, nrounds = 250) # 59902.649660
save(XGB_AREA_ZEALAND, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/XGB_AREA_ZEALAND_t2.RData")
save(XGB_LAGPRRICE_ZEALAND_t2, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/XGB_LAGPRRICE_ZEALAND_t2.RData")


# Importance matrix 
importance_matrix <- xgb.importance(
  feature_names = colnames(xgb_train), 
  model = XGB_LAGPRRICE_ZEALAND_t2
)
importance_matrix
xgb.plot.importance(importance_matrix)
library(Ckmeans.1d.dp)
(gg <- xgb.ggplot.importance(importance_matrix, measure = "Importance", rel_to_first = TRUE))
gg + ggplot2::ylab("Importance")
