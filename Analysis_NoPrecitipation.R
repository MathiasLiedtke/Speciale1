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

------------------------------------------------------------------------
  
  # Load in data ----

load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_18_v2.Rdata")

Total_df <-  Total_df_18_v2
Total_df <- subset(Total_df, select = - `Tidligere udbetalt byg/løs/afgrd`)
Total_df$sales_price <- log(Total_df$sales_price)
Total_df <- subset(Total_df, in_both == FALSE)
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
train_set_1_NP <- Total_df[train_seq_1, , drop = FALSE]
## Test 1
test_set_1_NP <- Total_df[-train_seq_1, , drop = FALSE]

p <- 0.4    
iT <- p*nrow(Total_df)
## Train 2
set.seed(200)
train_seq_2 <- sample(nrow(Total_df), size = iT, replace = FALSE)
train_set_2_NP <- Total_df[train_seq_2, , drop = FALSE]
## Test 2
test_set_2_NP <- Total_df[-train_seq_2, , drop = FALSE]

------------------------------------------------------------------------
  
  # Preliminary analysis ----
## Test for spatial autocorrelation ----
### Train Set 1  ----
T1 <- Sys.time()
train_set_1_NP <- sf::st_as_sf(train_set_1_NP)
train_set_1_NP <- as(train_set_1_NP, "Spatial")
points_train_1_NP <- sp::coordinates(train_set_1_NP)
# Neighbor_train1_NP <- spdep::tri2nb(points_train_1_NP)  #When calculate neighbor
T2 <- Sys.time() - T1 # 20 min
# save(Neighbor_train1_NP, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_train1_NP.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_train1_NP.Rdata")

### Train Set 2 ----
T3 <- Sys.time()
train_set_2_NP <- sf::st_as_sf(train_set_2_NP)
train_set_2_NP <- as(train_set_2_NP, "Spatial")
points_train_2_NP <- sp::coordinates(train_set_2_NP)
# Neighbor_train2_NP <- spdep::tri2nb(points_train_2_NP)  #When calculate neighbor
T4 <- Sys.time() - T1 # 20 min
# save(Neighbor_train2_NP, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_train2_NP.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_train2_NP.Rdata")


### Test Set 1  ----
T5 <- Sys.time()
test_set_1_NP <- sf::st_as_sf(test_set_1_NP)
test_set_1_NP <- as(test_set_1_NP, "Spatial")
points_test_1_NP <- sp::coordinates(test_set_1_NP)
# Neighbor_test1_NP <- spdep::tri2nb(points_test_1_NP)  #When calculate neighbor
T6 <- Sys.time() - T1 # 20 min
# save(Neighbor_test1_NP, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_test1_NP.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_test1_NP.Rdata")

### Test Set 2 ----
T7 <- Sys.time()
test_set_2_NP <- sf::st_as_sf(test_set_2_NP)
test_set_2_NP <- as(test_set_2_NP, "Spatial")
points_test_2_NP <- sp::coordinates(test_set_2_NP)
# Neighbor_test2_NP <- spdep::tri2nb(points_test_2_NP)  #When calculate neighbor
T8 <- Sys.time() - T1 # 20 min
# save(Neighbor_test2_NP, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_test2_NP.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_test2_NP.Rdata")

### Moran and geary test ----
#### Neighbor_train1 ----
# Test for normality
data <- train_set_1_NP$sales_price
qqnorm(data, main = 'Q-Q Plot for Normality', xlab = 'Theoretical Dist',
       ylab = 'Sample dist', col = 'steelblue')
qqline(data, col = 'red', lwd = 2, lty = 2)

# Seems fairly normal in distribution, continue to tests 
Neighbor_train1_weight_NP <- spdep::nb2listw(Neighbor_train1_NP) # Define nb2listw object 
Moran <- spdep::moran.test(train_set_1_NP$sales_price, listw = Neighbor_train1_weight_NP)
Geary <- spdep::geary.test(train_set_1_NP$sales_price, Neighbor_train1_weight_NP)    
# Sign of auto-correlation 
# Test for normality
#### Neighbor_train2 ----
data <- train_set_2$sales_price
qqnorm(data, main = 'Q-Q Plot for Normality', xlab = 'Theoretical Dist',
       ylab = 'Sample dist', col = 'steelblue')
qqline(data, col = 'red', lwd = 2, lty = 2)

# Seems fairly normal in distribution, continue to tests 
Neighbor_train2_weight_NP <- spdep::nb2listw(Neighbor_train2_NP) # Define nb2listw object 
Moran <- spdep::moran.test(train_set_1$sales_price, listw = Neighbor_train2_weight)
Geary <- spdep::geary.test(train_set_1$sales_price, Neighbor_train2_weight)    
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

lm_areas_NP_t1 <- stats::lm(formula = Formula, train_set_1_NP) # Estimate model
summary <- summary(lm_areas_NP_t1)[["coefficients"]]
# See if heteroscedistic
#create residual vs. fitted plot
plot(fitted(fit), resid(fit))
#add a horizontal line at y=0 
abline(0,0)
# Breuch pagan test 
bptest(lm_areas_NP_t1)
# But test says so
summary_lm_t1_robust <- lmtest::coeftest(lm_areas_NP_t1, 
                                         vcov = vcovHC(lm_areas_NP_t1, type = 'HC0'))

#### Test set 1 ----          
test_set_1_NP$yhat <- stats::predict.lm(lm_areas_NP_t1, newdata = test_set_1_NP) 
RMSE_LM <- sqrt(sum((test_set_1_NP$yhat-test_set_1_NP$sales_price)^2)/nrow(test_set_1_NP)) # 0.722349
            test_set_1_NP$yhat_price <- exp(test_set_1_NP$yhat)
            test_set_1_NP$sales_price_nom <- exp(test_set_1_NP$sales_price)
            RMSE_LM_Price <- sqrt(sum((test_set_1_NP$yhat_price-test_set_1_NP$sales_price_nom)^2)/nrow(test_set_1_NP))
MSE_LM <- sum((test_set_1_NP$yhat-test_set_1_NP$sales_price)^2)/nrow(test_set_1_NP) # 0.521788
MAE_LM <- sum(abs(test_set_1_NP$yhat-test_set_1_NP$sales_price))/nrow(test_set_1_NP) # 0.5510442

# train RMSE 
LM_RMSE_Train <- sqrt(sum((lm_areas_NP_t1$residuals)^2)/length(lm_areas_NP_t1$residuals))

#### Train set 2 ----
pred_lm <- colnames(subset(Total_df, 
                           select = - c(rowname, nominal_price, sales_price, 
                                        addressID, enhed_id, flooded, SA_EV1, SA_EV2, 
                                        SA_EV3, SA_EV4, SA_EV5, postnr, Dato, Hændelsesdato, 
                                        Coor, Lag_price, in_both, in_both_skader, Udbetaling))) #Variables to not include as predictors

Formula <- as.formula(paste("sales_price ~", 
                            paste(c(pred_lm[1:19], "flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5"), collapse=" + ")))

lm_areas_NP_t2 <- stats::lm(formula = Formula, train_set_2_NP) # Estimate model
summary <- summary(lm_areas_NP_t2)[["coefficients"]]
# See if heteroscedistic
#create residual vs. fitted plot
plot(fitted(fit), resid(fit))
#add a horizontal line at y=0 
abline(0,0)
# Breuch pagan test 
bptest(lm_areas_NP_t2)
# But test says so
summary_lm_t2_robust <- lmtest::coeftest(lm_areas_NP_t2, 
                                         vcov = vcovHC(lm_areas_NP_t2, type = 'HC0'))


### SAR ----
#### Train set 1 ----  
# I think the train set should be changed to a regular data frame. 
train_set_1_dataframe <- sf::st_drop_geometry(train_set_1_NP) 

# Predictors
pred_sar <- colnames(subset(Total_df, select = - c(rowname, 
                                                   nominal_price, sales_price, addressID, enhed_id, 
                                                   Lag_price, Areas, SA_EV1, SA_EV2, SA_EV3, SA_EV4, 
                                                   SA_EV5, postnr, Dato, Hændelsesdato, Coor, 
                                                   Udbetaling, in_both, in_both_skader)))

# Formula 
Formula <- as.formula(paste("sales_price ~", 
                            paste(c(pred_sar[1:22], "flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5"), collapse=" + ")))

# Model Estimation
Time <- Sys.time()
SAR_DF_t1_NP <- spatialreg::lagsarlm(formula = Formula, data = train_set_1_dataframe,
                                          listw = Neighbor_train1_weight_NP, method = "LU", zero.policy = TRUE)
Stoptime <- Sys.time() - Time  # 1.346548 hours
save(SAR_DF_t1_NP, file ="/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/SAR_DF_t1_NP.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/SAR_DF_t1_NP.Rdata")
SAR_DF_t1_NP <- SAR_DF_t2_NP

# Check data        
# Standard errors 
# See if heteroscedistic
plot(SAR_DF_t1$fitted.values, SAR_DF_t1$residuals)
#add a horizontal line at y=0 
abline(0,0)
# Does not look like heteroscedasticity
# Breuch pagan test 
bptest.Sarlm(SAR_DF_t1_NP)
# But test says so
Formula <- as.formula(paste("sales_price ~ Height + m2 + Outbuilding + 
            TerracedHouse + rooms + flooded + forest_distance + coastline_distance + railway_distance + 
            lake_distance + Trainstation_distance + Wateryarea_distance + Car_Garage + Built + 
            Renovated + Heating + Roof + BMaterial +
            powerline_distance + flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5")) 
Time <- Sys.time()
SAR_DF_t1_het <- sphet::spreg(formula = Formula, data = train_set_1_dataframe,
                              listw = Neighbor_train1_weight_NP, model = "lag", het = TRUE)
Stoptime <- Sys.time() - Time
summary_sar_t1 <- summary(SAR_DF_t1_het)

#### Test set 1 ----
# A stupid way, but cannot think of something else. Manually doing the prediction
test_SAR <- Total_df[-train_seq_1, , drop = FALSE]
test_SAR <- test_SAR %>%
  rowwise() %>%
  mutate(yhat = 4.7144e+00 +
           4.1892e-04 * Height +
           3.1007e-03 * m2 +
           -1.3442e-03 * Outbuilding +
           1.9202e-01 * TerracedHouse +
           1.9125e-02 * rooms +
           1.9601e-02 * flooded +
           6.4626e-05 * forest_distance -
           2.7848e-06 * coastline_distance +
           9.4682e-06 * railway_distance +
           4.0552e-05 * lake_distance -
           1.1153e-05 * Trainstation_distance -
           5.8325e-06 * Wateryarea_distance -
           3.5536e-02 * Car_Garage + 
           ifelse(Built == 2, 2.0988e-02,
                  ifelse(Built == 3, 1.0362e-01,
                         ifelse(Built == 4, 1.2930e-01,
                                ifelse(Built == 5, 1.6116e-01,
                                       ifelse(Built == 6, 2.3342e-01,
                                              ifelse(Built == 7, 7.2120e-02,
                                                     ifelse(Built == 8, 2.1944e-01,
                                                            ifelse(Built == 9, -1.2377e-01, 0)))))))) +
           ifelse(Renovated == 1, 7.0179e-02,
                  ifelse(Renovated == 2, 3.2310e-02,
                         ifelse(Renovated == 3, 3.5389e-02,
                                ifelse(Renovated == 4, 1.4136e-02, 4.3338e-02)))) +
           ifelse(Heating == 1, 9.9837e-02,
                  ifelse(Heating == 2, 1.0090e-01, 2.7547e-02)) +
           ifelse(Roof == 1, 8.2214e-022,
                  ifelse(Roof == 2, 8.7069e-02, -5.0242e-02)) +
           ifelse(BMaterial == 1, 6.7854e-04,
                  ifelse(BMaterial == 2, 3.8268e-03, -3.3659e-02)) +
           powerline_distance * 1.8492e-05 +
           SA_EV1 * 4.2788e-01 +
           SA_EV2 * 5.0318e-01 +
           SA_EV3 * 5.1838e-01 +
           SA_EV4 * 5.9056e-01 +
           SA_EV5 * 7.1646e-01 +
           flooded * (SA_EV1 * 1.2618e-02 +
                        SA_EV2 * -5.8867e-02 +
                        SA_EV3 * 1.7895e-03 +
                        SA_EV4 * 1.8473e-02 +
                        SA_EV5 * -1.0360e-02) +
           5.8067e-01 * log(Lag_price))

test_SAR$yhat_price <- exp(test_SAR$yhat)
test_SAR$sales_price_nominal <- exp(test_SAR$sales_price)
# RMSE 
RMSE_SAR <- sqrt(sum((test_SAR$yhat-test_SAR$sales_price)^2)/nrow(test_SAR)) # 0.9054654
RMSE_SAR_nominal <- sqrt(sum((test_SAR$yhat_price-test_SAR$sales_price_nominal)^2)/nrow(test_SAR)) # 0.9054654
MSE_SAR <- sum((test_SAR$yhat-test_SAR$sales_price)^2)/nrow(test_SAR) #0.8198677
MAE_SAR <- sum(abs(test_SAR$yhat-test_SAR$sales_price))/nrow(test_SAR) #0.6456845

# Train rmse
SAR_RMSE_Train <- sqrt(sum((SAR_DF_t1_het$residuals)^2)/length(SAR_DF_t1_het$residuals))

#### Train set 2 ----
train_set_2_dataframe <- sf::st_drop_geometry(train_set_2_NP) 

# Predictors
pred_sar <- colnames(subset(Total_df, select = - c(rowname, 
                 nominal_price, sales_price, addressID, enhed_id, 
                 Lag_price, Areas, SA_EV1, SA_EV2, SA_EV3, SA_EV4, 
                 SA_EV5, postnr, Dato, Hændelsesdato, Coor, 
                 Udbetaling, in_both, in_both_skader)))


# Formula 
Formula <- as.formula(paste("sales_price ~", 
                            paste(c(pred_sar[1:22], "flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5"), collapse=" + ")))

# Model Estimation
Time <- Sys.time()
SAR_DF_t2_NP <- spatialreg::lagsarlm(formula = Formula, data = train_set_2_dataframe,
                                     listw = Neighbor_train2_weight, method = "LU", zero.policy = TRUE)
Stoptime <- Sys.time() - Time  # 1.346548 hours
save(SAR_DF_t2_NP, file ="/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/SAR_DF_t2_NP.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/SAR_DF_t2_NP.Rdata")

Time <- Sys.time()
SAR_DF_t2_het <- sphet::spreg(formula = Formula, data = train_set_2_dataframe,
                              listw = Neighbor_train2_weight_NP, model = "lag", het = TRUE)
Stoptime <- Sys.time() - Time
summary_sar_t2 <- summary(SAR_DF_t2_het)


## XGBoost ---- 
## Train set + test  1 ----
### Train set ----
# The data frame is not suitable as data for xgbosting, remove attributes from df
train_set_1_xg <- train_set_1_NP # make a new data frame 
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
test_set_1_xg <- test_set_1_NP
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
                                                                         Hændelsesdato, Dato, Lag_price, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5, in_both, in_both_skader)))
PredictorVariables_Lag_price <- colnames(subset(train_set_1_xg, select = - c(nominal_price, sales_price, rowname, postnr, 
                                                                             Hændelsesdato, Dato, Areas, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5,in_both, in_both_skader)))

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
XGB_AREA <-  xgb.train(data = xgb_train, max.depth = 25, watchlist=watchlist, nrounds = 250) #
XGB_LAG <-  xgb.train(data = xgb_train, max.depth = 25, watchlist=watchlist, nrounds = 250) #


save(XGB_AREA, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/XGB_AREA_NP.RData")
load(file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/XGB_AREA_NP.RData")
save(XGB_LAG, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/XGB_LAG_NP.RData")
load(file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/XGB_LAG_NP.RData")

library(xgboost)
pred_xg_area <- predict(XGB_AREA, test_x)
mae_xg_area <- sum(abs(pred_xg_area-test_set_1_xg$sales_price))/length(pred_xg_area) #0.6142142
pred_xg_lag <- predict(XGB_LAG, test_x)
mae_xg_lag <- sum(abs(pred_xg_lag-test_set_1_xg$sales_price))/length(pred_xg_lag) #0.6212424

# Test on errors
XG_ERROR_Moran <- moran.test(pred_xg_area-test_set_1_xg$sales_price, listw=spdep::nb2listw(Neighbor_test1))
XG_ERROR_Moran <- moran.test(pred_xg_lag-test_set_1_xg$sales_price, listw=spdep::nb2listw(Neighbor_test1))

# Importance matrix 
importance_matrix <- xgb.importance(
  feature_names = colnames(xgb_train), 
  model = XGB_LAG
)
importance_matrix
xgb.plot.importance(importance_matrix)
library(Ckmeans.1d.dp)
(gg <- xgb.ggplot.importance(importance_matrix, measure = "Importance", rel_to_first = TRUE))
gg + ggplot2::ylab("Importance")

## Train set 2 ----    
train_set_2_xg <- train_set_2_NP # make a new data frame 
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
test_set_2_xg <- test_set_2_NP
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
                                                                         Hændelsesdato, Dato, Lag_price, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5, in_both, in_both_skader)))
PredictorVariables_Lag_price <- colnames(subset(train_set_2_xg, select = - c(nominal_price, sales_price, rowname, postnr, 
                                                                             Hændelsesdato, Dato, Areas, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5,in_both, in_both_skader)))

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
XGB_AREA_t2_NP <-  xgb.train(data = xgb_train, max.depth = 25, watchlist=watchlist, nrounds = 250) #
XGB_LAG_t2_NP <-  xgb.train(data = xgb_train, max.depth = 25, watchlist=watchlist, nrounds = 250) #


save(XGB_AREA_t2_NP, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/XGB_AREA_NP_t2_NP.RData")
save(XGB_LAG_t2_NP, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/XGB_LAG_NP_t2_NP.RData")

library(xgboost)
pred_xg_area <- predict(XGB_AREA, test_x)
mae_xg_area <- sum(abs(pred_xg_area-test_set_1_xg$sales_price))/length(pred_xg_area) #0.6142142
pred_xg_lag <- predict(XGB_LAG, test_x)
mae_xg_lag <- sum(abs(pred_xg_lag-test_set_1_xg$sales_price))/length(pred_xg_lag) #0.6212424

# Test on errors
XG_ERROR_Moran <- moran.test(pred_xg_area-test_set_1_xg$sales_price, listw=spdep::nb2listw(Neighbor_test1))
XG_ERROR_Moran <- moran.test(pred_xg_lag-test_set_1_xg$sales_price, listw=spdep::nb2listw(Neighbor_test1))

# Importance matrix 
importance_matrix <- xgb.importance(
  feature_names = colnames(xgb_train), 
  model = XGB_LAG_t2_NP
)
importance_matrix
xgb.plot.importance(importance_matrix)
library(Ckmeans.1d.dp)
(gg <- xgb.ggplot.importance(importance_matrix, measure = "Importance", rel_to_first = TRUE))
gg + ggplot2::ylab("Importance")