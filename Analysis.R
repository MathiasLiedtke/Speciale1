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

load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_18_v2.Rdata")

Total_df <-  Total_df_18_v2
Total_df <- subset(Total_df, select = - `Tidligere udbetalt byg/løs/afgrd`)
Total_df$sales_price <- log(Total_df$sales_price)
rm(Total_df_18_v2)



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

## Train 2
set.seed(200)
train_seq_2 <- sample(nrow(Total_df), size = iT, replace = FALSE)
train_set_2 <- Total_df[train_seq_2, , drop = FALSE]
## Test 2
test_set_2 <- Total_df[-train_seq_2, , drop = FALSE]


# Preliminary analysis ----
## Test for spatial autocorrelation ----
### First part ----
# Train 1
T1 <- Sys.time()
# train_set_1_df <- train_set_1 %>% Done previously in data_preparation
#   dplyr::distinct(geometry, .keep_all = TRUE) Done previously in data_preparation
# train_set_1 <- sf::st_as_sf(train_set_1)
# train_set_1 <- as(train_set_1, "Spatial")
# points_train_1 <- sp::coordinates(train_set_1)
# Neighbor_train <- spdep::tri2nb(points_train_1)  #When calculate neighbor
# T2 <- Sys.time() - T1 # 20 min
# # save(Neighbor_train, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_train.Rdata")
load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_train.Rdata")

# Test 1
T3 <- Sys.time()
# test_set_1_df <- test_set_1 %>% 
#   distinct(geometry, .keep_all = TRUE) # Necessary to run neighbor
test_set_1_df <- sf::st_as_sf(test_set_1_df)  # Necessary to run neighbor
test_set_1_df <- as(test_set_1_df, "Spatial")  # Necessary to run neighbor
points_test_1 <- sp::coordinates(test_set_1_df) # Necessary to run neighbor
Neighbor_test <- spdep::tri2nb(points_test_1)
T4 <- Sys.time() - T3 #
save(Neighbor_test, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_test.Rdata")
# load("/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_test.Rdata")

# Spatial weights object 
summary(Neighbor_train) # see how many neighbors per observation.


# Plot for visualization
neighborlinks <- as(train_set_1, "data.frame")
neighborlinks <- subset(neighborlinks, select = c(rowname, coords.x1, coords.x2))
# W = 6224289.319621, N = 573166.029488, E = 6223737.170789, S = 572817.747073
neighborlinks <- subset(neighborlinks[1:35,])
neighbor <- spdep::tri2nb(neighborlinks[,2:3])
plot(neighborlinks[,2:3])
plot(neighbor, neighborlinks[,2:3], add=TRUE, pch=".")



Neighbor_train_weight <- spdep::nb2listw(Neighbor_train)
Neighbor_train_weight # summary of weights
# Global Moran 
# Test for normality 
data <- train_set_1$sales_price
qqnorm(data, main = 'Q-Q Plot for Normality', xlab = 'Theoretical Dist',
       ylab = 'Sample dist', col = 'steelblue')
qqline(data, col = 'red', lwd = 2, lty = 2)

Moran <- spdep::moran.test(train_set_1$sales_price, listw = Neighbor_train_weight)
Geary <- spdep::geary.test(train_set_1$sales_price, Neighbor_train_weight)

lmtest::bptest(train_set_1_df)

# Spatial autocorrelation in model, correct with heteroscedasticity. Applied spatial data analysis with R, P. 275- 
PredictorVariables <- colnames(subset(Total_df, select = - c(nominal_price, addressID, enhed_id, postnr, 
                                                             geometry, Areas, lag_price)))
Formula <- formula(paste("nominal_price ~", 
                         paste(PredictorVariables[1:48], collapse=" + ")))


lm_SPAUTO <- stats::lm(formula = Formula, Total_df) # does not fit of observations in lmmorantest
lm_SPAUTO <- stats::lm(formula = Formula, train_set_1_df)

summary(lm_SPAUTO)
# save residuals 
train_set_1$lmresid <- residuals(lm_SPAUTO)

# Moran test on residuals
lm.morantest(lm_SPAUTO, Neighbor_train_weight) # p-value < 2.2e-16
# Observed Moran I      Expectation         Variance 
# 3.494663e-01    -2.705789e-05     6.370905e-07
# We see sign of spatial autocorrelation in error terms 
# Move to SAR model for rest of SAR regression


# Remember to merge on before distinct coordinates
train_set_1_test <- train_set_1
train_set_1_test$neighbor
# Problem will occur with nb class later of weights. We keep it with distinct, 
# but maybe select 'more important' observations



# Analysis ----

## Regression ----

### Linear model with zip codes ----
# train_set_1 
PredictorVariables <- colnames(subset(Total_df, 
                                      select = - c(rowname, nominal_price, sales_price, addressID, enhed_id, 
                                                   flooded, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5, postnr, Dato, Hændelsesdato, Coor, Lag_price)))
Formula <- as.formula(paste("sales_price ~", 
                            paste(c(PredictorVariables[1:20], "flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5"), collapse=" + ")))
lm_areas <- stats::lm(formula = Formula, train_set_1)
summary <- summary(lm_areas)[["coefficients"]]

# Input the coefficients and t value + Probability significance into table 
Area_coef <- as.data.frame(summary[16:91,])
summary <- summary[-(16:91),]
summary <- summary[-(39:43),]

Predictor_rows <- c("Intercept", "Height", "# m2", "Outbuilding", "Terraced house", "# Rooms", "Forest Distance", 
                    "Coastline Distance", "Powerline Distance", "Railway Distance", "Lake Distance", "Trainstation Distance", 
                    "Wateryarea distance", "Udbetaling", "Car/Garage", "Built 1940-1950", "Built 1950-1960", "Built 1960-1970",
                    "Built 1970-1980", "Built 1980-1990", "Built 1990-2000", "Built 2000-2010", "Built after 2010",
                    "Renovated 1940-1950", "Renovated 1950-1960", "Renovated 1960-1970", "Renovated 1970-1980", "Renovated 1980-1990", 
                    "District Heating", "Central Heating", "Electric Heating", "Tile", "Thatch", "Fibercement", "Brick", "Wood", 
                    "Concrete", "Flooded", "f·SA_EV1", "f·SA_EV2", "f·SA_EV3", "f·SA_EV4", "f·SA_EV5")

rownames(summary) <- Predictor_rows
summary <- as.data.frame(summary)
summary$Predictor <- rownames(summary)
summary <- summary[,-2]
summary <- summary[, c(4, 1:3)] 

#test spatial auto correlation again 
moran_lm_area <- spdep::moran.test(lm_areas$fitted.values, listw = Neighbor_train_weight)

# Is this treated completely right? Do i need to test something og adjust standard errors. 

### GWR model ----
# train_set_1
# Define formula 
# Define formula 
PredictorVariables_GWR <- colnames(subset(Total_df, 
                                          select = - c(rowname, nominal_price, sales_price, addressID, enhed_id, Areas, Lag_price,
                                                       flooded, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5, postnr, Dato, Hændelsesdato, Coor)))
Formula_GWR <- formula(paste("sales_price ~", 
                             paste(c(PredictorVariables_GWR[1:19],"flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5"), collapse=" + ")))
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
bw = 5.9999874
# Took 10.2 days to run.
Endtime_BW_spgwr <- Sys.time()-Starttime_BW_spgwr

# Use bandwidth to calculate gwr 
Startime.GWR <- Sys.time()
gwr.model <- spgwr::gwr(formula = Formula_GWR, coords = coords, data = train_set_1, bandwidth = bw)
Endtime_GWR <- Sys.time()-Startime.GWR
# TEst with formula
gwr.model <- spgwr::gwr(formula = Formula_GWR, data = train_set_1_df, bandwidth = bw)

Formula <- "sales_price ~ Built"

Startime.GWR <- Sys.time()
gwr.model <- GWmodel::gwr.basic(formula = Formula, 
                                data = train_set_1_sp, bw = bw, adaptive = TRUE, longlat = TRUE)
Endtime_GWR <- Sys.time()-Startime.GWR

gw_reg_all

train_set_1_dataframe <- train_set_1 %>%
  distinct(geometry, .keep_all = TRUE)
#write to qgis
coords <- sf::st_coordinates(train_set_1_dataframe)
train_set_1_dataframe <- cbind(train_set_1_dataframe, coords)
sf::st_write(train_set_1_dataframe, append = FALSE , "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/train_set_1.shp")


PredictorVariables_GWR <- colnames(subset(Total_df, 
                                          select = - c(nominal_price, addressID, enhed_id, postnr, geometry, Areas)))
Formula_GWR <- formula(paste("nominal_price ~", 
                             paste(PredictorVariables_GWR[1:20], collapse=" + ")))
Formula_GWR <- formula(nominal_price ~ m2)



save(bw, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df.Rdata")

# For GWmodel package 
Starttime_BW_GWmodel <- Sys.time()
bw_GWM <- GWmodel::bw.gwr(formula = Formula_GWR, data = train_set_1_df, 
                          adaptive = TRUE, longlat = TRUE)
Endtime_BW_GWmodel <- Sys.time()-Starttime_BW_GWmodel


Startime.GWR <- Sys.time()
gwr.model <- GWmodel::gwr(formula = Formula, data = train_set_1_df, adapt = bw)
Endtime_GWR <- Sys.time()-Startime.GWR


bw <- gwr.sel(formula = Formula_GWR, data = Total_df_GWR[1:round(nrow(Total_df_GWR)/2),], adapt=T, RMSE=T)

lm_areas <- stats::lm(formula = Formula, Total_df_df)


### SAR model ----
# train_set_1
# I think the train set should be changed to a regular data frame. 
train_set_1_dataframe <- sf::st_drop_geometry(train_set_1) 

# Predictors
PredictorVariables <- colnames(subset(Total_df, 
                      select = - c(rowname, nominal_price, sales_price, addressID, 
                                   enhed_id, Lag_price, Areas, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5, 
                                   postnr, Dato, Hændelsesdato, Coor)))

# Formula 
Formula <- as.formula(paste("sales_price ~", 
                            paste(c(PredictorVariables[1:20], "flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5"), collapse=" + ")))
train_set_1_dataframe$sales_price <- log(train_set_1_dataframe$sales_price)
train_set_1_dataframe$sales_price <- exp(train_set_1_dataframe$sales_price)

# Outbuilding, built

[1] "Height"              "m2"                  "Outbuilding"         "TerracedHouse"       "rooms"               "forest_distance"    
[7] "coastline_distance"  "powerline_distance"  "railway_distance"    "lake_distance"       "Wateryarea_distance" "Udbetaling"         
[13] "Car_Garage"          "Built"               "Renovated"           "Heating"             "Roof"                "BMaterial"          
[19] "Coor"  

# Model
Time <- Sys.time()
SAR_DF_log_tol_14 <- spatialreg::lagsarlm(formula = Formula, data = train_set_1_dataframe,
                                   listw = Neighbor_train_weight, method = "LU", zero.policy = TRUE, tol = 1e-14)
Stoptime <- Sys.time() - Time  # 1.346548 hours

# some missing Standard errors 
fd_hess <- SAR_DF_log[["fdHess"]]
var_hess <- solve(fd_hess)
se <- sqrt(diag(var_hess))




Formula <- as.formula(sales_price ~ flooded*SA_EV5)
SAR_DF_log_1 <- spatialreg::lagsarlm(formula = Formula, data = train_set_1_dataframe,
                                   listw = Neighbor_train_weight, method = "LU", zero.policy = TRUE)



## XGBoosting ----
# packages: xgboost and caret 
# Remove attributes from df

### Train ----
train_set_1_xg <- train_set_1
one_entry <- function(x) {
  for (i in length(x)) attr(x[[i]], "names") <- NULL
  return(x)
}
train_set_1_xg <- lapply(train_set_1_xg, FUN=one_entry)
train_set_1_xg$Coor <- sf::st_as_text(train_set_1_xg$Coor)
TEST <- matrix(ncol = length(train_set_1_xg), nrow = length(train_set_1_xg[[1]]))

for (i in seq_along(train_set_1_xg)) {
  TEST[, i] <- train_set_1_xg[[i]]
  print(i)
}
DF <- TEST
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


# Run model ----                
# Define predictors 
PredictorVariables_Areas <- colnames(subset(train_set_1_xg, select = - c(nominal_price, sales_price, rowname, postnr, 
                                                                         Hændelsesdato, Dato, Lag_price, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5)))
PredictorVariables_Lag_price <- colnames(subset(train_set_1_xg, select = - c(nominal_price, sales_price, rowname, postnr, 
                                                                             Hændelsesdato, Dato, Areas, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5)))

train_x = data.matrix(train_set_1_xg[, PredictorVariables_Areas]) # for one with areas
train_x = data.matrix(train_set_1_xg[, PredictorVariables_Lag_price]) # for one with lag price
train_y = train_set_1_xg[,"sales_price"]

# For test set 
test_x = data.matrix(test_set_1_xg[, PredictorVariables_Areas])
test_y = test_set_1_xg[,"sales_price"]

# Define training and test sets 
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

# Watchlist that evaluates the performance 
watchlist = list(train=xgb_train, test=xgb_test)

# Fit model 
model_6 <-  xgb.train(data = xgb_train, max.depth = 6, watchlist=watchlist, nrounds = 10) #
model_26_Area <-  xgb.train(data = xgb_train, max.depth = 26, watchlist=watchlist, nrounds = 500) # 59902.649660
model_26_Lagprice <-  xgb.train(data = xgb_train, max.depth = 26, watchlist=watchlist, nrounds = 500) # 59902.649660


# Importance matrix 
importance_matrix <- xgb.importance(
  feature_names = colnames(xgb_train), 
  model = model_26_Lagprice
)
importance_matrix
xgb.plot.importance(importance_matrix)




save(Total_df_XG, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Total_df_XG.Rdata")
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Total_df_XG.Rdata")

# split data set, like on https://www.statology.org/xgboost-in-r/#:~:text=XGBoost%20in%20R%3A%20A%20Step-by-Step%20Example%201%20Step,5%3A%20Use%20the%20Model%20to%20Make%20Predictions%20
parts = caret::createDataPartition(Total_df_XG$nominal_price, p = .8, list = F)
train = Total_df_XG[parts, ]
test = Total_df_XG[-parts, ]



watchlist = list(train=xgb_train, test=xgb_test)

# fit model 
model_25 = xgb.train(data = xgb_train, max.depth = 25, watchlist=watchlist, nrounds = 50) # 384863.487543
model_5 = xgb.train(data = xgb_train, max.depth = 5, watchlist=watchlist, nrounds = 50) # 908575.294794
model_51 = xgb.train(data = xgb_train, max.depth = 51, watchlist=watchlist, nrounds = 50) # 379630.054265
model_7 = xgb.train(data = xgb_train, max.depth = 10, watchlist=watchlist, nrounds = 50) # 379630.054265





# Comparison ----
library(ISLR)
attach(Carseats)
High <- ifelse(Sales <= 8,"No","Yes")

Carseats <- data.frame(Carseats, High)
Carseats$High <- as.factor(Carseats$High)

tree.carseats <-  tree(formula = High ~.-Sales, data = Carseats )

summary(tree.carseats)
# Notice 9% training error rate

plot(tree.carseats)
text(tree.carseats ,pretty =0)

tree.carseats


set.seed (2)
train=sample(1:nrow(Carseats), 200)
Carseats.test = Carseats [-train ,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred ,High.test)

set.seed (3)
cv.carseats = cv.tree(tree.carseats ,FUN=prune.misclass )
names(cv.carseats )

# Cross Validation 
cv.carseats

# Get the index of the smallest cross-validation error rate
min_index <- which.min(cv.carseats$dev)

# Get the number of nodes corresponding to the smallest cross-validation error rate
nodes_with_min_error <- cv.carseats$size[min_index]

# Print the number of nodes
print(nodes_with_min_error)

# We find that 21 nodes are the best. this gives the lowesst cross-validation error rate 

par(mfrow=c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")


# prune tree 
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats )
text(prune.carseats,pretty=0)

tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred ,High.test)
# No prediction is accurate
# (97+58)/200 = 77.5 %

# Random forest and bagging 
library(randomForest)
library(MASS)
set.seed (1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston ,subset=train)

summary(tree.boston)


plot(tree.boston)
text(tree.boston ,pretty=0)
# Notice how it includes 4 variables of all

# See if pruning the tree increase performance

cv.boston=cv.tree(tree.boston)
plot(cv.boston$size ,cv.boston$dev, type="b")
# We see the tree with lowest deviation (error) is most complex tree with 7 nodes

prune.boston=prune.tree(tree.boston ,best=5)
plot(prune.boston)
text(prune.boston ,pretty=0)


yhat=predict(tree.boston ,newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
# 35.28688


# Random forrest and bagging
library(randomForest)
set.seed (1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,
                        mtry=13,importance =TRUE)
bag.boston
# mtry = 13, betyder at alle prædiktorer skal betragtes for hvert split, derfor god ide at bruge bagging 
# bagging: Bootstrap aggregation, or bagging, is a general-purpose procedure for reducing the variance of 
# a statistical learning method; we introduce it here because it is particularly useful and frequently used 
# in the context of decision trees.
# Instead, we can bootstrap, by taking repeated samples from the (single) training data set. In this approach 
# we generate B different bootstrapped training data sets.

yhat.bag = predict(bag.boston, newdata=Boston[-train ,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

# Bagging improving MSE = 23,59 compared to before of 35.28688

#Random forest with 25 tree and 13 preds
bag.boston=randomForest(medv~.,data=Boston,subset=train, mtry=13,ntree=25)
yhat.bag = predict(bag.boston ,newdata=Boston[-train ,])
mean((yhat.bag-boston.test)^2)
# MSE = 23.66716

#Random forest with 25 tree and 6 preds
set.seed (1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=6,importance =TRUE)
yhat.rf = predict(rf.boston ,newdata=Boston[-train ,])

mean((yhat.rf-boston.test)^2)

# Shows the importance of each variable
importance (rf.boston)
varImpPlot (rf.boston)


# Boosting 
library(gbm)
set.seed (1)
# If binary then distribution "Bernouille"
boost.boston=gbm(medv~.,data=Boston[train,],distribution=
                   "gaussian",n.trees=5000, interaction.depth=4)
summary(boost.boston)
# lstat and rm most important variable 

par(mfrow=c(1,2)) 
plot(boost.boston ,i="rm")  #Median house prises increasing with rm 
plot(boost.boston ,i="lstat") # median house prices depreacing with lstat


yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost -boston.test)^2)
# MSE of 18



# https://www.appsilon.com/post/r-xgboost
# XGboosting
library(xgboost)
library(caTools)
library(dplyr)
library(cvms)
library(caret)

head(iris)

set.seed(42)
sample_split <- sample.split(Y = iris$Species, SplitRatio = 0.7)
train_set <- subset(x = iris, sample_split == TRUE)
test_set <- subset(x = iris, sample_split == FALSE)

y_train <- as.integer(train_set$Species) - 1
y_test <- as.integer(test_set$Species) - 1
X_train <- train_set %>% select(-Species)
X_test <- test_set %>% select(-Species)


xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = length(levels(iris$Species))
)

xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 5000,
  verbose = 1
)

xgb_model

# Importance matrix 
importance_matrix <- xgb.importance(
  feature_names = colnames(xgb_train), 
  model = xgb_model
)
importance_matrix

xgb.plot.importance(importance_matrix)



# DO DELETE ----
# SAR 
# Model not working because multicollinearity
test <- subset(train_set_1_dataframe, select = c(railway_distance, Trainstation_distance))
det(cov(test)) # No problem 

# Use multicoll to detect 
test <- train_set_1_dataframe[sapply(train_set_1_dataframe, is.numeric)]
# add interaction terms 
multiCol(test)


det(cov(test))
plm::detect.lindep(test)


cov <- cov(subset(train_set_1_dataframe, 
                  select = - c(addressID, postnr, enhed_id, Lag_price, Areas))) #Cannot take cov of factor
result <- plm::detect.lindep(subset(train_set_1_dataframe, 
                                    select = - c(rowname, nominal_price, sales_price, addressID, enhed_id, Lag_price, Areas,
                                                 flooded, SA_EV1, SA_EV2, SA_EV3, SA_EV4, SA_EV5, postnr, Dato, Hændelsesdato)))

det(cov) # Large determinant no problem of changing
result <- plm::detect.lindep(subset(train_set_1_dataframe, 
                                    select = - c(nominal_price, postnr, addressID, enhed_id,
                                                 Areas, built_1970_1980, lag_price, Car_Grg, Trainstation)))


PredictorVariables <- colnames(subset(train_set_1_dataframe, 
                                      select = - c(nominal_price, postnr, addressID, enhed_id,
                                                   Areas, Car_Grg)))

Formula <- formula(paste("sales_price ~", 
                         paste(PredictorVariables, collapse=" + ")))
Formula <- formula(nominal_price ~ m2 + Outbuilding + TerracedHouse + rooms + Udbetalt + flooded + forest_distance +
                     coastline_distance + powerline_distance + railway_distance + lake_distance + Trainstation_distance)
NA_DF <- subset(train_set_1_dataframe, is.na(train_set_1_dataframe$nominal_price))

# Outbuilding, built_1950_1960




cov <- cov(train_set_1_TEST_Sub)
det(cov)
library(plm)
result <- plm::detect.lindep(train_set_1_TEST_Sub)

PredictorVariables <- colnames(subset(Total_df, 
                                      select = - c(nominal_price, addressID, enhed_id, postnr,
                                                   Areas, built_1970_1980)))
Formula <- formula(paste("nominal_price ~", 
                         paste(PredictorVariables[1:44], collapse=" + ")))
Formula_SAR <- formula(nominal_price ~ m2)
Formula <- as.formula(paste("nominal_price ~", 
                            paste(c(PredictorVariables[1:19], "flooded*SA_EV1 + flooded*SA_EV2 + flooded*SA_EV3 + flooded*SA_EV4 + flooded*SA_EV5"), collapse=" + ")))


SAR_DF <- spatialreg::spautolm(formula = Formula, data = train_set_1_df, 
                               listw = Neighbor_train_weight, method = "Matrix_J") # Took some hours, about 2
Time <- Sys.time()
SAR_DF <- spatialreg::lagsarlm(formula = Formula, data = train_set_1_df,
                               listw = Neighbor_train_weight, method = "Matrix_J", interval = c(-1, 1))
# Try with LU
# SAR_DF <- spatialreg::lagsarlm(formula = Formula, data = train_set_1_df,
#           listw = Neighbor_train_weight, method = "LU", interval = c(-1, 1))
Stoptime <- Sys.time() - Time

# Matrix 
mat <- as.matrix(subset(train_set_1_df, select = PredictorVariables))

lm_SPAUTO <- stats::lm(formula = Formula, train_set_1_df)

Total_df_SAR <- Total_df
Total_df_SAR_ds <- Total_df_SAR %>%
  distinct(geometry, .keep_all = TRUE)
Total_df_SAR_ds <- sf::st_as_sf(Total_df_SAR_ds)
Total_df_SAR_ds <- as(Total_df_SAR_ds, "Spatial")
Neighbor <- spdep::tri2nb(coordinates(Total_df_SAR_ds[1:1000,]))

Starttime <- Sys.time()
Neighbor <- spdep::knearneigh(coordinates(Total_df_SAR), longlat = TRUE)
Neighbor <- spdep::tri2nb(coordinates(Total_df_SAR[1:1000,]))
HowLongdidIttake <- Sys.time()-Starttime 
HowLongdidIttake

PredictorVariables_SAR <- colnames(subset(Total_df, select = - c(nominal_price, lag_price, addressID, enhed_id, postnr, geometry, Areas)))
Formula_SAR <- formula(paste("nominal_price ~", 
                             paste(PredictorVariables_SAR, collapse=" + ")))
Formula_SAR <- formula(nominal_price ~ forest_distance, coastline_distance)

SAR_listwW <- spdep::nb2listw(Neighbor, style = "W")
SAR_Lag <- spatialreg::lagsarlm(Formula_SAR, data = Total_df_SAR_ds[1:1000,], 
                                listw = SAR_listwW, method = "LU")
SAR_Lag <- spatialreg::lagsarlm(Formula_SAR, data = Total_df_GWR)
summary(SAR_Lag)

install.packages("unix") 
library(unix)
rlimit_as(1e12)  #increases to ~12GB
rlimit_all()

# Load necessary libraries
library(spatialreg)
library(parallel)

Total_df_SAR_ds <- Total_df_SAR_ds[1:100000,]
# Define a function to fit lagsarlm to each subset
fit_lagsarlm <- function(subset) {
  lagsarlm(Formula_SAR, data = Total_df_SAR_ds, listw = SAR_listwW)
}

