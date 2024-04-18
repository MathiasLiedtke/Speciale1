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



# Load in file ----
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_15.Rdata")

# Export to QGIS ----
Total_df_15 <- sf::st_as_sf(Total_df_15)
sf::st_write(Total_df_15, "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Total_df_15.shp")

# Load in with heights ----
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_16.Rdata")
Total_df_16$Coor <- centroid <- sf::st_centroid(Total_df_16$geometry)
# QGIS renamed columns, so we change them back 
colnames(Total_df_16)
colnames(Total_df_15)[1:51] # The first 51 variables are aligned 
colnames(Total_df_16)[1:51] <- colnames(Total_df_15)[1:51]
Total_df_16$geometry <- sf::st_as_text(Total_df_16$geometry)
Total_df_16 <- sf::st_set_geometry(Total_df_16, Total_df_16$Coor)
st_agr(Total_df_16) <- "Coor"
Total_df_16 <- subset(Total_df_16, select = -Coor)
Total_df_16$Areas <- as.factor(Total_df_16$Areas)

# Change variables to desired form like relevant literature ----
    # Create training sets ----
    # Partition 60% for training 
    p <- 0.6    
    iT <- p*nrow(Total_df_16)
    ## Train 1
    set.seed(13)
    train_seq_1 <- sample(nrow(Total_df_16), size = iT, replace = FALSE)
    train_set_1 <- Total_df_16[train_seq_1, , drop = FALSE]
    ## Test 1
    test_set_1 <- Total_df_16[-train_seq_1, , drop = FALSE]
    
    ## Train 2
    set.seed(200)
    train_seq_2 <- sample(nrow(Total_df_16), size = iT, replace = FALSE)
    train_set_2 <- Total_df_16[train_seq_2, , drop = FALSE]
    ## Test 2
    test_set_2 <- Total_df_16[-train_seq_2, , drop = FALSE]
    

# Preliminary analysis ----
    ## Test for spatial autocorrelation ----
        ### First part ----
        # Train 1
        T1 <- Sys.time()
        train_set_1_df <- train_set_1 %>%
          distinct(Coor, .keep_all = TRUE)
        train_set_1_df <- sf::st_as_sf(train_set_1_df)
        train_set_1_df <- as(train_set_1_df, "Spatial")
        points_train_1 <- sp::coordinates(train_set_1_df)
        Neighbor_train <- spdep::tri2nb(points_train_1)
        save(Neighbor_train, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_train.Rdata")
        T2 <- Sys.time() - T1 # 1.513962 hours
        
        # Test 1
        T3 <- Sys.time()
        test_set_1_df <- test_set_1 %>%
          distinct(Coor, .keep_all = TRUE)
        test_set_1_df <- sf::st_as_sf(test_set_1_df)
        test_set_1_df <- as(test_set_1_df, "Spatial")
        points_test_1 <- sp::coordinates(test_set_1_df)
        Neighbor_test <- spdep::tri2nb(points_test_1)
        save(Neighbor_test, file = "/Users/mathiasliedtke/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Neighbor_test.Rdata")
        T4 <- Sys.time() - T3 # 53.89177 mins
        
        # Spatial weights object 
        summary(Neighbor_train) # see how many neighbors per observation. 
        Neighbor_train_weight <- nb2listw(Neighbor_train)
        Neighbor_train_weight # summary of weights
        # Global Moran 
        Moran <- spdep::moran.test(train_set_1_df$nmnl_pr, listw = Neighbor_train_weight)
        lmtest::bptest(train_set_1_df)
        # Spatial autocorrelation in model, correct with heteroscedasticity. Applied spatial data analysis with R, P. 275- 
        PredictorVariables <- colnames(subset(Total_df_16, select = - c(nominal_price, addressID, enhed_id, postnr, 
                                                                        geometry, Areas, lag_price)))
        Formula <- formula(paste("nominal_price ~", 
                                 paste(PredictorVariables[1:48], collapse=" + ")))
        
        train_set_1
        lm_SPAUTO <- stats::lm(formula = Formula, Total_df_16)
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
    


# Analysis ----
 
    ## Regression ----

        ### linear model with zip codes ----
        Total_df_16_df <- as.data.frame(Total_df_16)
        Total_df_16_df <- subset(Total_df_16_df, select = - Coor)
        PredictorVariables <- colnames(subset(Total_df_16_df, select = - c(nominal_price, addressID, enhed_id, postnr)))
        Formula <- formula(paste("nominal_price ~", 
                                 paste(PredictorVariables, collapse=" + ")))
        
        Formula <- formula(nominal_price ~ Car_Garage)
        lm_areas <- stats::lm(formula = Formula, Total_df_16_df)
        
        Formula <- formula(nominal_price ~ m2 + district_heating + central_heating + electric_heating)
        lm_areas <- stats::lm(formula = Formula, Total_df_14_df, na.action = NULL)
        
        Formula <- formula(nominal_price ~ m2 + district_heating + central_heating + electric_heating)
        lm_areas <- stats::lm(formula = Formula, Total_df_14_df, na.action = NULL)
        

        ### GWR model ----
        Total_df_16_GWR <- Total_df_16
        Total_df_16_GWR <- sf::st_as_sf(Total_df_16_GWR)
        Total_df_16_GWR <- as(Total_df_16_GWR, "Spatial")
        
        # Total_df_16 <- sf::arrange(Total_df_16)
        # Total_df_16$Coor <- Total_df_16[order(Total_df_16$Coor), ]
        # Total_df_16_GWR_sf <- sf::st_as_sf(Total_df_16)
        # Total_df_16_GWR_sp <- Total_df_16
        # Total_df_16_GWR_sp <- sf::st_as_sf(Total_df_16_GWR_sp)
        # Total_df_16_GWR_sp <- as(Total_df_16_GWR_sp, "Spatial")
        
        PredictorVariables_GWR <- colnames(subset(Total_df_16, select = - c(nominal_price, addressID, enhed_id, postnr, Coor, Areas)))
        Formula_GWR <- formula(paste("nominal_price ~", 
                                 paste(PredictorVariables_GWR, collapse=" + ")))
        Formula_GWR <- formula(nominal_price ~ m2)
        
        # For spgwr package 
        Starttime_BW_spgwr <- Sys.time()
        bw <- gwr.sel(formula = Formula_GWR, data = Total_df_16_GWR[1:5000,], RMSE=TRUE, adapt = TRUE)
        Endtime_BW_spgwr <- Sys.time()-Starttime_BW_spgwr
        
        # For GWmodel package 
        Starttime_BW_GWmodel <- Sys.time()
        bw_GWM <- bw.gwr(formula = Formula_GWR, data = Total_df_16_GWR[1:50000,], kernel = "gaussian",
                     adaptive = TRUE, parallel.method = "cluster")
        Endtime_BW_GWmodel <- Sys.time()-Starttime_BW_GWmodel
        
        
        Startime.GWR <- Sys.time()
        gwr.model<-gwr(formula = Formula_GWR, data = Total_df_16_GWR[1:round(nrow(Total_df_16_GWR)/2),], adapt = bw)
        Endtime_GWR <- Sys.time()-Startime.GWR
        
        
        bw <- gwr.sel(formula = Formula_GWR, data = Total_df_16_GWR[1:round(nrow(Total_df_16_GWR)/2),], adapt=T, RMSE=T)
        
        lm_areas <- stats::lm(formula = Formula, Total_df_16_df)
        

        ### SAR model ----
        # Make neighbor list
        SAR_DF <- spatialreg::spautolm(formula = Formula, data = train_set_1_df, 
                                       listw = Neighbor_train_weight, method = "Matrix_J") # Took some hours, about 2
        Time <- Sys.time()
        SAR_DF <- spatialreg::lagsarlm(formula = Formula, data = train_set_1_df, 
                      listw = Neighbor_train_weight, method = "Matrix_J", interval = c(-1, 1))
        Stoptime <- Sys.time() - Time
        
        
        lm_SPAUTO <- stats::lm(formula = Formula, train_set_1_df)
        
        Total_df_16_SAR <- Total_df_16
        Total_df_16_SAR_ds <- Total_df_16_SAR %>%
                              distinct(Coor, .keep_all = TRUE)
        Total_df_16_SAR_ds <- sf::st_as_sf(Total_df_16_SAR_ds)
        Total_df_16_SAR_ds <- as(Total_df_16_SAR_ds, "Spatial")
        Neighbor <- spdep::tri2nb(coordinates(Total_df_16_SAR_ds[1:10000,]))

        
        
        Starttime <- Sys.time()
        Neighbor <- spdep::knearneigh(coordinates(Total_df_16_SAR), longlat = TRUE)
        Neighbor <- spdep::tri2nb(coordinates(Total_df_16_SAR[1:1000,]))
        HowLongdidIttake <- Sys.time()-Starttime 
        HowLongdidIttake
        
        PredictorVariables_SAR <- colnames(subset(Total_df_16, select = - c(nominal_price, lag_price, addressID, enhed_id, postnr, Coor, Areas)))
        Formula_SAR <- formula(paste("nominal_price ~", 
                                     paste(PredictorVariables_SAR, collapse=" + ")))
        Formula_SAR <- formula(nominal_price ~ m2)
        
        SAR_listwW <- spdep::nb2listw(Neighbor, style = "W")
        SAR_Lag <- spatialreg::lagsarlm(Formula_SAR, data = Total_df_16_SAR_ds[1:10000,], listw = SAR_listwW)
        SAR_Lag <- spatialreg::lagsarlm(Formula_SAR, data = Total_df_16_GWR)
        summary(SAR_Lag)
        
        install.packages("unix") 
        library(unix)
        rlimit_as(1e12)  #increases to ~12GB
        rlimit_all()

        # Load necessary libraries
        library(spatialreg)
        library(parallel)
        
        Total_df_16_SAR_ds <- Total_df_16_SAR_ds[1:100000,]
        # Define a function to fit lagsarlm to each subset
        fit_lagsarlm <- function(subset) {
          lagsarlm(Formula_SAR, data = Total_df_16_SAR_ds, listw = SAR_listwW)
        }
        
        # Apply lagsarlm in parallel
        num_cores <- detectCores()-1
        results <- mclapply(list(Total_df_16_SAR_ds), fit_lagsarlm, mc.cores = num_cores)
        
        # Combine results as needed
        # ...
        
        
        
    ## XGBoosting ----
        # packages: xgboost and caret 
        # Remove attributes from df
        Total_df_16_XG <- Total_df_16
        one_entry <- function(x) {
          for (i in length(x)) attr(x[[i]], "names") <- NULL
          return(x)
        }
        Total_df_16_XG <- lapply(Total_df_16_XG, FUN=one_entry)
        Total_df_16_XG$Coor <- sf::st_as_text(Total_df_16_XG$Coor)
        TEST <- matrix(ncol = length(Total_df_16_XG), nrow = length(Total_df_16_XG[[1]]))
        
        for (i in seq_along(Total_df_16_XG)) {
          TEST[, i] <- Total_df_16_XG[[i]]
          print(i)
        }
        DF <- TEST
        rm(TEST)
        colnames(DF) <- names(Total_df_16_XG)
        
        # Delete character variables to change to numeric matrix 
        Total_df_16_XG <- DF
        rm(DF)
        Total_df_16_XG <- subset(Total_df_16_XG, select = -c(addressID, enhed_id, Coor))
        Total_df_16_XG <- as.data.frame(Total_df_16_XG)
        
        # Change to numeric
        library(dplyr)
        Total_df_16_XG <- Total_df_16_XG %>% mutate_if(is.character, as.numeric)
        
        
        save(Total_df_16_XG, file = "~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Total_df_16_XG.Rdata")
        load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Clean Data/Total_df_16_XG.Rdata")
        
        # split data set, like on https://www.statology.org/xgboost-in-r/#:~:text=XGBoost%20in%20R%3A%20A%20Step-by-Step%20Example%201%20Step,5%3A%20Use%20the%20Model%20to%20Make%20Predictions%20
        parts = caret::createDataPartition(Total_df_16_XG$nominal_price, p = .8, list = F)
        train = Total_df_16_XG[parts, ]
        test = Total_df_16_XG[-parts, ]
        
        # Define predictor and response variable
        PredictorVariables_XG <- colnames(subset(Total_df_16_XG, select = - c(nominal_price, lag_price, Areas)))
        train_x = data.matrix(train[, PredictorVariables_XG])
        train_y = train[,"nominal_price"]
        
        # For test set 
        PredictorVariables_XG <- colnames(subset(Total_df_16_XG, select = - c(nominal_price, lag_price, Areas)))
        test_x = data.matrix(test[, PredictorVariables_XG])
        test_y = test[,"nominal_price"]
        
        # Define training and test sets 
        xgb_train = xgb.DMatrix(data = train_x, label = train_y)
        xgb_test = xgb.DMatrix(data = test_x, label = test_y)
        
        xgb_train = xgb.DMatrix(data = train_x, label = train_y)
        xgb_test = xgb.DMatrix(data = test_x, label = test_y)
        
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
