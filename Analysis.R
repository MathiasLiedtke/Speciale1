####
####



# Library ----
library(stats) # For linear regression estimation, functions as lm
library(tidyverse) # use to arrange sf objects
library(GWmodel) # For Geographically weighted models
library(spgwr) # For Geographically weighted models
    library(rgdal)
    library(maptools)
library(xgboost) # For xgboosting
library(tree) # visualize trees 



# Load in file ----
load("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/10. semester forår 2024/Data/Raw Data/Toke/Total_df_15.Rdata")


# Analysis ----
 
    ## Regression ----

        ### linear model with zip codes ----
        Total_df_15_df <- as.data.frame(Total_df_15)
        Total_df_15_df <- subset(Total_df_15_df, select = - Coor)
        PredictorVariables <- colnames(subset(Total_df_15_df, select = - c(nominal_price, addressID, enhed_id, postnr)))
        Formula <- formula(paste("nominal_price ~", 
                                 paste(PredictorVariables, collapse=" + ")))
        
        Formula <- formula(nominal_price ~ Car_Garage)
        lm_areas <- stats::lm(formula = Formula, Total_df_15_df)
        
        Formula <- formula(nominal_price ~ m2 + district_heating + central_heating + electric_heating)
        lm_areas <- stats::lm(formula = Formula, Total_df_14_df, na.action = NULL)
        
        Formula <- formula(nominal_price ~ m2 + district_heating + central_heating + electric_heating)
        lm_areas <- stats::lm(formula = Formula, Total_df_14_df, na.action = NULL)
        

        ### GWR model ----
        Total_df_15_GWR <- Total_df_15
        Total_df_15_GWR <- sf::st_as_sf(Total_df_15_GWR)
        Total_df_15_GWR <- as(Total_df_15_GWR, "Spatial")
        
        # Total_df_15 <- sf::arrange(Total_df_15)
        # Total_df_15$Coor <- Total_df_15[order(Total_df_15$Coor), ]
        # Total_df_15_GWR_sf <- sf::st_as_sf(Total_df_15)
        # Total_df_15_GWR_sp <- Total_df_15
        # Total_df_15_GWR_sp <- sf::st_as_sf(Total_df_15_GWR_sp)
        # Total_df_15_GWR_sp <- as(Total_df_15_GWR_sp, "Spatial")
        
        PredictorVariables_GWR <- colnames(subset(Total_df_15, select = - c(nominal_price, addressID, enhed_id, postnr, Coor, Areas)))
        Formula_GWR <- formula(paste("nominal_price ~", 
                                 paste(PredictorVariables_GWR, collapse=" + ")))
        Formula_GWR <- formula(nominal_price ~ m2)
        
        # For spgwr package 
        Starttime_BW_spgwr <- Sys.time()
        bw <- gwr.sel(formula = Formula_GWR, data = Total_df_15_GWR[1:5000,], RMSE=TRUE, adapt = TRUE)
        Endtime_BW_spgwr <- Sys.time()-Starttime_BW_spgwr
        
        # For GWmodel package 
        Starttime_BW_GWmodel <- Sys.time()
        bw_GWM <- bw.gwr(formula = Formula_GWR, data = Total_df_15_GWR[1:50000,], kernel = "gaussian",
                     adaptive = TRUE, parallel.method = "cluster")
        Endtime_BW_GWmodel <- Sys.time()-Starttime_BW_GWmodel
        
        
        Startime.GWR <- Sys.time()
        gwr.model<-gwr(formula = Formula_GWR, data = Total_df_15_GWR[1:round(nrow(Total_df_15_GWR)/2),], adapt = bw)
        Endtime_GWR <- Sys.time()-Startime.GWR
        
        
        bw <- gwr.sel(formula = Formula_GWR, data = Total_df_15_GWR[1:round(nrow(Total_df_15_GWR)/2),], adapt=T, RMSE=T)
        
        lm_areas <- stats::lm(formula = Formula, Total_df_15_df)
        

        ### SAR model ----
        
        
        

    ## XGBoosting ----

 



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

yhat.bag = predict(bag.boston ,newdata=Boston[-train ,])
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
