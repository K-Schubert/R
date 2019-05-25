setwd("/Users/kieranschubert/Desktop/Statistics/2nd_Semester/Machine Learning/ML/Report_2_final_submission")
install.packages("gbm")
require(gbm)
require(magrittr)
require(tibble)
# for reproducibility
set.seed(123)

# train GBM model
gbm.fit <- gbm(
  formula = y ~ .,
  distribution = "gaussian",
  data = train_tr,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 10,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# print results
print(gbm.fit)

sqrt(min(gbm.fit$cv.error))
gbm.perf(gbm.fit, method = "cv")


# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)


# randomize data
random_index <- sample(1:nrow(train_tr), nrow(train_tr))
random_train <- train_tr[random_index, ]

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = y ~ .,
    distribution = "gaussian",
    data = random_train,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

# modify hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .05, .1, 0.15, 0.2, 0.3),
  interaction.depth = c(3, 5, 7, 10),
  n.minobsinnode = c(5, 7, 10),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = y ~ .,
    distribution = "gaussian",
    data = random_train,
    n.trees = 6000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

pdf("Pict/grid_search_params.pdf", height=5, width=10)
grid_search <- hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)
grid_search[, 6] <- round(grid_search[, 6], digits=2)
grid.table(grid_search, rows=NULL)
dev.off()
# for reproducibility
set.seed(123)

# CV error of 10 best models with previous hyper-parameters
CV_RMSE_full <- rep(0, 10)

for (i in 1:10){
  gbm.fit.final <- gbm(
    formula = y ~ .,
    distribution = "gaussian",
    data = train_tr,
    n.trees = grid_search$optimal_trees[i],
    interaction.depth = grid_search$interaction.depth[i],
    shrinkage = grid_search$shrinkage[i],
    n.minobsinnode = grid_search$n.minobsinnode[i],
    bag.fraction = grid_search$bag.fraction[i], 
    train.fraction = 1,
    cv.folds = 10,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )  
  
  # find index for n trees with minimum CV error
  min_MSE <- which.min(gbm.fit.final$cv.error)
  CV_RMSE_full[i] <- sqrt(gbm.fit.final$cv.error[min_MSE])
}

# get MSE and compute RMSE
CV_RMSE_full1 <- 78.95 #1st best model from 1st optim

pdf("Pict/CV_RMSE_full.pdf", height=4, width=8)
plot(1:10, CV_RMSE_full, type="o", main="CV RMSE for 10 best hyperparameter values", xlab="Best Models", ylab="CV RMSE", ylim=c(66, 75))
points(which.min(CV_RMSE_full), min(CV_RMSE_full), col="red", cex=2)
dev.off()

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit.final, method = "cv")

#Visualizing variable importance
par(mar = c(5, 8, 1, 1))
pdf("Pict/varimp_boosting.pdf", height=5, width=10)
summary(
  gbm.fit.final, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)
dev.off()


# predict values for test data
pred <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, test_tr)

pred_boost <- tibble(ID = 1:350, y = pred)
write.csv(pred_boost, "Predictions/pred_boost.csv", row.names = FALSE)

CV_err <- sqrt(gbm.fit.final$cv.error[min_MSE])
sumTable_BoostModFull <- data.frame(model="Full Model Boosting", p=111, CV_error=CV_err_full, Public_Leaderboard=67.04)






###################################
# BOOSTING WITH ENGINEERED FEATURES
# X32, X34, X35, X55, X59, X60, X62, X68, X75, X104

subset <- c("X32", "X34", "X35", "X55", "X59", "X60", "X62", "X68", "X75", "X104")
train_eng <- train_tr[, subset]

numeric <- unlist(lapply(train_eng, is.numeric))

square <- function(x){
  return(x^2)
}

cube <- function(x){
  return(x^3)
}

square_feat <- apply(train_eng[, numeric], MARGIN=2, FUN=square)
colnames(square_feat) <- c("X59^2", "X60^2", "X62^2", "X104^2")
cube_feat <- apply(train_eng[, numeric], MARGIN=2, FUN=cube)
colnames(cube_feat) <- c("X59^3", "X60^3", "X62^3", "X104^3")
train_eng <- cbind(train_tr$y, train_eng, square_feat, cube_feat)
  
  
# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, 0.3),
  interaction.depth = c(3, 5, 7, 10),
  n.minobsinnode = c(5, 7, 10),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)

# randomize data
#random_index <- sample(1:nrow(train_eng), nrow(train_eng))
#random_train <- train_eng[random_index, ]
random_index <- sample(1:nrow(train_tr), nrow(train_tr))
random_train <- train_tr[random_index, ]

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = y ~ .,
    distribution = "gaussian",
    data = random_train,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)








###### CV RMSE

# MODEL 1: train GBM model with squared and cubed features
train_eng <- train_tr[, subset]
square_feat <- apply(train_eng[, numeric], MARGIN=2, FUN=square)
colnames(square_feat) <- c("X59^2", "X60^2", "X62^2", "X104^2")
cube_feat <- apply(train_eng[, numeric], MARGIN=2, FUN=cube)
colnames(cube_feat) <- c("X59^3", "X60^3", "X62^3", "X104^3")
train_eng <- cbind(train_tr$y, train_eng, square_feat, cube_feat)
colnames(train_eng)[1] <- "y"

random_index <- sample(1:nrow(train_eng), nrow(train_eng))
random_train <- train_eng[random_index, ]

gbm.fit.final <- gbm(
  formula = y ~ .,
  distribution = "gaussian",
  data = random_train,
  n.trees = 4999,
  interaction.depth = 5,
  shrinkage = 0.01,
  n.minobsinnode = 7,
  bag.fraction = 1, 
  train.fraction = 1,
  cv.folds = 10,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit.final$cv.error)

# get MSE and compute RMSE
CV_RMSE_subset1 <- sqrt(gbm.fit.final$cv.error[min_MSE])
sumTable_BoostMod1 <- data.frame(model="Model 1 Subset Boosting", p=18, cv_error=CV_RMSE_subset1, leaderboard=NA)


# MODEL 2: train GBM model with squared features
train_eng <- train_tr[, subset]
square_feat <- apply(train_eng[, numeric], MARGIN=2, FUN=square)
colnames(square_feat) <- c("X59^2", "X60^2", "X62^2", "X104^2")
train_eng <- cbind(train_tr$y, train_eng, square_feat)
colnames(train_eng)[1] <- "y"

random_index <- sample(1:nrow(train_eng), nrow(train_eng))
random_train <- train_eng[random_index, ]

gbm.fit.final <- gbm(
  formula = y ~ .,
  distribution = "gaussian",
  data = random_train,
  n.trees = 4999,
  interaction.depth = 5,
  shrinkage = 0.01,
  n.minobsinnode = 7,
  bag.fraction = 1, 
  train.fraction = 1,
  cv.folds = 10,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit.final$cv.error)

# get MSE and compute RMSE
CV_RMSE_subset2 <- sqrt(gbm.fit.final$cv.error[min_MSE])
sumTable_BoostMod2 <- data.frame(model="Model 2 Boosting", p=14, cv_error=CV_RMSE_subset2, leaderboard=NA)

# MODEL 3: train GBM model with all 2-way interactions
train_eng <- train_tr[, subset]
train_eng <- cbind(train_tr$y, train_eng)
colnames(train_eng)[1] <- "y"

random_index <- sample(1:nrow(train_eng), nrow(train_eng))
random_train <- train_eng[random_index, ]

gbm.fit.final <- gbm(
  formula = y ~ (X32+X34+X35+X55+X59+X60+X62+X68+X75+X104)^2,
  distribution = "gaussian",
  data = random_train,
  n.trees = 4999,
  interaction.depth = 5,
  shrinkage = 0.01,
  n.minobsinnode = 7,
  bag.fraction = 1, 
  train.fraction = 1,
  cv.folds = 10,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit.final$cv.error)

# get MSE and compute RMSE
CV_RMSE_subset3 <- sqrt(gbm.fit.final$cv.error[min_MSE])
sumTable_BoostMod3 <- data.frame(model="Model 3 Boosting", p=55, cv_error=CV_RMSE_subset3, leaderboard=NA)

# MODEL 4: train GBM model with only subset
train_eng <- train_tr[, subset]
train_eng <- cbind(train_tr$y, train_eng)
colnames(train_eng)[1] <- "y"

random_index <- sample(1:nrow(train_eng), nrow(train_eng))
random_train <- train_eng[random_index, ]

gbm.fit.final <- gbm(
  formula = y ~ .,
  distribution = "gaussian",
  data = random_train,
  n.trees = 4999,
  interaction.depth = 5,
  shrinkage = 0.01,
  n.minobsinnode = 7,
  bag.fraction = 1, 
  train.fraction = 1,
  cv.folds = 10,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit.final$cv.error)

# get MSE and compute RMSE
CV_RMSE_subset4 <- sqrt(gbm.fit.final$cv.error[min_MSE])
sumTable_BoostMod4 <- data.frame(model="Model 4 Boosting", p=10, cv_error=CV_RMSE_subset4, leaderboard=67.59)

CV_RMSE_subset <- c(CV_RMSE_subset1,CV_RMSE_subset2,CV_RMSE_subset3,CV_RMSE_subset4)

pdf("Pict/CV_RMSE_subset.pdf", height=5, width=10)
plot(1:4, CV_RMSE_subset, type="o", ylim=c(60, 75), main="CV RMSE of subset models", xlab="Model", ylab="CV RMSE")
points(which.min(CV_RMSE_subset), min(CV_RMSE_subset), col="red", cex=2)
dev.off()

# USE MODEL 4 FOR PREDICTION
train_eng <- train_tr[, subset]
train_eng <- cbind(train_tr$y, train_eng)
colnames(train_eng)[1] <- "y"

random_index <- sample(1:nrow(train_eng), nrow(train_eng))
random_train <- train_eng[random_index, ]

gbm.fit.final <- gbm(
  formula = y ~ .,
  distribution = "gaussian",
  data = random_train,
  n.trees = 4999,
  interaction.depth = 5,
  shrinkage = 0.01,
  n.minobsinnode = 7,
  bag.fraction = 1, 
  train.fraction = 1,
  #cv.folds = 10,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

sumTable_boost2 <- data.frame(model="Model 4 Subset Boosting", p=10, cv_error=66.52, leaderboard=67.59)


pred <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, test_tr)
pred_boost2 <- tibble(ID = 1:350, y = pred)
write.csv(pred_boost2, "Predictions/pred_boost2.csv", row.names = FALSE)

sumTable_BoostMod1
sumTable_BoostMod2
sumTable_BoostMod3
sumTable_BoostMod4

sumTable_boost1 <- data.frame(model="Full Model Boosting", p=111, CV_error=78.95, Public_Leaderboard=67.04)
sumTable_boost2



