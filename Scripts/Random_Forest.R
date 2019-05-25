library(randomForest)
set.seed(2)
rf.mod <- randomForest(y~., data=trn, mtry=37, importance=T, na.action=na.omit)


predict(rf.mod)


tree.RF <- randomForest(y~., data=X, importance=T, na.action=na.omit)



rf.model <- randomForest(uptake ~ ., 
                         data = CO2,
                         ntree = 50,
                         nodesize = 5,
                         mtry=2,
                         importance=TRUE, 
                         metric="RMSE")

rf_caret <- train(y ~ X1+X2, 
                         data = trn,
                         method="rf",
                         ntree=50,
                         tuneGrid=data.frame(mtry=2),
                         nodesize = 5,
                         importance=TRUE, 
                         metric="RMSE",
                         trControl = trainControl(method="oob"),
                         allowParallel=FALSE)







bag_caret <- caret::train(form=y~X1+X2, data=X,
                          method = "bag",
                          B = 10,
                          bagControl = bagControl(fit = ldaBag$fit,
                                                  predict = ldaBag$pred,
                                                  aggregate = ldaBag$aggregate),
                          tuneGrid = data.frame(vars = c((1:10)*10 , ncol(X))),
                          trControl = trainControl(
                            method = "cv", 
                            number = 10,  # Nr of folds
                            selectionFunction = "oneSE")
)

train(form=y~X1+X2, data=X)
