

set.seed(42)
k <- 10

cv <- function(trn, k){
  trn_shuffled <- trn[sample(nrow(trn)),]
  folds <- cut(seq(1,nrow(trn_shuffled)),breaks=k,labels=FALSE)
  CV_MSE <- rep(0, 22)
  SE <- rep(0, 22)
  h <- 0
  
  for (m in seq(5, ncol(trn)-1, by=5)){
    h <- h + 1
    MSE <- rep(0, k)
    for (i in 1:k){
      test_ind <- which(folds==i,arr.ind=TRUE)
      testData <- trn_shuffled[test_ind, ]
      trainData <- trn_shuffled[-test_ind, ]
      
      ########
      #RF
      rf_mod <- randomForest(y~., data=trainData, mtry=m, importance=F, na.action=na.omit)
      rf_pred <- predict(rf_mod, newdata=testData)
      ########
     
      MSE[i] <- (k/nrow(trn))*sum((rf_pred-testData$y)^2)
    }
    CV_MSE[h] <- (1/k)*sum(MSE)
    SE[h] <- (1/sqrt(k))*sqrt(sum((MSE-CV_MSE[h])^2)/(k-1))
  }
  data <- list(mse = MSE, cv_mse = CV_MSE, se = SE)
  return(data)
}

a <- cv(train_tr, k)
CV_MSE <- a$cv_mse
SE <- a$se


pdf("Pict/RF_CV_RMSE_m.pdf", height=4, width=8)
plot(seq(5,111,by=5), sqrt(CV_MSE), ylab="CV RMSE", xlab="m", main="CV RMSE of RF models as a function of m (ntree=500)", type="o")
points(70, min(sqrt(CV_MSE)), col="red", cex=2)
dev.off()



########################################

set.seed(42)
k <- 10

cv <- function(trn, k){
  trn_shuffled <- trn[sample(nrow(trn)),]
  folds <- cut(seq(1,nrow(trn_shuffled)),breaks=k,labels=FALSE)
  CV_MSE <- rep(0, 20)
  SE <- rep(0, 20)
  h <- 0
  
  for (n in seq(50, 1000, by=50)){
    h <- h + 1
    MSE <- rep(0, k)
    for (i in 1:k){
      test_ind <- which(folds==i,arr.ind=TRUE)
      testData <- trn_shuffled[test_ind, ]
      trainData <- trn_shuffled[-test_ind, ]
      
      ########
      #RF
      rf_mod <- randomForest(y~., data=trainData, mtry=70, ntree=n, importance=F, na.action=na.omit)
      rf_pred <- predict(rf_mod, newdata=testData)
      ########
      
      MSE[i] <- (k/nrow(trn))*sum((rf_pred-testData$y)^2)
    }
    CV_MSE[h] <- (1/k)*sum(MSE)
    SE[h] <- (1/sqrt(k))*sqrt(sum((MSE-CV_MSE[h])^2)/(k-1))
  }
  data <- list(mse = MSE, cv_mse = CV_MSE, se = SE)
  return(data)
}

a <- cv(train_tr, k)
CV_MSE <- a$cv_mse
SE <- a$se

pdf("Pict/RF_CV_RMSE_ntree.pdf", height=4, width=8)
plot(seq(50, 1000, by=50), sqrt(CV_MSE), ylab="CV RMSE", xlab="ntree", main="CV RMSE of RF models as a function of ntree (m=70)", type="o")
points(550, min(sqrt(CV_MSE)), col="red", cex=2)
dev.off()





