set.seed(42)
k <- 10

cv <- function(trn, k){
  trn_shuffled <- trn[sample(nrow(trn)),]
  folds <- cut(seq(1,nrow(trn_shuffled)),breaks=k,labels=FALSE)
  CV_MSE <- rep(0, 10)
  SE <- rep(0, 10)
  boot <- 0
  
  for (b in seq(100,1000,by=100)){
    boot <- boot + 1
    MSE <- rep(0, k)
    for (i in 1:k){
      test_ind <- which(folds==i,arr.ind=TRUE)
      testData <- trn_shuffled[test_ind, ]
      trainData <- trn_shuffled[-test_ind, ]
      
      ########
      #BAGGING
      B <- b
      pred <- matrix(0, nrow=nrow(testData), ncol=B)
      
      for (b in 1:B){
        ind <- sample(nrow(trainData), nrow(trainData), replace=T)
        mod <- rpart(slice(trainData, ind), method="anova")
        pred[, b] <- predict(mod, newdata=testData)
      }
      
      bagged_pred_fold <- rowSums(pred)/B
      ########
      
      MSE[i] <- (k/nrow(trn))*sum((bagged_pred_fold-testData$y)^2)
    }
    CV_MSE[boot] <- (1/k)*sum(MSE)
    SE[boot] <- (1/sqrt(k))*sqrt(sum((MSE-CV_MSE[boot])^2)/(k-1))
  }
  data <- list(mse = MSE, cv_mse = CV_MSE, se = SE)
  return(data)
}

a <- cv(train_tr, k)
CV_MSE <- a$cv_mse
SE <- a$se


pdf("Pict/Bagging_CV_RMSE_B.pdf", height=4, width=8)
plot(seq(100,1000,by=100), sqrt(CV_MSE), ylim=c(91, 93), ylab="CV RMSE", xlab="B", main="CV RMSE of Bagging models as a function of B", type="o")
points(400, min(sqrt(CV_MSE)), col="red", cex=2)
dev.off()


