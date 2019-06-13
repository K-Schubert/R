B <- 1000
pred <- matrix(0, nrow=nrow(X), ncol=B)

for (b in 1:B){
  ind <- sample(nrow(trn), nrow(trn), replace=T)
  mod <- lm(y~., data=test[ind,])
  pred[, b] <- predict(mod)
}

bagged_pred <- rowSums(pred)/B

bagged_pred

bagged_pred-trn$y
