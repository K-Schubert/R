install.packages("e1071")
library(e1071)

#Gamma tuning
iris2 <- iris[, c("Sepal.Width", "Sepal.Length", "Species")]
xgrid <- ygrid <- seq(0, 10, 0.02)
xygrid <- expand.grid(xgrid, ygrid)
colnames(xygrid) <- c("Sepal.Width", "Sepal.Length")
gamma.vec <- c(0.1, 1, 20)
par(mfrow=c(1,3))
for (i in 1:length(gamma.vec)){
  svm.iris <- svm(Species~., data=iris2, kernel="radial", gamma=gamma.vec[i], cost=1)
  pred <- predict(svm.iris, newdata=xygrid)
  levels(pred) <- c(1,2,3)
  plot(iris2[, c("Sepal.Width", "Sepal.Length")], col=iris2[, "Species"], main=paste("gamma=", gamma.vec[i]))
  contour(xgrid, ygrid, matrix(pred, length(xgrid), length(ygrid)), levels=c(1,2,3), add=T, col="blue", drawlabels = F)
}

#Cost tuning
cost.vec <- c(0.1, 20)
par(mfrow=c(1,2))
for (i in 1:length(cost.vec)){
  svm.iris <- svm(Species~., data=iris2, kernel="radial", gamma=20, cost=cost.vec[i])
  pred <- predict(svm.iris, newdata=xygrid)
  levels(pred) <- c(1,2,3)
  plot(iris2[, c("Sepal.Width", "Sepal.Length")], col=iris2[, "Species"], main=paste("cost=", cost.vec[i]))
  contour(xgrid, ygrid, matrix(pred, length(xgrid), length(ygrid)), levels=c(1,2,3), add=T, col="blue", drawlabels = F)
}

#Built-in tuning function
set.seed(15)
tune.out <- tune(svm, Species~., data=iris2, kernel="radial", ranges=list(cost=c(1,5,10), gamma=c(0.01,0.1,1,10,100)))
tune.out$best.parameters
best.svm <- svm(Species~., data=iris2, kernel="radial", gamma=tune.out$best.parameters$gamma, cost=tune.out$best.parameters$cost)
best.pred <- predict(best.svm, newdata=xygrid)
levels(best.pred) <- c(1,2,3)
par(mfrow=c(1,1))
plot(iris2[, c("Sepal.Width", "Sepal.Length")], col=iris2[, "Species"])
contour(xgrid, ygrid, matrix(best.pred, length(xgrid), length(ygrid)), levels=c(1,2,3), add=T, col="blue", drawlabels = F)
