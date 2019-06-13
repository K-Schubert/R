library(MASS)
iris2 <- iris[, c("Sepal.Width", "Petal.Width", "Species")]
lda.iris <- lda(Species~Sepal.Width + Petal.Width, data=iris2)
xgrid <- ygrid <- seq(0, 6, 0.01)
xygrid <- expand.grid(xgrid, ygrid)
colnames(xygrid) <- c("Sepal.Width", "Petal.Width")
pred1 <- predict(lda.iris, newdata=xygrid)$class
levels(pred1) <- c(1,2,3)
plot(iris2[, c("Sepal.Width", "Petal.Width")], col=iris2[, "Species"])
contour(xgrid, ygrid, matrix(pred1, length(xgrid), length(ygrid)), levels=c(1,2,3), add=T, col="blue", drawlabels=F)
