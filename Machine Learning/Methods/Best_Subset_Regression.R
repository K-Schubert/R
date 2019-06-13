install.packages("mfp")
install.packages("leaps")
library(mfp)
library(leaps)
data(bodyfat)

bodyfat2 <- bodyfat[, -c(1,2,4)]
fit.lm <- regsubsets(siri~., data=bodyfat2, nvmax=13)
nb.var <- 3
coef(fit.lm, id=nb.var)
plot(summary(fit.lm)$rss, ylab="RSS", xlab="number of variables")
