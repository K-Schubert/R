setwd("/Users/kieranschubert/Desktop/Statistics/2nd_Semester/Machine Learning/ML/ML Challenge")
rm(list=ls())

trn <- read.csv("trainingdata.csv", header=T)
test <- read.csv("test_predictors.csv", header=T)
head(trn)
str(trn)
p <- ncol(trn) - 1 
n_trn <- nrow(trn)

head(test)
str(test)
ncol(test)
n_test <- nrow(test)

y_train <- trn$y
y_test <- test$y

#EDA
par(mfrow=c(2,2))
plot(y,type="l", main="y (unscaled) vs index")
hist(y, main="Histogram of y")
plot(density(y), main="Kernel density of y")
qqnorm(y, main="Normal QQ-plot of y")
qqline(y, col="red")



#5 by 5 relationships between variables
for (i in seq(1,ncol(trn),5)){
plot(trn[,c(i:(i+5))])
}
#5 by 5 relationships between response and variables
for (i in seq(2,ncol(trn),5)){
  plot(trn[,c(1,i:(i+5))])
}

#check y vs X62, y vs X60
head(trn)


#Linear model
mod <- lm(y~X60+X62, data=data)
summary(mod)

#Ridge, Lasso, Elastic net
library(glmnet)

#Ridge regression
grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(as.matrix(trn[,-1]), y, alpha=0, lambda=grid, standardize=TRUE)
ridge.mod$a0
dim(coef(ridge.mod))
plot(ridge.mod)
set.seed(1)
cv.ridge <- cv.glmnet(as.matrix(trn[,-1]), y, alpha=0, nfolds=10)
plot(cv.ridge)

opt_lambda <- cv.ridge$lambda.1se
opt_lambda

fit <- cv.ridge$glmnet.fit
y_predicted <- predict(fit, s = opt_lambda, newx = as.matrix(trn[,-1]))

pred_ridge <- data.frame(ID=seq(1,350), y=y_predicted)
colnames(pred_ridge) <- c("ID", "y")
write.csv(pred_ridge, "pred_ridge.csv", row.names = FALSE)

# Sum of Squares Total and Error
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq

#Lasso regression
lasso.mod <- glmnet(as.matrix(trn[,-1]), y, alpha=1, lambda=grid, standardize=TRUE)
plot(lasso.mod)
cv.lasso <- cv.glmnet(as.matrix(trn[,-1]), y, alpha=1, nfolds=10)
plot(cv.lasso)

opt_lambda <- cv.lasso$lambda.1se
opt_lambda

fit <- cv.lasso$glmnet.fit
y_predicted <- predict(fit, s = opt_lambda, newx = as.matrix(trn[,-1]))

sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

rsq <- 1 - sse / sst
rsq

#Elastic net

