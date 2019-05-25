rm(list=ls())
install.packages("plyr")
library(plyr)

trn <- read.csv("trainingdata.csv", header=T)
test <- read.csv("test_predictors.csv", header=T)

p <- 5 #subset size
regressors <- sample(colnames(trn)[-1], p, replace=F)
X.train <- trn[regressors]
X.test <- test[regressors]
n <- nrow(X.train)

y.train <- trn[, 1]
y.test <- test[, 1]


#Forward selection
#There are 2^p subsets for p features (2^p - 1 excluding the null model)

#rep(c(TRUE, FALSE), 5)
#expand.grid(rep(c(TRUE, FALSE), 5))

regMat <- expand.grid(c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE))

regMat <- regMat[-(dim(regMat)[1]),] #The last row is the null model so we remove it
names(regMat) <- regressors

allModelsList <- apply(regMat, 1, function(x) as.formula(paste(c("y.train~1", regressors[x]), collapse="+")))

allModelsResults <- lapply(allModelsList, function(x) lm(x, data=X.train))

dfCoefNum   <- ldply(allModelsResults, function(x) as.data.frame(
  t(coef(x))))
dfStdErrors <- ldply(allModelsResults, function(x) as.data.frame(
  t(coef(summary(x))[, "Std. Error"])))
dftValues   <- ldply(allModelsResults, function(x) as.data.frame(
  t(coef(summary(x))[, "t value"])))
dfpValues   <- ldply(allModelsResults, function(x) as.data.frame(
  t(coef(summary(x))[, "Pr(>|t|)"]))) 

names(dfStdErrors) <- paste("se", names(dfStdErrors), sep=".")
names(dftValues) <- paste("t", names(dftValues), sep=".")
names(dfpValues) <- paste("p", names(dfpValues), sep=".")

calcPval <- function(x){
  fstat <- summary(x)$fstatistic
  pVal <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
  return(pVal)
}

NoOfCoef <- unlist(apply(regMat, 1, sum))
R2 <- unlist(lapply(allModelsResults, function(x) summary(x)$r.squared))
adjR2 <- unlist(lapply(allModelsResults, function(x) summary(x)$adj.r.squared))
RMSE <- unlist(lapply(allModelsResults, function(x) summary(x)$sigma))
fstats <- unlist(lapply(allModelsResults, calcPval))

results <- data.frame( model = as.character(allModelsList),
                       NoOfCoef = NoOfCoef,
                       dfCoefNum,
                       dfStdErrors,
                       dftValues,
                       dfpValues,
                       R2 = R2,
                       adjR2 = adjR2,
                       RMSE = RMSE,
                       pF = fstats  )

results

results[which.max(results$R2),]

#loop through many random subsets and store them





###############
beta_MLE   <- ldply(allModelsResults, function(x) as.data.frame(
  t(coef(x))))
#dfStdErrors <- ldply(allModelsResults, function(x) as.data.frame(
#  t(coef(summary(x))[, "Std. Error"])))
#dftValues   <- ldply(allModelsResults, function(x) as.data.frame(
#  t(coef(summary(x))[, "t value"])))
#dfpValues   <- ldply(allModelsResults, function(x) as.data.frame(
#  t(coef(summary(x))[, "Pr(>|t|)"]))) 

#names(dfStdErrors) <- paste("se", names(dfStdErrors), sep=".")
#names(dftValues) <- paste("t", names(dftValues), sep=".")
#names(dfpValues) <- paste("p", names(dfpValues), sep=".")

beta_MLE[is.na(beta_MLE)] <- 0 #replace all NAs by 0
beta_MLE <- beta_MLE[,c(3:7)] #remove intercept column
colnames(beta_MLE) <- c('beta1', 'beta2', 'beta3', 'beta4', 'beta5')
head(beta_MLE)

y_hat <- matrix(0, nrow=n/2, ncol=nrow(beta_MLE))
for (i in 1:nrow(beta_MLE)){
  y_hat[,i] <- as.matrix(X)%*%t(beta_MLE[i,])
}
head(y_hat)

#loss <- (1/nrow(X.test.set))*sum((y_hat-y.test.set)^2)  
loss <- colSums(apply(y_hat, 2, function(x) (x-y.test.set)^2))
plot(loss, type="l")
allModelsList[which.min(loss)]