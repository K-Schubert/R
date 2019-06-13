rm(list=ls())
setwd("/Users/kieranschubert/Desktop/Statistics/2nd_Semester/Machine Learning/ML")

RMSE <- function(error) {
  sqrt(mean(error^2))
}

trn <- read.csv("trainingdata.csv", header=T)
test <- read.csv("test_predictors.csv", header=T)

#OF <- function (x, y) cor(x, y) ^ 2


#GENETIC ALGORITHM
FE <- 100 # max nb of fct evaluations
popSize <- 100
maxGen <- FE/popSize #nb of generations 

pXO <- 0.5 #probability that a bit is inherited from parent 1
pMut <- 0.01 #mutation probability

N <- ncol(trn) - 1 
X <- trn[,-1]
y <- trn[,1]
  
maxfeat <- 5:10

GA <- as.data.frame(matrix(0, nrow=6, ncol=4))

for (feat in 1:length(maxfeat)){

  # Initialisation

  #parent population
  x <- matrix(0, nrow=popSize, ncol=N) 
  for (i in 1:popSize){
    x[i, sample(N, maxfeat, replace=F)]=1
  }
  
  x <- (x == 1)
  fit <- matrix(0, nrow=popSize, ncol=1)
  fitN <- fit
  
  for (i in 1:popSize){
    mod <- lm(paste("y~",paste(colnames(X[x[i,]]), collapse = "+"), sep = ""), data=X)
    fit[i] <- summary(mod)$r.squared
    #fit[i] <- OF(X[, x[i,]], y) 
  }
  
  iEl <- which.max(fit)
  fitEl <- max(fit)
  xEl <- x[iEl,]
  
  # Run
  
  for (gen in 2:maxGen){
  
    #XO
    parent2 <- sample(popSize, popSize, replace=F) #find out who is parent2
    xN <- x[parent2,] #offspring inherits features of parent2
    XO <- matrix(runif(popSize*N), nrow=popSize, ncol=N) < pXO #which bits should be inherited from parent1
    xN[XO] <- x[XO] #replace elements with features of parent1 (parent1=1:popSize)
    
    #mutation
    mut <- matrix(runif(popSize*N), nrow=popSize, ncol=N) < pMut #choose bits to mutate
    xN[mut] = !xN[mut] #perform mutation
    
    #evaluate
    for (i in 1:popSize){
      punish <- (-1)*(sum(xN[i,])>maxfeat[feat])
      
      if (sum(xN[i,]) < maxfeat[feat]){
        xN[i, sample(N, maxfeat, replace=F)] <- TRUE
      }
      
      mod <- lm(paste("y~",paste(colnames(X[xN[i,]]), collapse = "+"), sep = ""), data=X)
      fitN[i] <- summary(mod)$r.squared + punish
      #fitN[i] <- OF(X[,xN[i,]], y)
    }
    
    #new population
    improved <- fitN > fit #tournament between offspring i and parent i
    x[improved,] <- xN[improved,]
    fit[improved] <- fitN[improved]
    
    iEl <- which.max(fitN)
    fitNEl <- max(fitN)
    
    if (fitNEl > fitEl){
      xEl <- xN[iEl,]
      fitEl <- fitNEl
    }
  
  }
  
  # Report result
  paste(c("Best subset: ", colnames(trn)[xEl]), collapse=" ")
  paste(c("Best R2: ", fitEl), collapse=" ")
  mod.fit <- summary(lm(y~as.matrix(X[,xEl])))

  GA[feat,1] <- maxfeat[feat]
  GA[feat,2] <- paste(colnames(trn)[xEl], collapse="+")
  GA[feat,3] <- fitEl
  GA[feat,4] <- RMSE(mod.fit$residuals)
}

GA <- cbind(model = seq(1:6), GA)

colnames(GA) <- c("model", "maxfeat", "selected features", "R^2", "RMSE")

#table of GA
library(knitr)
kable(GA, caption="GA Subset Selection")


# Make csv prediction
mod_75 <- lm(paste("y~", GA[5,3], sep=""), data=trn)
pred_GA <- predict(mod_75, test)
tibble(ID = 1:350, y = predict(mod_75, newdata = test))


#TABLE COMPARISON SUBSET SELECTION

table <- as.data.frame(matrix(0, nrow=9, ncol=5))
colnames(table) <- c("Model", "CV (Y/N)", "#p", "RMSE train", "Score")
table[1,1] <- "Lasso"
table[1,2] <- "Y"
table[1,3] <- 8
table[1,4] <- 72.17375
table[1,5] <- 88.75

table[2,1] <- "GA_5"
table[2,2] <- "N"
table[2,3] <- 5
table[2,4] <- 88.53136
table[2,5] <- NA

table[3,1] <- "GA_10"
table[3,2] <- "N"
table[3,3] <- 10
table[3,4] <- 86.70488
table[3,5] <- NA

table[4,1] <- "GA_20"
table[4,2] <- "N"
table[4,3] <- 20
table[4,4] <- 80.49352
table[4,5] <- NA

table[5,1] <- "GA_50"
table[5,2] <- "N"
table[5,3] <- 50
table[5,4] <- 75.95641
table[5,5] <- NA

table[6,1] <- "GA_75"
table[6,2] <- "N"
table[6,3] <- 75
table[6,4] <- 73.80047
table[6,5] <- NA

table[7,1] <- "GA_100"
table[7,2] <- "N"
table[7,3] <- 100
table[7,4] <- 73.83969
table[7,5] <- 156.52481

table[8,1] <- "kNN"
table[8,2] <- "Y"
table[8,3] <- 111
table[8,4] <- 155.0958
table[8,5] <- 122.32133

table[9,1] <- "Tree"
table[9,2] <- "Y"
table[9,3] <- 11
table[9,4] <- 73.97789
table[9,5] <- 92.24640

table

## GA CV
set.seed(42)
k <- 10

cv <- function(trn, k){
  trn_shuffled <- trn[sample(nrow(trn)),]
  folds <- cut(seq(1,nrow(trn_shuffled)),breaks=k,labels=FALSE)
  CV_MSE <- rep(0, 6)
  for (mod in 1:6){
    MSE <- rep(0, k)
    for (i in 1:k){
      test_ind <- which(folds==i,arr.ind=TRUE)
      testData <- trn_shuffled[test_ind, ]
      trainData <- trn_shuffled[-test_ind, ]
      fit <- lm(paste("y~", GA[mod,3], sep=""), data=trainData)
      pred <- predict(fit, testData)
      MSE[i] <- (k/nrow(trn))*sum((pred-testData$y)^2)
    }
    CV_MSE[mod] <- (1/k)*sum(MSE)
    SE[mod] <- (1/sqrt(k))*sqrt(sum((MSE-CV_MSE[mod])^2)/(k-1))
  }
  list(MSE, CV_MSE, SE)
}

#MSE <- cv(trn, k)[[1]]
CV_MSE <- cv(trn, k)[[2]]
SE <- cv(trn, k)[[3]]
plot(sqrt(CV_MSE), ylim=c(60, 250), col="red", ylab="RMSE", xlab="GA Model", main="RMSE of GA models", pch=15)
points(GA$RMSE, col="blue", pch=17)
legend("topright", c("CV_RMSE", "Train_RMSE"), col=c("red","blue"), lwd=1, lty=c(0,0), 
       pch=c(15,17), cex=0.75)
segments(1:6, sqrt(CV_MSE)-sqrt(SE), 1:6, sqrt(CV_MSE)+sqrt(SE), col="red")

sqrt(CV_MSE) - (sqrt(CV_MSE[6])+sqrt(SE[6]))

#plot(seq(k), MSE)
#segments(seq(k), MSE-SE, seq(k), MSE+SE, col="red")
#lines(MSE+SE, col="red")
#lines(MSE-SE, col="red")


#TABLE OF VARIABLES
RF <- c("X34", "X35", "X59", "X60", "X62", "X68", "X75", "X104")

feature_occurence <- c(variables[-1], unlist(strsplit(GA[1,3], "\\+")), unlist(strsplit(GA[2,3], "\\+")),
                       unlist(strsplit(GA[3,3], "\\+")), unlist(strsplit(GA[4,3], "\\+")),
                       unlist(strsplit(GA[5,3], "\\+")), unlist(strsplit(GA[6,3], "\\+")), RF)

table(feature_occurence)
freq_vars <- names(sort(table(feature_occurence), decreasing=T)[1:10])
barplot(sort(table(feature_occurence), decreasing=T)[1:10], names.arg=names(sort(table(feature_occurence), decreasing=T)[1:10][1:10]), main="Feature Occurence in Subsets", las=2)

#DESCRIPTIVE STATS
var <- paste("Var    :", round(sapply(trn[freq_vars], var), 4))
#var <- round(sapply(trn[freq_vars], var), 6)

summary <- rbind(summary(trn[freq_vars]),var)
summary

# LASSO
grid <- 10^seq(10, -2, length = 100)
lasso_caret <- caret::train(X,y, method = "glmnet", lambda= 0,
                            tuneGrid = expand.grid(alpha = 1,  lambda = grid))
cv.lasso <- cv.glmnet(data.matrix(X), y, alpha = 1, nfolds = 10)
co <- coef(cv.lasso,s = "lambda.1se")
inds <- which(co!=0)
variables <- row.names(co)[inds]








## Lasso regression ----

lasso_fit <- caret::train(
  x = train_tr %>% select(-y),
  y = train_tr$y, 
  y ~ x, 
  method = "glmnet", 
  metric = "RMSE",
  tuneGrid=expand.grid(
    .alpha=1,
    .lambda=seq(0, 100, by = 0.1))
  alpha=1)


### Lasso regression
grid <- 10^seq(10, -2, length = 100)
lasso.mod <- glmnet(data.matrix(trn[,-1]), y, alpha = 1, lambda = grid,
                    standardize = TRUE)

### Print results
pdf(file = "Plot/lasso_1.pdf")
par(mfrow = c(1,2))
plot(lasso.mod)
cv.lasso <- cv.glmnet(data.matrix(trn[,-1]), y, alpha = 1, nfolds = 10)
plot(cv.lasso)
dev.off(9)



co <- coef(cv.lasso,s = "lambda.1se")
inds <- which(co!=0)
variables <- row.names(co)[inds]

### fit Optimal lambda in lasso model
opt_lambda <- cv.lasso$lambda.1se
opt_lambda 
fit <- cv.lasso$glmnet.fit
y_predicted <- predict(fit, s = opt_lambda, newx = data.matrix(train_tr[,-1]))

### compute the Root MSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse / sst

## Save the predictions
pred_lasso_1 <- 
  tibble(ID = 1:350, y = predict(fit, s = opt_lambda, 
                                 newx = data.matrix(test_tr[, -1])))
write.csv(pred_lasso_1, "Predictions/pred_lasso_1.csv", row.names = FALSE)
















