install.packages("randomForest")
library(randomForest)
Heart <- read.csv("/Users/kieranschubert/Desktop/Statistics/2nd_Semester/Machine_Learning/ML/Datasets/Heart.csv")

p <- ncol(Heart) - 2
set.seed(2)
tree.bag <- randomForest(AHD~., data=Heart, mtry=p, importance=T, na.action=na.omit)
tree.RF <- randomForest(AHD~., data=Heart, importance=T, na.action=na.omit)
par(mfrow=c(1,1))
plot(tree.bag$err.rate[,1], type="l", ylim=c(min(tree.RF$err.rate[,1]), max(tree.bag$err.rate[,1])), xlab="Number of Trees", ylab="Error")
lines(tree.RF$err.rate[,1], col="red")
legend("topright", legend=c("OOB: Bagging", "OOB: RF"), col=c("black", "red"), lty=1)
tree.bag$confusion
tree.RF$confusion
