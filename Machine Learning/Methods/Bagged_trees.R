Heart <- read.csv("/Users/kieranschubert/Desktop/Statistics/2nd_Semester/Machine_Learning/ML/Datasets/Heart.csv")
set.seed(1)
par(mfrow=c(1,3))
for (b in 1:3){
  HeartB <- Heart[sample(1:nrow(Heart), size=nrow(Heart), replace=T),]
  treeB <- tree(AHD~., data=HeartB, split="deviance", control=tree.control(nobs=nrow(Heart), minsize=20, mindev=0))
  plot(treeB)
  text(treeB)
}
