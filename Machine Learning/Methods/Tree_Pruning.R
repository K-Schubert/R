install.packages("tree")
library(tree)

#Tree growing
Heart <- read.csv("/Users/kieranschubert/Desktop/Statistics/2nd_Semester/Machine_Learning/ML/Datasets/Heart.csv")
tree.heart1 <- tree(AHD~., data=Heart, split="deviance", control=tree.control(nobs=nrow(Heart), minsize=50, mindev=0))
plot(tree.heart1)
text(tree.heart1)
tree.heart2 <- tree(AHD~., data=Heart, split="deviance", control=tree.control(nobs=nrow(Heart), minsize = 1, mindev=0))
plot(tree.heart2)
text(tree.heart2)

#Tree pruning
tree.full <- tree(AHD~., data=Heart, split="deviance", control=tree.control(nobs=nrow(Heart), minsize = 2, mindev=0))
plot(tree.full)
text(tree.full)
tree.pruned1 <- prune.tree(tree=tree.full, best=10)
plot(tree.pruned1)
text(tree.pruned1)
tree.pruned2 <- prune.tree(tree=tree.full, best=5)
plot(tree.pruned2)
text(tree.pruned2)

#Cost-complexity pruning
tree.full <- tree(AHD~., data=Heart, split="deviance", control=tree.control(nobs=nrow(Heart), minsize = 2, mindev=0))
plot(tree.full)
text(tree.full)
cv.heart <- cv.tree(tree.full, FUN = prune.tree, K=10)
plot(cv.heart)
best.tune <- cv.heart$k[which(cv.heart$dev == min(cv.heart$dev))]
tree.pruned <- prune.tree(tree=tree.full, k=best.tune)
plot(tree.pruned)
text(tree.pruned)
