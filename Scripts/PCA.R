#Separate numerical and categorical variables on TRN
data <- as.data.frame(train_tr[,-1])
num_ind <- unlist(lapply(data, is.numeric))  
numerical <- as.matrix(data[, num_ind])
categorical <- data[, !num_ind]

# Do principal component analysis
dat <- scale(numerical, center = T, scale = F)
n <- NROW(dat)
p <- NCOL(dat)
pca <- svd(dat)
A_trn <- pca$u %*% diag(pca$d)
H_trn <- t(pca$v)

# Look at the variance explained by the first q components, for some q < p
q <- 60
lambdas <- pca$d^2
barplot(lambdas[1:q]/sum(lambdas), names.arg = 1:q)
pdf("Pict/PCA_cumsum.pdf", height=5, width=10)
barplot(cumsum(lambdas[1:q]/sum(lambdas)), names.arg = 1:q, ylim = c(0, 1), xlab="Principal Components", ylab="Proportion of variance explained")
abline(h = 1, col = 'red')
abline(h = 0.8, col = 'green') #Use first 40 PCs 
dev.off()

# Recombine approximated numerical dataset with categorical variables (TRN)
pca_trn <- cbind(A_trn[,1:40] %*% H_trn[1:40,], categorical) #used first 40 PCs

#Separate numerical and categorical variables on TEST
data <- as.data.frame(test_tr[,-1])
num_ind <- unlist(lapply(data, is.numeric))  
numerical <- as.matrix(data[, num_ind])
categorical <- data[, !num_ind]

# Extract A matrix on TEST
dat <- scale(numerical, center = T, scale = F)
n <- NROW(dat)
p <- NCOL(dat)
pca <- svd(dat)
A_test <- pca$u %*% diag(pca$d) #only recompute A on TEST

# Recombine approximated numerical dataset with categorical variables (TEST)
pca_test <- cbind(A_test[,1:40] %*% H_trn[1:40,], categorical) #use A (scores) from TEST with H (PCs) from TRN

