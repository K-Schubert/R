

OF <- function (x, y) cor(x, y) ^ 2

#GENETIC ALGORITHM
FE <- 100000 # max nb of fct evaluations
popSize <- 1000
maxGen <- FE/popSize #nb of generations 

pXO <- 0.5 #probability that a bit is inherited from parent 1
pMut <- 0.01 #mutation probability

N <- ncol(trn) - 1 
maxfeat=50;
X <- trn[,-1]
y <- trn[,1]

# Initialisation

#parent population
x <- matrix(0, nrow=popSize, ncol=N) 
for (i in 1:popSize){
  x[i, sample(N, maxfeat, replace=F)]=1;
}

x <- (x == 1)
fit <- matrix(0, nrow=popSize, ncol=1)
fitN <- fit

for (i in 1:popSize){
  fit[i] <- OF(X[, x[i,]], y) 
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
  xN[mut] = !xN[mut] #perform mutation: x_i := not(x_i)
  
  #evaluate
  for (i in 1:popSize){
   fitN[i] <- OF(X[,xN[i,]], y)
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
    gen
    fitEl
  }

}

# Report result
paste(c("Best subset: ", colnames(trn)[xEl]), collapse=" ")
paste(c("Best R2: ", fitEl), collapse=" ")
summary(lm(y~as.matrix(trn[,xEl])))
