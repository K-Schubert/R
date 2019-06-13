setwd("/Users/kieranschubert/Desktop/Statistics/2nd_Semester/GLAM/Exam")
install.packages("gee")
library(gee)
install.packages("statmod")
library(statmod)

# Description of data
# Bacteria CRA8132 alters taste of food, want to understand in which conditions 
# it evolves => MODEL growth of bacteria as a function of covariates involved
# in the production process.
# Data is a worst-case scenario (orange juice), known to have high-contamination
# risk. 
# COVARIATES: pH, temperature, soluble solids concentration, Lactococcus Lactis
# RESPONSE: Conditioning on covariates, has the bacteria been growing or not after 2 weeks?

# Import data
orange <- read.table("orange_juice.dat", header=T)
y <- orange$CRA8132

head(orange)
dim(orange) # 74 rows, 5 columns
str(orange) 

# Units of Measure
# pH : pH Index (numeric) 
# LL (Lactococcus Lactis) : Concentration in IU/mL (integer)
# Temp: Temperature in Celsius (integer)
# Brix (Soluble Solids Concentration) : Brix scale (integer)
# CRA8132 (Growth Response) : Y/N (binary) 

#1) EDA
summary(orange)

range(orange$CRA8132) # 0-1
table(orange$CRA8132) # 0: 48 counts, 1: 26 counts

range(orange$pH) # 3.5-5.5
table(orange$pH) # 3.5: 18 counts, 4: 20 counts, 5: 18 counts, 5.5: 18 counts


range(orange$LL) # 0-70
table(orange$LL) # 0: 22 counts, 30: 18 counts, 50: 16 counts, 70: 18 counts

range(orange$Temp) # 25-50
table(orange$Temp) # 25: 18 counts, 35: 18 counts, 43: 20 counts, 50: 18 counts

range(orange$Brix) # 11-19
table(orange$Brix) # 11: 24 counts, 13: 16 counts, 15: 16 counts, 19: 18 counts

# Response vs covariates
plot(y, orange$pH)
plot(y, orange$LL)
plot(y, orange$Temp)
plot(y, orange$Brix)

# covariates vs covariates
plot(orange$pH, orange$LL)
plot(orange$pH, orange$Temp)
plot(orange$pH, orange$Brix)

plot(orange$LL, orange$Temp)
plot(orange$LL, orange$Brix)

plot(orange$Temp, orange$Brix)


# MODELLING
lr1.fit <- glm(CRA8132~., data=orange, family=binomial)
lr1.pred <- predict(lr1.fit, type="response")
plot(lr1.pred, orange$CRA8132, xlab="Fitted values", ylab="CRA8132")

summary(glm(CRA8132~., data=orange, family=binomial))

# Exhaustive search
regressors <- colnames(orange)[-5]
regMat <- expand.grid(c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE))

regMat <- regMat[-(dim(regMat)[1]),] #The last row is the null model so we remove it
names(regMat) <- regressors

allModelsList <- apply(regMat, 1, function(x) as.formula(paste(c("y~1", regressors[x]), collapse="+")))

allModelsResults <- lapply(allModelsList, function(x) glm(x, data=orange, family=binomial))

