mod1.gam <- gam(CRA8132~s(pH, k=4)+s(LL, k=4)+s(Temp, k=4)+s(Brix, k=4),
                family=binomial,
                data=orange)
summary(mod1.gam)
plot(mod1.gam,cex=3,residuals=T,pages=1)
gam.check(mod1.gam)
#suggests quadratic fit for Brix

mod2.gam <- gam(CRA8132~s(pH, k=4)+s(LL, k=4)+s(Temp, k=4)+s(Brix, k=4),
                family=binomial,
                data=orange.omit)
summary(mod2.gam)
plot(mod2.gam,cex=3,residuals=T,pages=1)
gam.check(mod2.gam)
#doesn't change

fit.quad <- glm(CRA8132~pH+LL+Temp+I(Brix^2), family=binomial, data=orange)
summary(fit.quad)

#normal diagnostics
plot(fit.quad)






pH_LL <- orange$pH*orange$LL
pH_Temp <- orange$pH*orange$Temp
pH_Brix <- orange$pH*orange$Brix
LL_Temp <- orange$LL*orange$Temp
LL_Brix <- orange$LL*orange$Brix
Temp_Brix <- orange$Temp*orange$Brix
Brix2 <- orange$Brix^2

orange.aug <- cbind(orange, pH_LL, pH_Temp, pH_Brix, LL_Temp, LL_Brix, Temp_Brix, Brix2)
regressors <- colnames(orange.aug)[-5]
regMat <- expand.grid(c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE), 
                      c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE))

regMat <- regMat[-(dim(regMat)[1]),] #The last row is the null model so we remove it
names(regMat) <- regressors

allModelsList <- apply(regMat, 1, function(x) as.formula(paste(c("CRA8132~1", regressors[x]), collapse="+")))

allModelsResults <- lapply(allModelsList, function(x) glm(x, data=orange, family=binomial))

library(plyr)
aic <- ldply(allModelsResults, function(x) as.data.frame(
  t(AIC(x))))
plot(aic$V1, type="o")
a <- sort(aic$V1, index.return=T)
a$ix[a$x>35]

