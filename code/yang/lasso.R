#Lasso Method

a1.model <- lm(a.sir.comp[,1] ~ land.isomap$points[,1] + land.isomap$points[,2] + 
                 factor(dataA$marrgp) + factor(dataA$edugp) + factor(dataA$sexgp) + 
                 dataA$agegp + dataA$incogp, weights=dataA$weight) 



dataC
dataC.iv<-as.matrix(cbind(land.isomap$points[,1], land.isomap$points[,2], 
                            factor(dataA$marrgp), factor(dataA$edugp), factor(dataA$sexgp), 
                            dataA$agegp,dataA$incogp))
dataC.dv<-as.matrix(a.sir.comp[,1])
#Cross‐validation to determine optimal value of lambda#
dataC.lasso.cv<-cv.glmnet(dataC.iv, dataC.dv, type.measure="mse", nfolds=10)
plot(dataC.lasso.cv)

library(selectiveInference)
lasso.sigma<-estimateSigma(dataC.iv,dataC.dv)
lasso.beta = coef(dataC.lasso, s=.01543617)[-1]  
#[‐1] leaves out the intercept from inference#
lasso.inference = fixedLassoInf(dataC.iv,dataC.dv,lasso.beta,.01543617,
                                sigma=lasso.sigma$sigmahat)
lasso.inference