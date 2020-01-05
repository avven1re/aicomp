library(mice)
library(VIM) 
dataA.aggrplot<-aggr(dataA, col=c('lightblue','red'), 
                      numbers=TRUE, prop = TRUE, sortVars=TRUE, 
                      labels=names(dataA), cex.axis=.7, gap=3)
# q21 為 missing by design
# missing at random < 10%, do not need to impute values
dataB.aggrplot<-aggr(dataB, col=c('lightblue','red'), 
                     numbers=TRUE, prop = TRUE, sortVars=TRUE, 
                     labels=names(dataB), cex.axis=.7, gap=3)
dataC.aggrplot<-aggr(dataC, col=c('lightblue','red'), 
                     numbers=TRUE, prop = TRUE, sortVars=TRUE, 
                     labels=names(dataA), cex.axis=.7, gap=3)