## packages=======================================================
pkg <- c("sf", "gridExtra", "dplyr", "FactoMineR", "factoextra", 
         "corrplot", "vegan","missMDA", "stats", "NbClust", "dr", "glmnet")
lapply(pkg, library, character.only=T)

## Reading & Cleaning Data ========================================
# Input Happiness------------------------------------------------
ques = read.csv("dataset/Happiness.csv")
ques <- ques[, -1]
names(ques)[1:40] <- c(paste('q',1:38,sep=''),"marrgp","incogp")
names(ques)[45] <- "TOWNCODE"
keeps <- c(paste('q',5:19,sep=''), paste('q',30:38,sep=''),
           "marrgp", "incogp", "sexgp", "agegp", "edugp", 
           "weight", "TOWNCODE")
ques <- ques[keeps]
names(ques)[1:6] <- c(paste('a',1:6,sep=''))
names(ques)[7:15] <- c(paste('b',1:9,sep=''))
names(ques)[16:24] <- c(paste('c',1:9,sep=''))
ques <- ques[is.na(ques$TOWNCODE) == 0, ]

# Input landdata & Calculate------------------------------------- 
ld1=read.csv("dataset/landdata.csv", skip=1, header=F, 
             fileEncoding = "UTF-8-BOM")
names(ld1) <- c("county","town","area", paste('l',1:63,sep=''))
for (i in 1:dim(ld1)[1]){
  for (j in 4:dim(ld1)[2]){
    ld1[i,j] <- ld1[i,j]/ld1[i,3]
  }
}
ld2 <- cbind(ld1[,1:3], 1000*ld1[,4:66], rowSums(ld1[,4:9]), rowSums(ld1[,10:16]), rowSums(ld1[,17:27])
             , rowSums(ld1[,28:38]), rowSums(ld1[,39:47]), rowSums(ld1[,48:53])
             , rowSums(ld1[,54:57]), rowSums(ld1[,58:60]), rowSums(ld1[,61:66]))
names(ld2) <- c("county","town","area", paste('l',1:63,sep=''),"agri","forest","traffic"
                ,"water","building","gov","leisure","mine","other")

# Input Shapefile------------------------------------------------
taiwan.town.map <- st_read("dataset/town/TOWN_MOI_1070205.shp")
ntw.map <- as.data.frame(
  taiwan.town.map[c("COUNTYNAME","TOWNNAME","TOWNCODE")])
ntw.map <- ntw.map[,1:3]

## Merging and Sampling Data ======================================
# Merge landdata & Shapefile & Happiness-------------------------
truemap <- left_join(ntw.map, ld2,
                     by= c("COUNTYNAME"="county","TOWNNAME"="town"))
dataA <- merge(truemap, ques, by = "TOWNCODE", all.ques=T)
dataA <- dataA[, c(-(1:3))]

# Sampling ABC---------------------------------------------------
# dim(dataA)
# head(dataA)
set.seed(100)
dataB = dataA[sample(nrow(dataA),100),]
# head(dataB)
set.seed(200)
dataC = dataA[sample(nrow(dataA),33),]
# head(dataC)

# install.packages(mice)
library(mice)
# install.packages(VIM)
library(VIM) 
dataA.aggrplot<-aggr(dataA, col=c('lightblue','red'), 
                     numbers=TRUE, prop = TRUE, sortVars=TRUE, 
                     labels=names(dataA), cex.axis=.7, gap=3)
dataB.aggrplot<-aggr(dataB, col=c('lightblue','red'), 
                     numbers=TRUE, prop = TRUE, sortVars=TRUE, 
                     labels=names(dataB), cex.axis=.7, gap=3)
dataC.aggrplot<-aggr(dataC, col=c('lightblue','red'), 
                     numbers=TRUE, prop = TRUE, sortVars=TRUE, 
                     labels=names(dataA), cex.axis=.7, gap=3)

# 
# install.packages("missForest")
library(missForest)
# seed missing values ( 5% )
dataC.mis <- prodNA(dataC[,c(89:91)], noNA = 0.05)
summary(dataC.mis)

# impute missing values, using all parameters as default values
dataC.imp <- missForest(dataC.mis)

# check imputed values
dataC.imp$ximp

# check imputation error
dataC.imp$OOBerror

# comparing actual data accuracy
# dataC.err <- mixError(dataC.imp$ximp, dataC.mis, dataC)
# dataC.err

write.csv(dataC.imp$ximp,file="dataset/dataC_missingvalues.csv",row.names = FALSE)
