## packages=======================================================
pkg <- c("sf", "gridExtra", "dplyr", "FactoMineR", "factoextra", 
         "corrplot", "vegan","missMDA", "stats", "NbClust", "dr")
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
land=read.csv("dataset/landdata.csv", skip=1, header=F, 
             fileEncoding = "UTF-8-BOM")
names(land) <- c("county","town","area", paste('l',1:63,sep=''))
for (i in 1:dim(land)[1]){
  for (j in 4:dim(land)[2]){
    land[i,j] <- 1000*land[i,j]/land[i,3]
  }
}

 # Input Shapefile------------------------------------------------
taiwan.town.map <- st_read("dataset/town/TOWN_MOI_1070205.shp")
ntw.map <- as.data.frame(
  taiwan.town.map[c("COUNTYNAME","TOWNNAME","TOWNCODE")])
ntw.map <- ntw.map[,1:3]

## Merging and Sampling Data ======================================
 # Merge landdata & Shapefile & Happiness-------------------------
truemap <- left_join(ntw.map, land,
                     by= c("COUNTYNAME"="county","TOWNNAME"="town"))
dataA <- merge(truemap, ques, by = "TOWNCODE", all.ques=T)
dataA <- dataA[, c(-(1:4))]

 # Sampling ABC---------------------------------------------------
 # dim(dataA)
 # head(dataA)
set.seed(100)
dataB = dataA[sample(nrow(dataA),100),]
 # head(dataB)
set.seed(200)
dataC = dataA[sample(nrow(dataA),33),]
 # head(dataC)

## dataA ==========================================================
 # Dealing with missing-------------------------------------------
a.comp <- imputePCA(dataA[, c(64:69)], ncp=2, row.w = dataA$weight)
b.comp <- imputePCA(dataA[, c(70:78)], ncp=2, row.w = dataA$weight)
c.comp <- imputePCA(dataA[, c(79:87)], ncp=2, row.w = dataA$weight)

 # Kmeans Cluster Analysis----------------------------------------
 # Cluster a => 4 cluster
a.Nb <- NbClust(a.comp$completeObs, distance = "euclidean", 
                min.nc=2, max.nc=10, method = "kmeans", 
                index = "dunn")
a.Nb$All.index #Max
set.seed(123)
a.kmeans <- kmeans(a.comp$completeObs, 4, 20)

 # Cluster b => 3 cluster
fviz_nbclust(b.comp$completeObs, kmeans, method = "wss", 
             k.max = 10) + theme_minimal()
set.seed(123)
b.kmeans <- kmeans(b.comp$completeObs, 3, 20)

 # Cluster c => 4 cluster
c.Nb <- NbClust(c.comp$completeObs, distance = "euclidean", 
                min.nc=2, max.nc=10, method = "kmeans", index = "sdbw")
c.Nb$All.index #Min
set.seed(123)
c.kmeans <- kmeans(c.comp$completeObs, 4, 20)

 # Cluster Land => 7 cluster
land.Nb <- NbClust(dataA[, 1:63], distance = "euclidean", 
                  min.nc=1, max.nc=10, method = "complete", index = "sdbw")
land.Nb$All.index
set.seed(123)
land.kmeans <- kmeans(dataA[, 1:63], 7, 50)
 # plot(land.kmeans$centers[1,], type = "b", ylim=c(0,1000), xlim=c(0,65)) #black 
 # points(land.kmeans$centers[2,], col=2, type = "b") #red 38 40 45 46 63 工業住宅
 # points(land.kmeans$centers[3,], col=3, type = "b") #green
 # points(land.kmeans$centers[4,], col=4, type = "b") #blue 西部平原農業區 1 2 22 32 38 40 60 61 63 
 # points(land.kmeans$centers[5,], col=5, type = "b") #lblue 11 丘陵
 # points(land.kmeans$centers[6,], col=6, type = "b") #purple 9 22 全台山地
 # points(land.kmeans$centers[7,], col=7, type = "b") #gray 24 35 53 南部濱海
 # test <- cbind(dataA, land.kmeans$cluster)
 # test[land.kmeans$cluster==1, 2:3]

 # Dimension Reduction -------------------------------------------
 # Reduce a => 3 principal component (65%) & SIR
  # S1:worried and depressed
  # S2:Social contact and interaction dissatisfaction
  # S3:Not worried but depressed
a.pca <- PCA(a.comp$completeObs, ncp=3, row.w = dataA$weight)
get_eigenvalue(a.pca) #Variance
par(mfrow=c(1, 2))
plot(a.pca$ind$coord[, 1], a.pca$ind$coord[, 2], 
     xlab="PCA-1", ylab="PCA-2", col=a.kmeans$cluster)
a.comp.df <- as.data.frame(a.comp$completeObs)
a.sir <- dr(a.kmeans$cluster ~ ., data=a.comp.df, 
            nslices=4, chi2approx="wood", method="sir")
summary(a.sir)
a.sir.comp <- as.matrix(a.comp$completeObs %*% as.matrix(a.sir$evectors[,1:2]))
plot(a.sir.comp, col=a.kmeans$cluster)

 # Reduce b => 2 principal component (61%) & SIR
  # S1:Dissatisfaction at all levels of daily life
  # S2:Could be satisfied with the safety and living environment
b.pca <- PCA(b.comp$completeObs, ncp=2, row.w = dataA$weight)
get_eigenvalue(b.pca) #Variance
par(mfrow=c(1, 2))
plot(b.pca$ind$coord[, 1], b.pca$ind$coord[, 2], 
     xlab="PCA-1", ylab="PCA-2", col=b.kmeans$cluster)
b.comp.df <- as.data.frame(b.comp$completeObs)
b.sir <- dr(b.kmeans$cluster ~ ., data=b.comp.df, 
            nslices=3, chi2approx="wood", method="sir")
summary(b.sir)
b.sir.comp <- as.matrix(b.comp$completeObs %*% as.matrix(b.sir$evectors[,1:2]))
plot(b.sir.comp, col=b.kmeans$cluster)

 # Reduce c => 3 principal component (63%) & SIR
  # S1:Overall trust in government agencies or institutions
  # S2:Supporting to free speech and dissatisfaction of political officials
  # S3:Distrust the central government and trust the local governments
c.pca <- PCA(c.comp$completeObs, ncp=3, row.w = dataA$weight)
get_eigenvalue(c.pca) #Variance
par(mfrow=c(1, 2))
plot(c.pca$ind$coord[, 1], c.pca$ind$coord[, 2], 
     xlab="PCA-1", ylab="PCA-2", col=c.kmeans$cluster)
c.comp.df <- as.data.frame(c.comp$completeObs)
c.sir <- dr(c.kmeans$cluster ~ ., data=c.comp.df, 
            nslices=4, chi2approx="wood", method="sir")
summary(c.sir)
c.sir.comp <- as.matrix(c.comp$completeObs %*% as.matrix(c.sir$evectors[,1:3]))
plot(c.sir.comp[,1:2], col=c.kmeans$cluster)

 # Reduce land => 2 principal component (26%) & ISOMAP
  # ISO1:Altitude
  # ISO2:Housing to agriculture
land.pca <- PCA(dataA[, 1:63], ncp=2, row.w = dataA$weight)
get_eigenvalue(land.pca) #Variance
par(mfrow=c(1, 2))
plot(land.pca$ind$coord[, 1], land.pca$ind$coord[, 2], 
     xlab="PCA-1", ylab="PCA-2", col=land.kmeans$cluster)
land.isomap <- isomap(dist(dataA[, 1:63]), ndim=2, k=75)
plot(land.isomap, col=land.kmeans$cluster)