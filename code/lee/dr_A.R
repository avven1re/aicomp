## dataA ==========================================================
# Dealing with missing-------------------------------------------
aa.comp <- imputePCA(dataA[, c(10:15)], ncp=2, row.w = dataA$weight)
ab.comp <- imputePCA(dataA[, c(16:24)], ncp=2, row.w = dataA$weight)
ac.comp <- imputePCA(dataA[, c(25:33)], ncp=2, row.w = dataA$weight)

# Kmeans Cluster Analysis----------------------------------------
# Cluster a => 2 cluster
aa.Nb <- NbClust(aa.comp$completeObs, distance = "euclidean", 
                min.nc=2, max.nc=5, method = "kmeans",index = "sdbw")
aa.Nb$All.index
aa.Nb$Best.nc
set.seed(123)
aa.kmeans <- kmeans(aa.comp$completeObs, 2, 20)

# Cluster b => 2 cluster
# fviz_nbclust(b.comp$completeObs, kmeans, method = "wss", 
#              k.max = 10) + theme_minimal()
ab.Nb <- NbClust(ab.comp$completeObs, distance = "euclidean", 
                min.nc=2, max.nc=5, method = "kmeans",index = "sdbw")
ab.Nb$All.index
ab.Nb$Best.nc
set.seed(123)
ab.kmeans <- kmeans(ab.comp$completeObs, 2, 20)

# Cluster c => 2 cluster
ac.Nb <- NbClust(ac.comp$completeObs, distance = "euclidean", 
                min.nc=2, max.nc=5, method = "kmeans", index = "sdbw")
ac.Nb$All.index
ac.Nb$Best.nc
set.seed(123)
ac.kmeans <- kmeans(ac.comp$completeObs, 4, 20)

# Cluster Land => 7 cluster
# land.Nb <- NbClust(dataA[, 1:63], distance = "euclidean",
#                    min.nc=2, max.nc=10, method = "kmeans", index="sdbw")
# land.Nb$All.index
# set.seed(123)
# land.kmeans <- kmeans(c.comp$completeObs, 7, 20)

# Dimension Reduction -------------------------------------------
# Reduce a => 3 principal component (65%) & SIR
# S1:worried and depressed
# S2:Social contact and interaction dissatisfaction
# S3:Not worried but depressed
a.pca <- PCA(a.comp$completeObs, ncp=2, row.w = dataA$weight)
get_eigenvalue(a.pca) #Variance
par(mfrow=c(1, 2))
plot(a.pca$ind$coord[, 1], a.pca$ind$coord[, 2], 
     xlab="PCA-1", ylab="PCA-2", col=a.kmeans$cluster, main="dataA_a's PCA")
a.comp.df <- as.data.frame(a.comp$completeObs)
a.sir <- dr(a.kmeans$cluster ~ ., data=a.comp.df, 
            nslices=4, chi2approx="wood", method="sir")
summary(a.sir)
a.sir.comp <- as.matrix(a.comp$completeObs %*% as.matrix(a.sir$evectors[,1:2]))
plot(a.sir.comp, col=a.kmeans$cluster, main="dataA_a's SIR")

# Reduce b => 2 principal component (61%) & SIR
# S1:Dissatisfaction at all levels of daily life
# S2:Could be satisfied with the safety and living environment
b.pca <- PCA(b.comp$completeObs, ncp=2, row.w = dataA$weight)
get_eigenvalue(b.pca) #Variance
par(mfrow=c(1, 2))
plot(b.pca$ind$coord[, 1], b.pca$ind$coord[, 2], 
     xlab="PCA-1", ylab="PCA-2", col=b.kmeans$cluster, main="dataA_b's PCA")
b.comp.df <- as.data.frame(b.comp$completeObs)
b.sir <- dr(b.kmeans$cluster ~ ., data=b.comp.df, 
            nslices=3, chi2approx="wood", method="sir")
# summary(b.sir)
b.sir.comp <- as.matrix(b.comp$completeObs %*% as.matrix(b.sir$evectors[,1:2]))
plot(b.sir.comp, col=b.kmeans$cluster, main="dataA_b's SIR")

# Reduce c => 3 principal component (63%) & SIR
# S1:Overall trust in government agencies or institutions
# S2:Supporting to free speech and dissatisfaction of political officials
# S3:Distrust the central government and trust the local governments
c.pca <- PCA(c.comp$completeObs, ncp=2, row.w = dataA$weight)
get_eigenvalue(c.pca) #Variance
par(mfrow=c(1, 2))
plot(c.pca$ind$coord[, 1], c.pca$ind$coord[, 2], 
     xlab="PCA-1", ylab="PCA-2", col=c.kmeans$cluster, main="dataA_c's PCA")
c.comp.df <- as.data.frame(c.comp$completeObs)
c.sir <- dr(c.kmeans$cluster ~ ., data=c.comp.df, 
            nslices=4, chi2approx="wood", method="sir")
# summary(c.sir)
c.sir.comp <- as.matrix(c.comp$completeObs %*% as.matrix(c.sir$evectors[,1:3]))
plot(c.sir.comp[,1:2], col=c.kmeans$cluster, main="dataA_c's SIR")

# Reduce land => 2 principal component (26%) & ISOMAP
# ISO1:Altitude
# ISO2:Housing to agriculture
land.pca <- PCA(dataA[, 1:63], ncp=2, row.w = dataA$weight)
get_eigenvalue(land.pca) #Variance
par(mfrow=c(1, 2))
plot(land.pca$ind$coord[, 1], land.pca$ind$coord[, 2], 
     xlab="PCA-1", ylab="PCA-2", col=land.kmeans$cluster, main="dataA_land's PCA")
land.isomap <- isomap(dist(dataA[, 1:63]), ndim=2, k=5)
plot(land.isomap, col=land.kmeans$cluster, main="dataA_land's ISOMAP")
