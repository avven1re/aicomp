# Dealing with missing-------------------------------------------
a.comp <- imputePCA(dataC[, c(64:69)], ncp=2, row.w = dataC$weight)
b.comp <- imputePCA(dataC[, c(70:78)], ncp=2, row.w = dataC$weight)
c.comp <- imputePCA(dataC[, c(79:87)], ncp=2, row.w = dataC$weight)

# Kmeans Cluster Analysis----------------------------------------
# Cluster a => 4 cluster
a.Nb <- NbClust(a.comp$completeObs, distance = "euclidean", 
                min.nc=2, max.nc=10, method = "kmeans", 
                index = "dunn")
a.Nb$All.index #Max
a.kmeans <- kmeans(a.comp$completeObs, 7, 20)

# Cluster b => 3 cluster
b.Nb <- NbClust(b.comp$completeObs, distance = "euclidean", 
                min.nc=2, max.nc=10, method = "kmeans", 
                index = "sdbw")
fviz_nbclust(b.comp$completeObs, kmeans, method = "wss", 
             k.max = 10) + theme_minimal()
b.Nb$All.index #Max
b.kmeans <- kmeans(b.comp$completeObs, 3, 20)

# Cluster c => 4 cluster
c.Nb <- NbClust(c.comp$completeObs, distance = "euclidean", 
                min.nc=2, max.nc=10, method = "kmeans", index = "sdbw")
c.Nb$All.index #Min
c.kmeans <- kmeans(c.comp$completeObs, 3, 20)

# Cluster Land => 4 cluster
land.Nb <- NbClust(dataC[, 1:63], distance = "euclidean", 
                   min.nc=11, max.nc=20, method = "complete", index = "sdbw")
land.Nb$All.index
land.kmeans <- kmeans(c.comp$completeObs, 4, 20)


# Dimension Reduction -------------------------------------------
# Reduce a => 3 principal component (65%) & SIR
# S1:worried and depressed
# S2:Social contact and interaction dissatisfaction
# S3:Not worried but depressed
a.pca <- PCA(a.comp$completeObs, ncp=3, row.w = dataC$weight)
get_eigenvalue(a.pca) #Variance
par(mfrow=c(1, 2))
plot(a.pca$ind$coord[, 1], a.pca$ind$coord[, 2], 
     xlab="PCA-1", ylab="PCA-2", col=a.kmeans$cluster, main = "dataC_a's PCA")
a.comp.df <- as.data.frame(a.comp$completeObs)
a.sir <- dr(a.kmeans$cluster ~ ., data=a.comp.df, 
            nslices=4, chi2approx="wood", method="sir")
summary(a.sir)
a.sir.comp <- as.matrix(a.comp$completeObs %*% as.matrix(a.sir$evectors[,1:2]))
plot(a.sir.comp, col=a.kmeans$cluster, main = "dataC_a's SIR")

# Reduce b => 2 principal component (61%) & SIR
# S1:Dissatisfaction at all levels of daily life
# S2:Could be satisfied with the safety and living environment
b.pca <- PCA(b.comp$completeObs, ncp=2, row.w = dataC$weight)
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
c.pca <- PCA(c.comp$completeObs, ncp=3, row.w = dataC$weight)
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
