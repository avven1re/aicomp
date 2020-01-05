## dataB ==========================================================
# Dealing with missing-------------------------------------------
a.comp <- imputePCA(dataB[, c(64:69)], ncp=2, row.w = dataB$weight)
b.comp <- imputePCA(dataB[, c(70:78)], ncp=2, row.w = dataB$weight)
c.comp <- imputePCA(dataB[, c(79:87)], ncp=2, row.w = dataB$weight)

# Kmeans Cluster Analysis----------------------------------------
# Cluster a => 2 cluster
a.Nb <- NbClust(a.comp$completeObs, distance = "euclidean", 
                min.nc=2, max.nc=10, method = "kmeans")
# a.Nb$All.index #Max
a.kmeans <- kmeans(a.comp$completeObs, 2)

# Cluster b => 2 cluster
fviz_nbclust(b.comp$completeObs, kmeans, method = "wss", 
             k.max = 10) + theme_minimal()
b.Nb <- NbClust(b.comp$completeObs, distance = "euclidean", 
                min.nc=2, max.nc=10, method = "kmeans")
# b.Nb$All.index #Min

b.kmeans <- kmeans(b.comp$completeObs, 2)

# Cluster c => 2 cluster
c.Nb <- NbClust(c.comp$completeObs, distance = "euclidean", 
                min.nc=2, max.nc=10, method = "kmeans")
# c.Nb$All.index #Min
c.kmeans <- kmeans(c.comp$completeObs,2)

# Cluster Land => 4 cluster
land.Nb <- NbClust(dataB[, 1:63], distance = "euclidean",
                   min.nc=2, max.nc=10, method = "complete")
land.Nb$All.index
land.kmeans <- kmeans(c.comp$completeObs, 2, 20)


# Dimension Reduction -------------------------------------------
# Reduce a => 3 principal component (65%) & SIR
# S1:worried and depressed
# S2:Social contact and interaction dissatisfaction
# S3:Not worried but depressed
a.pca <- PCA(a.comp$completeObs, ncp=3, row.w = dataB$weight)
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
b.pca <- PCA(b.comp$completeObs, ncp=2, row.w = dataB$weight)
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
c.pca <- PCA(c.comp$completeObs, ncp=3, row.w = dataB$weight)
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

