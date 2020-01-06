## dataC ==========================================================
# Dealing with missing-------------------------------------------
ca.comp <- imputePCA(dataC[, c(10:15)], ncp=2, row.w = dataC$weight)
cb.comp <- imputePCA(dataC[, c(16:24)], ncp=2, row.w = dataC$weight)
cc.comp <- imputePCA(dataC[, c(25:33)], ncp=2, row.w = dataC$weight)

# Kmeans Cluster Analysis----------------------------------------
# Cluster a => 5 cluster
ca.Nb <- NbClust(ca.comp$completeObs, distance = "euclidean", 
                 min.nc=2, max.nc=5, method = "kmeans",index = "sdbw")
ca.Nb$All.index
ca.Nb$Best.nc
set.seed(123)
ca.kmeans <- kmeans(ca.comp$completeObs, 5, 20)

# Cluster b => 5 cluster
cb.Nb <- NbClust(cb.comp, distance = "euclidean", 
                 min.nc=2, max.nc=5, method = "kmeans",index = "sdbw")
cb.Nb$All.index
cb.Nb$Best.nc
set.seed(123)
cb.kmeans <- kmeans(cb.comp$completeObs, 5, 20)

# Cluster c => 5 cluster
cc.Nb <- NbClust(cc.comp$completeObs, distance = "euclidean", 
                 min.nc=2, max.nc=5, method = "kmeans", index = "sdbw")
cc.Nb$All.index
cc.Nb$Best.nc
set.seed(123)
cc.kmeans <- kmeans(cc.comp$completeObs, 5, 20)
