
dr.pkg <- c("FactoMineR", "factoextra", "corrplot", "vegan")
# install.packages(dr.pkg)
lapply(dr.pkg, library, character.only=T)

## PCA for land
land <- ld2[4:12]
pca.land <- PCA(land)
fviz_eig(pca.land)
pca.land$eig
# comp1~3 are important

## ISOMAP for land
iso.land <- isomap(dist(land), ndim = 2, k = 5, fragmentedOK = T)
plot(iso.land)
