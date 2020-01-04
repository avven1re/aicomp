# encoding : UTF-8
#Land
r_ld2_d <- as.data.frame(ld2[, 3 : 66])

r_ld2 <- princomp(r_ld2_d)

biplot(r_ld2)

barplot(r_ld2$scores)

#install.packages("FactoMineR")
library(FactoMineR)
#install.packages("factoextra")
library(factoextra)
#install.packages(corrplot)
library(corrplot)
library(ggplot2)


r_ld2 <- PCA(r_ld2_d)

fviz_eig(r_ld2)

#PCA
#Question1
r_ques1_d <- as.data.frame(ques[, 7:11])
r_ques1 <- PCA(r_ques1_d)
fviz_eig(r_ques1)

#Question2
r_ques2_d <- as.data.frame(ques[, 12 : 20])
r_ques2 <- PCA(r_ques2_d)
fviz_eig(r_ques2)

#Question3
r_ques3_d <- as.data.frame(ques[, 31 : 39])
r_ques3 <- PCA(r_ques3_d)
fviz_eig(r_ques3)

r_ques4_d <- ques$weight * as.data.frame(cbind(r_ques1_d, r_ques2_d, r_ques3_d))
r_ques4 <- PCA(r_ques4_d)
fviz_eig(r_ques4)
#isomap
library(vegan)
i_r_ques1 <- isomap(dist(r_ques1_d), ndim = 2, k = 5, fragmentedOK = T)
plot(i_r_ques1)

i_r_ques2 <- isomap(dist(r_ques2_d), ndim = 2, k = 5, fragmentedOK = T)
plot(i_r_ques2)

i_r_ques3 <- isomap(dist(r_ques3_d), ndim = 2, k = 5, fragmentedOK = T)
plot(i_r_ques3)

i_r_ld2 <- isomap(dist(r_ld2_d), ndim = 2, k = 5, fragmentOK = T)
i_r_ld2
plot(i_r_ld2)

i_r_ques4 <- isomap(dist(r_ques4_d), ndim = 2, k = 3, fragmentedOK = T)
plot(i_r_ques4)