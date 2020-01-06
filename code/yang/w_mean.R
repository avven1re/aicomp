# encoding : UTF-8

weighted.mean(dataA[, 10], w = dataA$weight, na.rm = T)
pos_vec <- 10:33
data <- dataA

weightm <- function(pos_vec, data){
  mat <- matrix(NaN, 1, 24)
  k <- 1
  for (i in pos_vec) {
    mat[1, k] <- weighted.mean(data[, i], w = data$weight, na.rm = T)
    k <- k + 1
  }
  return(mat)
}

wm_A <- weightm(10:33, dataA)
wm_B <- weightm(10:33, dataB)
wm_C <- weightm(10:33, dataC)

wm_dataset <- as.data.frame(rbind(wm_A, wm_B, wm_C))
names(wm_dataset) <- names(dataA[10:33])
row.names(wm_dataset) <- c("dataA", "dataB", "dataC")
wm_dataset
write.csv(wm_dataset, "dataset/weightmean_dataABC.csv")

wm_la <- as.data.frame(lapply(dataA[, 1 : 9], mean))
wm_lb <- as.data.frame(lapply(dataB[, 1 : 9], mean))
wm_lc <- as.data.frame(lapply(dataC[, 1 : 9], mean))

wm_landABC <- as.data.frame(rbind(wm_la, wm_lb, wm_lc))
names(wm_landABC)
row.names(wm_landABC) <- c("dataA", "dataB", "dataC")
wm_landABC
write.csv(wm_landABC, "dataset/mean_landABC.csv")


#corr land & ques
a_cor <-cor(dataA[, 1 : 33], use = "complete.obs")
b_cor <-cor(dataB[, 1 : 33], use = "complete.obs")
c_cor <-cor(dataC[, 1 : 33], use = "complete.obs")
write.csv(a_cor, "dataset/dataA_l_q_cor_matrix.csv")
write.csv(b_cor, "dataset/dataB_l_q_cor_matrix.csv")
write.csv(c_cor, "dataset/dataC_l_q_cor_matrix.csv")

library(reshape2)
melted_a_cor <- melt(a_cor)
head(melted_a_cor)

melted_b_cor <- melt(b_cor)
head(melted_a_cor)

melted_c_cor <- melt(c_cor)
head(melted_a_cor)

ggplot(data = melted_a_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle("dataA Correlation Matrix")
ggsave("images/dataA_l_q_corr_image.png")

ggplot(data = melted_b_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle("dataB Correlation Matrix")
ggsave("images/dataB_l_q_corr_image.png")


ggplot(data = melted_c_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle("dataC Correlation Matrix")
ggsave("images/dataC_l_q_corr_image.png")

#cortest matrix land & ques
cor.test.mat <- function(data, vec_pos, type = "p.value"){
  cortestl <- list()
  cortest_mat <- matrix(NaN, length(vec_pos), length(vec_pos))
  for (i in vec_pos) {
    
    for (k in vec_pos) {
      re_test <- cor.test(data[, i], data[, k])
      if(type == "p.value"){
      cortest_mat[i, k] <- round(re_test$p.value, 3)
      }
      
      if(type == "estimate"){
        cortest_mat[i, k] <- round(re_test$estimate, 3)
      }
      
      if(mode(type) == "numeric"){
        cortest_mat[i, k] <- round(re_test[type], 3)
      }
    }
  }
  return(cortest_mat)
}

a_sir <- as.data.frame(a.sir.comp[, 1 : 2])
names(a_sir) <- c("a_sir1", "a_sir2")

b_sir <- as.data.frame(b.sir.comp[, 1 : 2])
names(b_sir) <- c("b_sir1", "b_sir2")

c_sir <- as.data.frame(c.sir.comp[, 1 : 3])
names(c_sir) <- c("c_sir1", "c_sir2", "c_sir3")

l_iso <- as.data.frame(land.isomap$points[,1:2])
names(l_iso) <- c("land_iso1", "land_iso2")
corr <- cbind(a_sir, b_sir, c_sir, dataA[ ,64:72], 
              l_iso)
names(corr)

dataA_cor_test <- as.data.frame(cor.test.mat(corr, 1 : 18))
names(dataA_cor_test) <- names(corr)
row.names(dataA_cor_test) <- names(corr)
head(dataA_cor_test)
write.csv(dataA_cor_test, "dataset/dataA_pvalue_reduced_cortest_mat.csv")

dataA_cor_test2 <- as.data.frame(cor.test.mat(corr, 1 : 18), type = "estimate")
names(dataA_cor_test2) <- names(corr)
row.names(dataA_cor_test2) <- names(corr)
head(dataA_cor_test2)
write.csv(dataA_cor_test2, "dataset/dataA_estimate_reduced_cortest_mat.csv")

heatmap(as.matrix(dataA_cor_test))

c_corr <- cor(corr, use = "complete.obs")
melted_dataA_cor_test <- melt(c_corr)
head(melted_dataA_cor_test)

ggplot(data = melted_dataA_cor_test, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle("dataA Reduced Correlation Matrix")


#dataB

a_sir <- as.data.frame(a.sir.comp[, 1 : 2])
names(a_sir) <- c("a_sir1", "a_sir2")

b_sir <- as.data.frame(b.sir.comp[, 1 : 2])
names(b_sir) <- c("b_sir1", "b_sir2")

c_sir <- as.data.frame(c.sir.comp[, 1 : 3])
names(c_sir) <- c("c_sir1", "c_sir2", "c_sir3")

l_iso <- as.data.frame(land.isomap$points[,1:2])
names(l_iso) <- c("land_iso1", "land_iso2")
corr <- cbind(a_sir, b_sir, c_sir, dataB[ ,64:72], 
              l_iso)
names(corr)

dataB_cor_test <- as.data.frame(cor.test.mat(corr, 1 : 18))
names(dataB_cor_test) <- names(corr)
row.names(dataB_cor_test) <- names(corr)
head(dataB_cor_test)
write.csv(dataB_cor_test, "dataset/dataB_pvalue_reduced_cortest_mat.csv")

dataB_cor_test2 <- as.data.frame(cor.test.mat(corr, 1 : 18), type = "estimate")
names(dataB_cor_test2) <- names(corr)
row.names(dataB_cor_test2) <- names(corr)
head(dataB_cor_test2)
write.csv(dataB_cor_test2, "dataset/dataB_estimate_reduced_cortest_mat.csv")

heatmap(as.matrix(dataB_cor_test))

c_corr <- cor(corr, use = "complete.obs")
melted_dataB_cor_test <- melt(c_corr)
head(melted_dataB_cor_test)

ggplot(data = melted_dataB_cor_test, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle("dataB Reduced Correlation Matrix")

#dataC

a_sir <- as.data.frame(a.sir.comp[, 1 : 2])
names(a_sir) <- c("a_sir1", "a_sir2")

b_sir <- as.data.frame(b.sir.comp[, 1 : 2])
names(b_sir) <- c("b_sir1", "b_sir2")

c_sir <- as.data.frame(c.sir.comp[, 1 : 3])
names(c_sir) <- c("c_sir1", "c_sir2", "c_sir3")

l_iso <- as.data.frame(land.isomap$points[,1:2])
names(l_iso) <- c("land_iso1", "land_iso2")
corr <- cbind(a_sir, b_sir, c_sir, dataC[ ,64:72], 
              l_iso)
names(corr)

dataC_cor_test <- as.data.frame(cor.test.mat(corr, 1 : 18))
names(dataC_cor_test) <- names(corr)
row.names(dataC_cor_test) <- names(corr)
head(dataC_cor_test)
write.csv(dataC_cor_test, "dataset/dataC_pvalue_reduced_cortest_mat.csv")

dataC_cor_test2 <- as.data.frame(cor.test.mat(corr, 1 : 18), type = "estimate")
names(dataC_cor_test2) <- names(corr)
row.names(dataC_cor_test2) <- names(corr)
head(dataC_cor_test2)
write.csv(dataC_cor_test2, "dataset/dataC_estimate_reduced_cortest_mat.csv")

heatmap(as.matrix(dataC_cor_test))

c_corr <- cor(corr, use = "complete.obs")
melted_dataC_cor_test <- melt(c_corr)
head(melted_dataC_cor_test)

ggplot(data = melted_dataC_cor_test, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle("dataC Reduced Correlation Matrix")