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
