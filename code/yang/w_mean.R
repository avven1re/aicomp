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