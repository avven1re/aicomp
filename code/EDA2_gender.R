#encoding : UTF-8
#packages
library(devtools)
#install.packages("readr")
library(readr)
library(ggplot2)
library(gganimate)
library(magrittr)
library("reshape2")
#Import data
{
  guess_encoding("dataset/Happiness.csv", n_max = 1000)
  nOECD <- read.csv("dataset/Happiness.csv")
  names(nOECD)[1] <- c("v1")
  names(nOECD)
  
  OECD_weight <- as.vector(nOECD$weight)
  nOECD2 <- as.data.frame(OECD_weight * as.matrix(nOECD[, 2:39]))
  nOECD <- as.data.frame(cbind(nOECD2, nOECD[, 40:44]))}

#gender
#animated plot 7 v7-v20
meanOECD10 <- aggregate(v7 ~ sexgp, nOECD, mean, na.rm = T)
meanOECD10 <- merge(aggregate(v8 ~ sexgp, nOECD, mean, na.rm = T), meanOECD10, by = "sexgp")
meanOECD10 <- merge(aggregate(v9 ~ sexgp, nOECD, mean, na.rm = T), meanOECD10, by = "sexgp")
meanOECD10 <- merge(aggregate(v10 ~ sexgp, nOECD, mean, na.rm = T), meanOECD10, by = "sexgp")
meanOECD10 <- merge(aggregate(v11 ~ sexgp, nOECD, mean, na.rm = T), meanOECD10, by = "sexgp")
meanOECD10 <- merge(aggregate(v12 ~ sexgp, nOECD, mean, na.rm = T), meanOECD10, by = "sexgp")
meanOECD10 <- merge(aggregate(v13 ~ sexgp, nOECD, mean, na.rm = T), meanOECD10, by = "sexgp")
meanOECD10 <- merge(aggregate(v14 ~ sexgp, nOECD, mean, na.rm = T), meanOECD10, by = "sexgp")
meanOECD10 <- merge(aggregate(v15 ~ sexgp, nOECD, mean, na.rm = T), meanOECD10, by = "sexgp")
meanOECD10 <- merge(aggregate(v16 ~ sexgp, nOECD, mean, na.rm = T), meanOECD10, by = "sexgp")
meanOECD10 <- merge(aggregate(v17 ~ sexgp, nOECD, mean, na.rm = T), meanOECD10, by = "sexgp")
meanOECD10 <- merge(aggregate(v18 ~ sexgp, nOECD, mean, na.rm = T), meanOECD10, by = "sexgp")
meanOECD10 <- merge(aggregate(v19 ~ sexgp, nOECD, mean, na.rm = T), meanOECD10, by = "sexgp")
meanOECD10 <- merge(aggregate(v20 ~ sexgp, nOECD, mean, na.rm = T), meanOECD10, by = "sexgp")
t_meanOECD10 <- reshape2::melt(meanOECD10, id = "sexgp")
sex <- NaN
for (i in 1 : length(t_meanOECD10[, 1])) {
  if(t_meanOECD10[i, 1] == 1){
    sex[i] <- c("M")
  }
  else {
    sex[i] <- c("F")
  }
}
t_meanOECD10 <- as.data.frame(cbind(sex, t_meanOECD10[, 2 : 3]))
write.csv(t_meanOECD10, file = "dataset/sex_to_v7-v20.csv")

#
meanOECD11 <- aggregate(v31 ~ sexgp, nOECD, mean, na.rm = T)
meanOECD11 <- merge(aggregate(v32 ~ sexgp, nOECD, mean, na.rm = T), meanOECD11, by = "sexgp")
meanOECD11 <- merge(aggregate(v33 ~ sexgp, nOECD, mean, na.rm = T), meanOECD11, by = "sexgp")
meanOECD11 <- merge(aggregate(v34 ~ sexgp, nOECD, mean, na.rm = T), meanOECD11, by = "sexgp")
meanOECD11 <- merge(aggregate(v35 ~ sexgp, nOECD, mean, na.rm = T), meanOECD11, by = "sexgp")
meanOECD11 <- merge(aggregate(v36 ~ sexgp, nOECD, mean, na.rm = T), meanOECD11, by = "sexgp")
t_meanOECD11 <- reshape2::melt(meanOECD11, id = "sexgp")
sex <- NaN
for (i in 1 : length(t_meanOECD11[, 1])) {
  if(t_meanOECD11[i, 1] == 1){
    sex[i] <- c("M")
  }
  else {
    sex[i] <- c("F")
  }
}
t_meanOECD11 <- as.data.frame(cbind(sex, t_meanOECD11[, 2 : 3]))
write.csv(t_meanOECD11, file = "dataset/sex_to_v31-v36.csv")