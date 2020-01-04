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
  
}
#

meanOECD <- aggregate(v7 ~ t, nOECD, mean, na.rm = T)
meanOECD <- merge(aggregate(v8 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v9 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v10 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v11 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v12 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v13 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v14 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v15 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v16 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v17 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v18 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v19 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v20 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")

meanOECD <- merge(aggregate(v22 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v31 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v32 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v33 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v34 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v35 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v36 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v37 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v38 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")
meanOECD <- merge(aggregate(v39 ~ t, nOECD, mean, na.rm = T), meanOECD, by = "t")

names(meanOECD)[1] <- c("TOWNCODE")
#write.csv(meanOECD, "dataset/town/t_trans_OECD.csv")
#t_meanOECD <- reshape2::melt(meanOECD, id = "t")