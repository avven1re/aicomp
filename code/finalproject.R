#encoding : UTF-8
library(devtools)
#install.packages("readr")
library(readr)

#Import data

guess_encoding("dataset/data_rHDDA.csv", n_max = 1000)
OECD <- read.csv("dataset/data_rHDDA.csv")
names(OECD)[1] <- c("v1")
names(OECD)