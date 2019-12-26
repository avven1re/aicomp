#encoding : UTF-8

source_url("https://github.com/avven1re/aicomp/blob/master/code/requiredpackages.R?raw=True")
#Import data

guess_encoding("dataset/data_rHDDA.csv", n_max = 1000)
OECD <- read.csv("dataset/data_rHDDA.csv")
names(OECD)[1] <- c("v1")
names(OECD)