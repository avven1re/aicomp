#encoding : UTF-8
#missForest
install.packages("missForest")
library(missForest)

#load data
{
  guess_encoding("dataset/Happiness.csv", n_max = 1000)
  nOECD <- read.csv("dataset/Happiness.csv")
  names(nOECD)[1] <- c("v1")
  names(nOECD)
  
}

#seed missing values ( 10% )
nOECD.mis <- prodNA(nOECD, noNA = 0.1)
summary(nOECD.mis)

#impute missing values, using all parameters as default values
nOECD.imp <- missForest(nOECD.mis)

#check imputed values
nOECD.imp$ximp

#check imputation error
nOECD.imp$OOBerror

#comparing actual data accuracy
nOECD.err <- mixError(nOECD.imp$ximp, nOECD.mis, nOECD)
nOECD.err

write.csv(nOECD.imp$ximp,file="dataset/data_missing.csv",row.names = FALSE)