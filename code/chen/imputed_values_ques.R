# install.packages("missForest")
library(missForest)
# seed missing values ( 5% )
dataC.mis <- prodNA(dataC[,c(29, 31:33)], noNA = 0.05)
summary(dataC.mis)

# impute missing values, using all parameters as default values
dataC.imp <- missForest(dataC.mis)

# check imputed values
dataC.imp$ximp

# check imputation error
dataC.imp$OOBerror

# comparing actual data accuracy
# dataC.err <- mixError(dataC.imp$ximp, dataC.mis, dataC)
# dataC.err

write.csv(ques.imp$ximp,file="dataset/data_missing.csv",row.names = FALSE)

