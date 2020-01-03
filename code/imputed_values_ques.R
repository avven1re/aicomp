## °İ¨÷
ques = read.csv("dataset/Happiness.csv")  
ques[,1] <- 109-ques[,1]
names(ques)[1:41] <- c("age",paste('q',1:38,sep=''),"marrgp","incogp")
names(ques)[46] <- "TOWNCODE"
head(ques)

# install.packages("missForest")
library(missForest)
# seed missing values ( 5% )
ques.mis <- prodNA(ques[,c(6:19, 30:38)], noNA = 0.05)
summary(ques.mis)

# impute missing values, using all parameters as default values
ques.imp <- missForest(ques.mis)

# check imputed values
ques.imp$ximp

# check imputation error
ques.imp$OOBerror

# comparing actual data accuracy
ques.err <- mixError(ques.imp$ximp, ques.mis, ques)
ques.err

write.csv(ques.imp$ximp,file="dataset/data_missing.csv",row.names = FALSE)