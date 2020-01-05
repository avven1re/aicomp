# encoding : UTF-8
install.packages("DataExplorer")
library(DataExplorer)



plot_boxplot(dataA[, 14], by = dataA$agegp)


library(plotly)

p <- plot_ly(
  x = c("giraffes", "orangutans", "monkeys"),
  y = c(20, 14, 23),
  name = "SF Zoo",
  type = "bar"
)

p

##
ageOECD <- function(var_pos, dataset = nOECD, save = F){
  for (i in var_pos) {
    pl <- ggplot(dataset, aes(x = dataset$agegp, y = dataset[, i] * dataset$weight, fill = dataset$agegp), na.rm = T) + geom_bar(stat = "summary", na.rm = T, fun.y = "mean") + 
      scale_fill_gradient(low = "black", high = "red", name="年齡分布", labels=c( "15-24 歲", "25-34 歲", "35-44 歲", "45-54 歲", "55-64 歲", "65 歲以上")) +
      #geom_text(aes(label = round(tapply(nOECD$v7, nOECD$agegp, mean, na.rm = T), 2)), vjust=1.6, size=5.5, color = "white")+
      xlab("年齡分布") + 
      ylab(paste0(names(dataset)[i], "mean"))+
      scale_x_discrete(limits = 1 : 6, labels= c( "15-24 歲", "25-34 歲", "35-44 歲", "45-54 歲", "55-64 歲", "65 歲以上")) +
      theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
      ylim(c(0, max(dataset[, i], na.rm = T))) +
      ggtitle(paste0("年齡分布與", names(dataset)[i]))  
    
  }
  if(save == T){
    ggsave(paste0("images/age_with_dataC_", names(dataset)[i], ".png"))}
  return(pl)
}

ageOECD(18, dataC)

for (i in 14 : 37) {
  ageOECD(i, dataC, save = F)
}


##
sexOECD <- function(var_pos, dataset = nOECD, save = F){
  for (i in var_pos) {
    pl <- ggplot(dataset, aes(x = dataset$sexgp, y = dataset[, i] * dataset$weight, fill = dataset$sexgp), na.rm = T) + geom_bar(stat = "summary", na.rm = T, fun.y = "mean") + 
      scale_fill_gradient(low = "black", high = "red", name = "性別", labels = c( "男", "女")) +
      #geom_text(aes(label = round(tapply(nOECD$v7, nOECD$agegp, mean, na.rm = T), 2)), vjust=1.6, size=5.5, color = "white")+
      xlab("年齡分布") + 
      ylab(paste0(names(dataset)[i], "mean"))+
      #scale_x_discrete(limits = c(1, 2), labels = c( "男", "女")) +
      theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
      ylim(c(0, max(dataset[, i], na.rm = T))) +
      ggtitle(paste0("性別與", names(dataset)[i]))  
    
  }
  if(save == T){
    ggsave(paste0("images/sex_with_dataA_", names(dataset)[i], ".png"))}
  return(pl)
}


sexOECD(18, dataA)

ggplot(dataA, aes(x = sexgp, y = dataA[, 15] * weight, fill = sexgp), na.rm = T) + geom_bar(stat = "summary", na.rm = T, fun.y = "mean") + 
  scale_fill_gradient(low = "black", high = "red", name = "性別", labels = c( "男", "女"))

a_age <- aggregate(dataA$weight * dataA[, 14 : 37], list(dataA$agegp), mean, na.rm = T)
b_age <- aggregate(dataB$weight * dataB[, 14 : 37], list(dataB$agegp), mean, na.rm = T)
c_age <- aggregate(dataC$weight * dataC[, 14 : 37], list(dataC$agegp), mean, na.rm = T)
write.csv(a_age, "dataset/dataA_age_mean")
write.csv(b_age, "dataset/dataB_age_mean")
write.csv(c_age, "dataset/dataC_age_mean")

a_sex <- aggregate(dataA$weight * dataA[, 14 : 37], list(dataA$sexgp), mean, na.rm = T)
b_sex <- aggregate(dataB$weight * dataB[, 14 : 37], list(dataB$sexgp), mean, na.rm = T)
c_sex <- aggregate(dataC$weight * dataC[, 14 : 37], list(dataC$sexgp), mean, na.rm = T)
write.csv(a_sex, "dataset/dataA_sex_mean")
write.csv(b_sex, "dataset/dataB_sex_mean")
write.csv(c_sex, "dataset/dataC_sex_mean")

a_marrgp <- aggregate(dataA$weight * dataA[, 14 : 37], list(dataA$marrgp), mean, na.rm = T)
b_marrgp <- aggregate(dataB$weight * dataB[, 14 : 37], list(dataB$marrgp), mean, na.rm = T)
c_marrgp <- aggregate(dataC$weight * dataC[, 14 : 37], list(dataC$marrgp), mean, na.rm = T)
write.csv(a_marrgp, "dataset/dataA_marr_mean")
write.csv(b_marrgp, "dataset/dataB_marr_mean")
write.csv(c_marrgp, "dataset/dataC_marr_mean")

a_incogp <- aggregate(dataA$weight * dataA[, 14 : 37], list(dataA$incogp), mean, na.rm = T)
b_incogp <- aggregate(dataB$weight * dataB[, 14 : 37], list(dataB$incogp), mean, na.rm = T)
c_incogp <- aggregate(dataC$weight * dataC[, 14 : 37], list(dataC$incogp), mean, na.rm = T)
write.csv(a_incogp, "dataset/dataA_inco_mean")
write.csv(b_incogp, "dataset/dataB_inco_mean")
write.csv(c_incogp, "dataset/dataC_inco_mean")

a_edugp <- aggregate(dataA$weight * dataA[, 14 : 37], list(dataA$edugp), mean, na.rm = T)
b_edugp <- aggregate(dataB$weight * dataB[, 14 : 37], list(dataB$edugp), mean, na.rm = T)
c_edugp <- aggregate(dataC$weight * dataC[, 14 : 37], list(dataC$edugp), mean, na.rm = T)
write.csv(a_edugp, "dataset/dataA_edu_mean")
write.csv(b_edugp, "dataset/dataB_edu_mean")
write.csv(c_edugp, "dataset/dataC_edu_mean")