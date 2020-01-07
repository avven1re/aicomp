#encoding : UTF-8


#Import data
{
  guess_encoding("dataset/Happiness.csv", n_max = 1000)
  nOECD <- read.csv("dataset/Happiness.csv")
  names(nOECD)[1] <- c("v1")
  names(nOECD)
  
  OECD_weight <- as.vector(nOECD$weight)
  nOECD2 <- as.data.frame(OECD_weight * as.matrix(nOECD[, 2:39]))
  nOECD <- as.data.frame(cbind(nOECD2, nOECD[, 40:44]))}

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

for (i in 10 : 33) {
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

a_age <- aggregate(dataA$weight * dataA[, 10 : 33], list(dataA$agegp), mean, na.rm = T)
b_age <- aggregate(dataB$weight * dataB[, 10 : 33], list(dataB$agegp), mean, na.rm = T)
c_age <- aggregate(dataC$weight * dataC[, 10 : 33], list(dataC$agegp), mean, na.rm = T)
write.csv(a_age, "dataset/dataA_age_mean.csv")
write.csv(b_age, "dataset/dataB_age_mean.csv")
write.csv(c_age, "dataset/dataC_age_mean.csv")

a_sex <- aggregate(dataA$weight * dataA[, 10 : 33], list(dataA$sexgp), mean, na.rm = T)
b_sex <- aggregate(dataB$weight * dataB[, 10 : 33], list(dataB$sexgp), mean, na.rm = T)
c_sex <- aggregate(dataC$weight * dataC[, 10 : 33], list(dataC$sexgp), mean, na.rm = T)
write.csv(a_sex, "dataset/dataA_sex_mean.csv")
write.csv(b_sex, "dataset/dataB_sex_mean.csv")
write.csv(c_sex, "dataset/dataC_sex_mean.csv")

a_marrgp <- aggregate(dataA$weight * dataA[, 10 : 33], list(dataA$marrgp), mean, na.rm = T)
b_marrgp <- aggregate(dataB$weight * dataB[, 10 : 33], list(dataB$marrgp), mean, na.rm = T)
c_marrgp <- aggregate(dataC$weight * dataC[, 10 : 33], list(dataC$marrgp), mean, na.rm = T)
write.csv(a_marrgp, "dataset/dataA_marr_mean.csv")
write.csv(b_marrgp, "dataset/dataB_marr_mean.csv")
write.csv(c_marrgp, "dataset/dataC_marr_mean.csv")

a_incogp <- aggregate(dataA$weight * dataA[, 10 : 33], list(dataA$incogp), mean, na.rm = T)
b_incogp <- aggregate(dataB$weight * dataB[, 10 : 33], list(dataB$incogp), mean, na.rm = T)
c_incogp <- aggregate(dataC$weight * dataC[, 10 : 33], list(dataC$incogp), mean, na.rm = T)
write.csv(a_incogp, "dataset/dataA_inco_mean.csv")
write.csv(b_incogp, "dataset/dataB_inco_mean.csv")
write.csv(c_incogp, "dataset/dataC_inco_mean.csv")

a_edugp <- aggregate(dataA$weight * dataA[, 10 : 33], list(dataA$edugp), mean, na.rm = T)
b_edugp <- aggregate(dataB$weight * dataB[, 10 : 33], list(dataB$edugp), mean, na.rm = T)
c_edugp <- aggregate(dataC$weight * dataC[, 10 : 33], list(dataC$edugp), mean, na.rm = T)
write.csv(a_edugp, "dataset/dataA_edu_mean.csv")
write.csv(b_edugp, "dataset/dataB_edu_mean.csv")
write.csv(c_edugp, "dataset/dataC_edu_mean.csv")

a_cor <-cor(dataA[, 10 : 33], use = "complete.obs")
b_cor <-cor(dataB[, 10 : 33], use = "complete.obs")
c_cor <-cor(dataC[, 10 : 33], use = "complete.obs")
write.csv(a_cor, "dataset/dataA_cor_matrix.csv")
write.csv(b_cor, "dataset/dataB_cor_matrix.csv")
write.csv(c_cor, "dataset/dataC_cor_matrix.csv")

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
ggsave("images/dataA_corr_image.png")

ggplot(data = melted_b_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle("dataB Correlation Matrix")
ggsave("images/dataB_corr_image.png")


ggplot(data = melted_c_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle("dataC Correlation Matrix")
ggsave("images/dataC_corr_image.png")


#CORR
a_sir <- as.data.frame(a.sir.comp[, 1 : 2])
names(a_sir) <- c("a_sir1", "a_sir2")

b_sir <- as.data.frame(b.sir.comp[, 1 : 2])
names(b_sir) <- c("b_sir1", "b_sir2")

c_sir <- as.data.frame(c.sir.comp[, 1 : 3])
names(c_sir) <- c("c_sir1", "c_sir2", "c_sir3")

l_sir <- as.data.frame(land.isomap$points[,1:2])
names(l_sir) <- c("land_iso1", "land_iso2")

r_cor <- (cor(cbind(a_sir, b_sir, c_sir, l_sir), use = "complete.obs"))
#names(r_cor) <- c("a_sir1", "a_sir2", "b_sir1", "b_sir2", "c_sir1", "c_sir2", "c_sir3", "land_iso1", "land_iso2")
#row.names(r_cor)<- c("a_sir1", "a_sir2", "b_sir1", "b_sir2", "c_sir1", "c_sir2", "c_sir3", "land_iso1", "land_iso2")
#write.csv(r_cor, "dataset/dataC_cor_matrix")

library(reshape2)
melted_r_cor <- melt(r_cor)
head(melted_r_cor)

ggplot(data = melted_r_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle("DataB Rec Correlation Matrix")
ggsave("images/r_dataB_corr_image.png")

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

#
meanOECD12 <- aggregate(v37 ~ agegp, nOECD, mean, na.rm = T)
meanOECD12 <- merge(aggregate(v38 ~ agegp, nOECD, mean, na.rm = T), meanOECD12, by = "agegp")
meanOECD12 <- merge(aggregate(v39 ~ agegp, nOECD, mean, na.rm = T), meanOECD12, by = "agegp")
t_meanOECD12 <- reshape2::melt(meanOECD12, id = "agegp")
sex <- NaN
for (i in 1 : length(t_meanOECD12[, 1])) {
  if(t_meanOECD12[i, 1] == 1){
    sex[i] <- c("M")
  }
  else {
    sex[i] <- c("F")
  }
}
t_meanOECD12 <- as.data.frame(cbind(sex, t_meanOECD12[, 2 : 3]))
write.csv(t_meanOECD12, file = "dataset/sex_to_v37-v39.csv")

#marraige to v7-20
meanOECD13 <- aggregate(v7 ~ v41, nOECD, mean, na.rm = T)
meanOECD13 <- merge(aggregate(v8 ~ v41, nOECD, mean, na.rm = T), meanOECD13, by = "v41")
meanOECD13 <- merge(aggregate(v9 ~ v41, nOECD, mean, na.rm = T), meanOECD13, by = "v41")
meanOECD13 <- merge(aggregate(v10 ~ v41, nOECD, mean, na.rm = T), meanOECD13, by = "v41")
meanOECD13 <- merge(aggregate(v11 ~ v41, nOECD, mean, na.rm = T), meanOECD13, by = "v41")
meanOECD13 <- merge(aggregate(v12 ~ v41, nOECD, mean, na.rm = T), meanOECD13, by = "v41")
meanOECD13 <- merge(aggregate(v13 ~ v41, nOECD, mean, na.rm = T), meanOECD13, by = "v41")
meanOECD13 <- merge(aggregate(v14 ~ v41, nOECD, mean, na.rm = T), meanOECD13, by = "v41")
meanOECD13 <- merge(aggregate(v15 ~ v41, nOECD, mean, na.rm = T), meanOECD13, by = "v41")
meanOECD13 <- merge(aggregate(v16 ~ v41, nOECD, mean, na.rm = T), meanOECD13, by = "v41")
meanOECD13 <- merge(aggregate(v17 ~ v41, nOECD, mean, na.rm = T), meanOECD13, by = "v41")
meanOECD13 <- merge(aggregate(v18 ~ v41, nOECD, mean, na.rm = T), meanOECD13, by = "v41")
meanOECD13 <- merge(aggregate(v19 ~ v41, nOECD, mean, na.rm = T), meanOECD13, by = "v41")
meanOECD13 <- merge(aggregate(v20 ~ v41, nOECD, mean, na.rm = T), meanOECD13, by = "v41")
t_meanOECD13 <- reshape2::melt(meanOECD13, id = "v41")

ani_OECD13 <- t_meanOECD13 %>%
  ggplot(aes(x = v41, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "問卷題目(滿意度低到高1到10分)", labels = rev(c("請問您昨天覺得快樂嗎？", "請問您昨天覺得擔憂嗎？", "請問您昨天覺得沮喪嗎？", 
                                                                  "您對於目前生活的滿意程度", "對於人生當中所有做過的事情值得嗎?", "您對自己生活水準的滿意程度", 
                                                                  "對自己健康狀況", "對自己人生的成就", "對自己的人際關係", "請問您對自己安全感受的滿意程度", 
                                                                  "您對自己歸屬於社區一份子的滿意程度", "對自己未來生活的保障", 
                                                                  "請問您對可以做自己喜歡事情的時間長短", "您對居住地區環境品質的滿意程度"))) +
  xlab("婚姻狀態") +
  ylab("平均值") +
  scale_x_discrete(limits = 1 : 6, labels= c( "未婚", "已婚", "離婚", "分居", "喪偶", "同居")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(0, 10)) +
  ggtitle("社會聯繫與生活層面滿意度") +
  transition_reveal(v41) 

animate(ani_OECD13, height = 500, width = 650, end_pause = 30)
anim_save("images/marriage_to_v7-20.gif")

#marriage to v31-v36
meanOECD14<- aggregate(v31 ~ v41, nOECD, mean, na.rm = T)
meanOECD14<- merge(aggregate(v32 ~ v41, nOECD, mean, na.rm = T), meanOECD14, by = "v41")
meanOECD14<- merge(aggregate(v33 ~ v41, nOECD, mean, na.rm = T), meanOECD14, by = "v41")
meanOECD14<- merge(aggregate(v34 ~ v41, nOECD, mean, na.rm = T), meanOECD14, by = "v41")
meanOECD14<- merge(aggregate(v35 ~ v41, nOECD, mean, na.rm = T), meanOECD14, by = "v41")
meanOECD14<- merge(aggregate(v36 ~ v41, nOECD, mean, na.rm = T), meanOECD14, by = "v41")
t_meanOECD14<- reshape2::melt(meanOECD14, id = "v41")

ani_OECD14 <- t_meanOECD14%>%
  ggplot(aes(x = v41, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "問卷題目(信任度高到低:1到4分)", labels = rev(c("請問您對立法院信任或不信任？", "對司法制度及法院?", "對其他的中央政府?", 
                                                                  "對您居住地區的地方政府?", "對媒體的品質及公正性?", "請問您對我國的社會保障制度信不信任？"))) +
  xlab("婚姻狀態") +
  ylab("平均值") +
  scale_x_discrete(limits = 1 : 6, labels= c( "未婚", "已婚", "離婚", "分居", "喪偶", "同居")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(4, 0)) +
  ggtitle("政府機構信任程度") + 
  transition_reveal(v41) 

animate(ani_OECD14, height = 500, width = 650, end_pause = 30)
anim_save("images/marriage_to_v31-36.gif")

#marriage to v37-v39
meanOECD15 <- aggregate(v37 ~ v41, nOECD, mean, na.rm = T)
meanOECD15 <- merge(aggregate(v38 ~ v41, nOECD, mean, na.rm = T), meanOECD15, by = "v41")
meanOECD15 <- merge(aggregate(v39 ~ v41, nOECD, mean, na.rm = T), meanOECD15, by = "v41")
t_meanOECD15 <- reshape2::melt(meanOECD15, id = "v41")

ani_OECD15 <- t_meanOECD15 %>%
  ggplot(aes(x = v41, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  #geom_area() +
  scale_color_discrete(name = "問卷題目(信任度高到低:1到4分)", labels = rev(c("請問您滿不滿意您在我國所擁有的民主生活？", "請問您滿不滿意我國的言論自由?", 
                                                                  "政府官員會重視我們一般老百姓的想法，是否同意?"))) +
  xlab("婚姻狀態") +
  ylab("平均值") +
  scale_x_discrete(limits = 1 : 6, labels= c( "未婚", "已婚", "離婚", "分居", "喪偶", "同居")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(5, 0)) +
  ggtitle("國家情勢信任程度") + 
  transition_reveal(v41) 

animate(ani_OECD15, height = 500, width = 650, end_pause = 30)
anim_save("images/marriage_to_v37-39.gif")

#animated plot v7-v20
meanOECD <- aggregate(v7 ~ agegp, nOECD, mean, na.rm = T)
meanOECD <- merge(aggregate(v8 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
meanOECD <- merge(aggregate(v9 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
meanOECD <- merge(aggregate(v10 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
meanOECD <- merge(aggregate(v11 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
meanOECD <- merge(aggregate(v12 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
meanOECD <- merge(aggregate(v13 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
meanOECD <- merge(aggregate(v14 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
meanOECD <- merge(aggregate(v15 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
meanOECD <- merge(aggregate(v16 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
meanOECD <- merge(aggregate(v17 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
meanOECD <- merge(aggregate(v18 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
meanOECD <- merge(aggregate(v19 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
meanOECD <- merge(aggregate(v20 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
t_meanOECD <- reshape2::melt(meanOECD, id = "agegp")

ani_OECD <- t_meanOECD %>%
  ggplot(aes(x = agegp, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "問卷題目(滿意度低到高1到10分)", labels = rev(c("請問您昨天覺得快樂嗎？", "請問您昨天覺得擔憂嗎？", "請問您昨天覺得沮喪嗎？", 
                                                                  "您對於目前生活的滿意程度", "對於人生當中所有做過的事情值得嗎?", "您對自己生活水準的滿意程度", 
                                                                  "對自己健康狀況", "對自己人生的成就", "對自己的人際關係", "請問您對自己安全感受的滿意程度", 
                                                                  "您對自己歸屬於社區一份子的滿意程度", "對自己未來生活的保障", 
                                                                  "請問您對可以做自己喜歡事情的時間長短", "您對居住地區環境品質的滿意程度"))) +
  xlab("年齡分布") +
  ylab("平均值") +
  scale_x_discrete(limits = 1 : 6, labels= c( "15-24 歲", "25-34 歲", "35-44 歲", "45-54 歲", "55-64 歲", "65 歲以上")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(0, 10)) +
  ggtitle("社會聯繫與生活層面滿意度") +
  transition_reveal(agegp) 

animate(ani_OECD, height = 500, width = 650, end_pause = 30)
anim_save("images/age_to_v7-20.gif")

#animated plot 2 v31-v36
meanOECD2 <- aggregate(v31 ~ agegp, nOECD, mean, na.rm = T)
meanOECD2 <- merge(aggregate(v32 ~ agegp, nOECD, mean, na.rm = T), meanOECD2, by = "agegp")
meanOECD2 <- merge(aggregate(v33 ~ agegp, nOECD, mean, na.rm = T), meanOECD2, by = "agegp")
meanOECD2 <- merge(aggregate(v34 ~ agegp, nOECD, mean, na.rm = T), meanOECD2, by = "agegp")
meanOECD2 <- merge(aggregate(v35 ~ agegp, nOECD, mean, na.rm = T), meanOECD2, by = "agegp")
meanOECD2 <- merge(aggregate(v36 ~ agegp, nOECD, mean, na.rm = T), meanOECD2, by = "agegp")
t_meanOECD2 <- reshape2::melt(meanOECD2, id = "agegp")

ani_OECD2 <- t_meanOECD2 %>%
  ggplot(aes(x = agegp, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "問卷題目(信任度高到低:1到4分)", labels = rev(c("請問您對立法院信任或不信任？", "對司法制度及法院?", "對其他的中央政府?", 
                                                                  "對您居住地區的地方政府?", "對媒體的品質及公正性?", "請問您對我國的社會保障制度信不信任？"))) +
  xlab("年齡分布") +
  ylab("平均值") +
  scale_x_discrete(limits = 1 : 6, labels= c( "15-24 歲", "25-34 歲", "35-44 歲", "45-54 歲", "55-64 歲", "65 歲以上")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(4, 0)) +
  ggtitle("政府機構信任程度") + 
  transition_reveal(agegp) 

animate(ani_OECD2, height = 500, width = 650, end_pause = 30)
anim_save("images/age_to_v31-36.gif")

#animated plot 3 v37-v39
meanOECD3 <- aggregate(v37 ~ agegp, nOECD, mean, na.rm = T)
meanOECD3 <- merge(aggregate(v38 ~ agegp, nOECD, mean, na.rm = T), meanOECD3, by = "agegp")
meanOECD3 <- merge(aggregate(v39 ~ agegp, nOECD, mean, na.rm = T), meanOECD3, by = "agegp")
t_meanOECD3 <- reshape2::melt(meanOECD3, id = "agegp")

ani_OECD3 <- t_meanOECD3 %>%
  ggplot(aes(x = agegp, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "問卷題目(信任度高到低:1到4分)", labels = rev(c("請問您滿不滿意您在我國所擁有的民主生活？", "請問您滿不滿意我國的言論自由?", 
                                                                  "政府官員會重視我們一般老百姓的想法，是否同意?"))) +
  xlab("年齡分布") +
  ylab("平均值") +
  scale_x_discrete(limits = 1 : 6, labels= c( "15-24 歲", "25-34 歲", "35-44 歲", "45-54 歲", "55-64 歲", "65 歲以上")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(4, 0)) +
  ggtitle("國家情勢信任程度") + 
  transition_reveal(agegp) 

animate(ani_OECD3, height = 500, width = 650, end_pause = 30)
anim_save("images/age_to_v37-39.gif")

#animated plot (by edu)
#animated plot 4 v7-v20
meanOECD4 <- aggregate(v7 ~ edugp, nOECD, mean, na.rm = T)
meanOECD4 <- merge(aggregate(v8 ~ edugp, nOECD, mean, na.rm = T), meanOECD4, by = "edugp")
meanOECD4 <- merge(aggregate(v9 ~ edugp, nOECD, mean, na.rm = T), meanOECD4, by = "edugp")
meanOECD4 <- merge(aggregate(v10 ~ edugp, nOECD, mean, na.rm = T), meanOECD4, by = "edugp")
meanOECD4 <- merge(aggregate(v11 ~ edugp, nOECD, mean, na.rm = T), meanOECD4, by = "edugp")
meanOECD4 <- merge(aggregate(v12 ~ edugp, nOECD, mean, na.rm = T), meanOECD4, by = "edugp")
meanOECD4 <- merge(aggregate(v13 ~ edugp, nOECD, mean, na.rm = T), meanOECD4, by = "edugp")
meanOECD4 <- merge(aggregate(v14 ~ edugp, nOECD, mean, na.rm = T), meanOECD4, by = "edugp")
meanOECD4 <- merge(aggregate(v15 ~ edugp, nOECD, mean, na.rm = T), meanOECD4, by = "edugp")
meanOECD4 <- merge(aggregate(v16 ~ edugp, nOECD, mean, na.rm = T), meanOECD4, by = "edugp")
meanOECD4 <- merge(aggregate(v17 ~ edugp, nOECD, mean, na.rm = T), meanOECD4, by = "edugp")
meanOECD4 <- merge(aggregate(v18 ~ edugp, nOECD, mean, na.rm = T), meanOECD4, by = "edugp")
meanOECD4 <- merge(aggregate(v19 ~ edugp, nOECD, mean, na.rm = T), meanOECD4, by = "edugp")
meanOECD4 <- merge(aggregate(v20 ~ edugp, nOECD, mean, na.rm = T), meanOECD4, by = "edugp")
t_meanOECD4 <- reshape2::melt(meanOECD4, id = "edugp")

ani_OECD4 <- t_meanOECD4 %>%
  ggplot(aes(x = edugp, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "問卷題目(滿意度低到高1到10分)", labels = rev(c("請問您昨天覺得快樂嗎？", "請問您昨天覺得擔憂嗎？", "請問您昨天覺得沮喪嗎？", 
                                                                  "您對於目前生活的滿意程度", "對於人生當中所有做過的事情值得嗎?", "您對自己生活水準的滿意程度", 
                                                                  "對自己健康狀況", "對自己人生的成就", "對自己的人際關係", "請問您對自己安全感受的滿意程度", 
                                                                  "您對自己歸屬於社區一份子的滿意程度", "對自己未來生活的保障", 
                                                                  "請問您對可以做自己喜歡事情的時間長短", "您對居住地區環境品質的滿意程度"))) +
  xlab("教育程度分布") +
  ylab("平均值") +
  scale_x_discrete(limits = 1 : 3, labels= c( "國中小及以下", "高中(職)含五專前 3 年", "大專以上")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(0, 10)) +
  ggtitle("社會聯繫與生活層面滿意度") +
  transition_reveal(edugp) 

animate(ani_OECD4, height = 500, width = 650, end_pause = 30)
anim_save("images/edu_to_v7-20.gif")

#animated plot 5 v31-v36
meanOECD5 <- aggregate(v31 ~ edugp, nOECD, mean, na.rm = T)
meanOECD5 <- merge(aggregate(v32 ~ edugp, nOECD, mean, na.rm = T), meanOECD5, by = "edugp")
meanOECD5 <- merge(aggregate(v33 ~ edugp, nOECD, mean, na.rm = T), meanOECD5, by = "edugp")
meanOECD5 <- merge(aggregate(v34 ~ edugp, nOECD, mean, na.rm = T), meanOECD5, by = "edugp")
meanOECD5 <- merge(aggregate(v35 ~ edugp, nOECD, mean, na.rm = T), meanOECD5, by = "edugp")
meanOECD5 <- merge(aggregate(v36 ~ edugp, nOECD, mean, na.rm = T), meanOECD5, by = "edugp")
t_meanOECD5 <- reshape2::melt(meanOECD5, id = "edugp")

ani_OECD5 <- t_meanOECD5 %>%
  ggplot(aes(x = edugp, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "問卷題目(信任度高到低:1到4分)", labels = rev(c("請問您對立法院信任或不信任？", "對司法制度及法院?", "對其他的中央政府?", 
                                                                  "對您居住地區的地方政府?", "對媒體的品質及公正性?", "請問您對我國的社會保障制度信不信任？"))) +
  xlab("教育程度分布") +
  ylab("平均值") +
  scale_x_discrete(limits = 1 : 3, labels= c( "國中小及以下", "高中(職)含五專前 3 年", "大專以上")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(4, 0)) +
  ggtitle("政府機構信任程度") + 
  transition_reveal(edugp) 

animate(ani_OECD5, height = 500, width = 650, end_pause = 30)
anim_save("images/edu_to_v31-36.gif")

#animated plot 6 v37-v39
meanOECD6 <- aggregate(v37 ~ edugp, nOECD, mean, na.rm = T)
meanOECD6 <- merge(aggregate(v38 ~ edugp, nOECD, mean, na.rm = T), meanOECD6, by = "edugp")
meanOECD6 <- merge(aggregate(v39 ~ edugp, nOECD, mean, na.rm = T), meanOECD6, by = "edugp")
t_meanOECD6 <- reshape2::melt(meanOECD6, id = "edugp")

ani_OECD6 <- t_meanOECD6 %>%
  ggplot(aes(x = edugp, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "問卷題目(信任度高到低:1到4分)", labels = rev(c("請問您滿不滿意您在我國所擁有的民主生活？", "請問您滿不滿意我國的言論自由?", 
                                                                  "政府官員會重視我們一般老百姓的想法，是否同意?"))) +
  xlab("教育程度分布") +
  ylab("平均值") +
  scale_x_discrete(limits = 1 : 3, labels= c( "國中小及以下", "高中(職)含五專前 3 年", "大專以上")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(4, 0)) +
  ggtitle("國家情勢信任程度") + 
  transition_reveal(edugp) 

animate(ani_OECD6, height = 500, width = 650)
anim_save("images/edu_to_v37-39.gif")


#animated plot (by income)
#animated plot 7 v7-v20
meanOECD7 <- aggregate(v7 ~ vI1, nOECD, mean, na.rm = T)
meanOECD7 <- merge(aggregate(v8 ~ vI1, nOECD, mean, na.rm = T), meanOECD7, by = "vI1")
meanOECD7 <- merge(aggregate(v9 ~ vI1, nOECD, mean, na.rm = T), meanOECD7, by = "vI1")
meanOECD7 <- merge(aggregate(v10 ~ vI1, nOECD, mean, na.rm = T), meanOECD7, by = "vI1")
meanOECD7 <- merge(aggregate(v11 ~ vI1, nOECD, mean, na.rm = T), meanOECD7, by = "vI1")
meanOECD7 <- merge(aggregate(v12 ~ vI1, nOECD, mean, na.rm = T), meanOECD7, by = "vI1")
meanOECD7 <- merge(aggregate(v13 ~ vI1, nOECD, mean, na.rm = T), meanOECD7, by = "vI1")
meanOECD7 <- merge(aggregate(v14 ~ vI1, nOECD, mean, na.rm = T), meanOECD7, by = "vI1")
meanOECD7 <- merge(aggregate(v15 ~ vI1, nOECD, mean, na.rm = T), meanOECD7, by = "vI1")
meanOECD7 <- merge(aggregate(v16 ~ vI1, nOECD, mean, na.rm = T), meanOECD7, by = "vI1")
meanOECD7 <- merge(aggregate(v17 ~ vI1, nOECD, mean, na.rm = T), meanOECD7, by = "vI1")
meanOECD7 <- merge(aggregate(v18 ~ vI1, nOECD, mean, na.rm = T), meanOECD7, by = "vI1")
meanOECD7 <- merge(aggregate(v19 ~ vI1, nOECD, mean, na.rm = T), meanOECD7, by = "vI1")
meanOECD7 <- merge(aggregate(v20 ~ vI1, nOECD, mean, na.rm = T), meanOECD7, by = "vI1")
t_meanOECD7 <- reshape2::melt(meanOECD7, id = "vI1")

ani_OECD7 <- t_meanOECD7 %>%
  ggplot(aes(x = vI1, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "問卷題目(1到10分)", labels = rev(c("請問您昨天覺得快樂嗎？", "請問您昨天覺得擔憂嗎？", "請問您昨天覺得沮喪嗎？", 
                                                            "您對於目前生活的滿意程度", "對於人生當中所有做過的事情值得嗎?", "您對自己生活水準的滿意程度", 
                                                            "對自己健康狀況", "對自己人生的成就", "對自己的人際關係", "請問您對自己安全感受的滿意程度", 
                                                            "您對自己歸屬於社區一份子的滿意程度", "對自己未來生活的保障", 
                                                            "請問您對可以做自己喜歡事情的時間長短", "您對居住地區環境品質的滿意程度"))) +
  xlab("收入分布") +
  ylab("平均值") +
  scale_x_discrete(limits =c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 125, 175, 250, 350), labels= c( "無收入", "1萬元以內", "1到2萬元", "2到3萬元", "3到4萬元", "4到5萬元", 
                                                                                                       "5到6萬元", "6到7萬元", "7到8萬元", "8到9萬元", "9到10萬元", "10到15萬元", 
                                                                                                       "15到20萬元", "20到30萬元", "30萬元以上")) +
  theme(axis.text.x = element_text(angle = 90), axis.text=element_text(size = 10)) +
  ylim(c(0, 10)) +
  ggtitle("社會聯繫與生活層面滿意度") +
  transition_reveal(vI1) 

animate(ani_OECD7, height = 500, width = 900)
anim_save("images/inc_to_v7-20.gif")

#animated plot 8 v31-v36
meanOECD8 <- aggregate(v31 ~ vI1, nOECD, mean, na.rm = T)
meanOECD8 <- merge(aggregate(v32 ~ vI1, nOECD, mean, na.rm = T), meanOECD8, by = "vI1")
meanOECD8 <- merge(aggregate(v33 ~ vI1, nOECD, mean, na.rm = T), meanOECD8, by = "vI1")
meanOECD8 <- merge(aggregate(v34 ~ vI1, nOECD, mean, na.rm = T), meanOECD8, by = "vI1")
meanOECD8 <- merge(aggregate(v35 ~ vI1, nOECD, mean, na.rm = T), meanOECD8, by = "vI1")
meanOECD8 <- merge(aggregate(v36 ~ vI1, nOECD, mean, na.rm = T), meanOECD8, by = "vI1")
t_meanOECD8 <- reshape2::melt(meanOECD8, id = "vI1")

ani_OECD8 <- t_meanOECD8 %>%
  ggplot(aes(x = vI1, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "問卷題目(信任度高到低:1到4分)", labels = rev(c("請問您對立法院信任或不信任？", "對司法制度及法院?", "對其他的中央政府?", 
                                                                  "對您居住地區的地方政府?", "對媒體的品質及公正性?", "請問您對我國的社會保障制度信不信任？"))) +
  xlab("收入分布") +
  ylab("平均值") +
  scale_x_discrete(limits =c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 125, 175, 250, 350), labels= c( "無收入", "1萬元以內", "1到2萬元", "2到3萬元", "3到4萬元", "4到5萬元", 
                                                                                                       "5到6萬元", "6到7萬元", "7到8萬元", "8到9萬元", "9到10萬元", "10到15萬元", 
                                                                                                       "15到20萬元", "20到30萬元", "30萬元以上")) +
  theme(axis.text.x = element_text(angle = 90), axis.text=element_text(size = 10)) +
  ylim(c(4, 0)) +
  ggtitle("政府機構信任程度") + 
  transition_reveal(vI1) 

animate(ani_OECD8, height = 500, width = 900, end_pause = 30)
anim_save("images/inc_to_v31-36.gif")

#animated plot 6 v37-v39
meanOECD9 <- aggregate(v37 ~ vI1, nOECD, mean, na.rm = T)
meanOECD9 <- merge(aggregate(v38 ~ vI1, nOECD, mean, na.rm = T), meanOECD9, by = "vI1")
meanOECD9 <- merge(aggregate(v39 ~ vI1, nOECD, mean, na.rm = T), meanOECD9, by = "vI1")
t_meanOECD9 <- reshape2::melt(meanOECD9, id = "vI1")

ani_OECD6 <- t_meanOECD9 %>%
  ggplot(aes(x = vI1, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "問卷題目(信任度高到低:1到4分)", labels = rev(c("請問您滿不滿意您在我國所擁有的民主生活？", "請問您滿不滿意我國的言論自由?", 
                                                                  "政府官員會重視我們一般老百姓的想法，是否同意?"))) +
  xlab("收入分布") +
  ylab("平均值") +
  scale_x_discrete(limits =c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 125, 175, 250, 350), labels= c( "無收入", "1萬元以內", "1到2萬元", "2到3萬元", "3到4萬元", "4到5萬元", 
                                                                                                       "5到6萬元", "6到7萬元", "7到8萬元", "8到9萬元", "9到10萬元", "10到15萬元", 
                                                                                                       "15到20萬元", "20到30萬元", "30萬元以上")) +
  theme(axis.text.x = element_text(angle = 90), axis.text=element_text(size = 10)) +
  ylim(c(4, 0)) +
  ggtitle("國家情勢信任程度") + 
  transition_reveal(vI1) 

animate(ani_OECD6, height = 500, width = 900, end_pause = 30)
anim_save("images/inc_to_v37-39.gif")

source_url("https://github.com/avven1re/aicomp/blob/master/code/wu/2020_1_4.R?raw=TRUE")
# replace dataA dataB dataC in 2020_1_4

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

#cortest matrix land & ques
cor.test.mat <- function(data, vec_pos, type = "p.value"){
  cortestl <- list()
  cortest_mat <- matrix(NaN, length(vec_pos), length(vec_pos))
  for (i in vec_pos) {
    
    for (k in vec_pos) {
      re_test <- cor.test(data[, i], data[, k])
      if(type == "p.value"){
        cortest_mat[i, k] <- round(re_test$p.value, 3)
      }
      
      if(type == "estimate"){
        cortest_mat[i, k] <- round(re_test$estimate, 3)
      }
      
      if(mode(type) == "numeric"){
        cortest_mat[i, k] <- round(re_test[type], 3)
      }
    }
  }
  return(cortest_mat)
}

a_sir <- as.data.frame(a.sir.comp[, 1 : 2])
names(a_sir) <- c("a_sir1", "a_sir2")

b_sir <- as.data.frame(b.sir.comp[, 1 : 2])
names(b_sir) <- c("b_sir1", "b_sir2")

c_sir <- as.data.frame(c.sir.comp[, 1 : 3])
names(c_sir) <- c("c_sir1", "c_sir2", "c_sir3")

l_iso <- as.data.frame(land.isomap$points[,1:2])
names(l_iso) <- c("land_iso1", "land_iso2")
corr <- cbind(a_sir, b_sir, c_sir, dataA[ ,64:72], 
              l_iso)
names(corr)

dataA_cor_test <- as.data.frame(cor.test.mat(corr, 1 : 18))
names(dataA_cor_test) <- names(corr)
row.names(dataA_cor_test) <- names(corr)
head(dataA_cor_test)
write.csv(dataA_cor_test, "dataset/dataA_pvalue_reduced_cortest_mat.csv")

dataA_cor_test2 <- as.data.frame(cor.test.mat(corr, 1 : 18), type = "estimate")
names(dataA_cor_test2) <- names(corr)
row.names(dataA_cor_test2) <- names(corr)
head(dataA_cor_test2)
write.csv(dataA_cor_test2, "dataset/dataA_estimate_reduced_cortest_mat.csv")

heatmap(as.matrix(dataA_cor_test))

c_corr <- cor(corr, use = "complete.obs")
melted_dataA_cor_test <- melt(c_corr)
head(melted_dataA_cor_test)

ggplot(data = melted_dataA_cor_test, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle("dataA Reduced Correlation Matrix")


#dataB

a_sir <- as.data.frame(a.sir.comp[, 1 : 2])
names(a_sir) <- c("a_sir1", "a_sir2")

b_sir <- as.data.frame(b.sir.comp[, 1 : 2])
names(b_sir) <- c("b_sir1", "b_sir2")

c_sir <- as.data.frame(c.sir.comp[, 1 : 3])
names(c_sir) <- c("c_sir1", "c_sir2", "c_sir3")

l_iso <- as.data.frame(land.isomap$points[,1:2])
names(l_iso) <- c("land_iso1", "land_iso2")
corr <- cbind(a_sir, b_sir, c_sir, dataB[ ,64:72], 
              l_iso)
names(corr)

dataB_cor_test <- as.data.frame(cor.test.mat(corr, 1 : 18))
names(dataB_cor_test) <- names(corr)
row.names(dataB_cor_test) <- names(corr)
head(dataB_cor_test)
write.csv(dataB_cor_test, "dataset/dataB_pvalue_reduced_cortest_mat.csv")

dataB_cor_test2 <- as.data.frame(cor.test.mat(corr, 1 : 18), type = "estimate")
names(dataB_cor_test2) <- names(corr)
row.names(dataB_cor_test2) <- names(corr)
head(dataB_cor_test2)
write.csv(dataB_cor_test2, "dataset/dataB_estimate_reduced_cortest_mat.csv")

heatmap(as.matrix(dataB_cor_test))

c_corr <- cor(corr, use = "complete.obs")
melted_dataB_cor_test <- melt(c_corr)
head(melted_dataB_cor_test)

ggplot(data = melted_dataB_cor_test, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle("dataB Reduced Correlation Matrix")

#dataC

a_sir <- as.data.frame(a.sir.comp[, 1 : 2])
names(a_sir) <- c("a_sir1", "a_sir2")

b_sir <- as.data.frame(b.sir.comp[, 1 : 2])
names(b_sir) <- c("b_sir1", "b_sir2")

c_sir <- as.data.frame(c.sir.comp[, 1 : 3])
names(c_sir) <- c("c_sir1", "c_sir2", "c_sir3")

l_iso <- as.data.frame(land.isomap$points[,1:2])
names(l_iso) <- c("land_iso1", "land_iso2")
corr <- cbind(a_sir, b_sir, c_sir, dataC[ ,64:72], 
              l_iso)
names(corr)

dataC_cor_test <- as.data.frame(cor.test.mat(corr, 1 : 18))
names(dataC_cor_test) <- names(corr)
row.names(dataC_cor_test) <- names(corr)
head(dataC_cor_test)
write.csv(dataC_cor_test, "dataset/dataC_pvalue_reduced_cortest_mat.csv")

dataC_cor_test2 <- as.data.frame(cor.test.mat(corr, 1 : 18), type = "estimate")
names(dataC_cor_test2) <- names(corr)
row.names(dataC_cor_test2) <- names(corr)
head(dataC_cor_test2)
write.csv(dataC_cor_test2, "dataset/dataC_estimate_reduced_cortest_mat.csv")

heatmap(as.matrix(dataC_cor_test))

c_corr <- cor(corr, use = "complete.obs")
melted_dataC_cor_test <- melt(c_corr)
head(melted_dataC_cor_test)

ggplot(data = melted_dataC_cor_test, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggtitle("dataC Reduced Correlation Matrix")