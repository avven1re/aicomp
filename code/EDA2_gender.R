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
    sex[i] <- c("男")
  }
  else {
    sex[i] <- c("女")
  }
}
t_meanOECD10 <- as.data.frame(cbind(sex, t_meanOECD10[, 2 : 3]))
write.csv(t_meanOECD10, file = "dataset/sex_to_v7-v20.csv")

ani_OECD10 <- t_meanOECD10 %>%
  ggplot(aes(x = sexgp, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "問卷題目(滿意度低到高1到10分)", labels = rev(c("請問您昨天覺得快樂嗎？", "請問您昨天覺得擔憂嗎？", "請問您昨天覺得沮喪嗎？", 
                                                                  "您對於目前生活的滿意程度", "對於人生當中所有做過的事情值得嗎?", "您對自己生活水準的滿意程度", 
                                                                  "對自己健康狀況", "對自己人生的成就", "對自己的人際關係", "請問您對自己安全感受的滿意程度", 
                                                                  "您對自己歸屬於社區一份子的滿意程度", "對自己未來生活的保障", 
                                                                  "請問您對可以做自己喜歡事情的時間長短", "您對居住地區環境品質的滿意程度"))) +
  xlab("年齡分布") +
  ylab("平均值") +
  scale_x_discrete(limits = 1 : 6, labels= c("女", "男")) +
  theme(axis.text.x = element_text(angle = 0), axis.text=element_text(size = 18)) +
  ylim(c(0, 10)) +
  ggtitle("社會聯繫與生活層面滿意度") +
  transition_reveal(sexgp) 

animate(ani_OECD10, height = 500, width = 650, end_pause = 30)
anim_save("images/sex_to_v7-20.gif")

#
t_meanOECD10 %>%
  ggplot(aes(x = sexgp, y = value)) +
  geom_bar(t_meanOECD10, stat = "identity", mapping = aes( y = value ,fill = variable, group = sexgp))