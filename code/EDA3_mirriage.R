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