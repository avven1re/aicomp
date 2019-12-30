#Combine Bar plot and Line chart practice

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
  guess_encoding("dataset/data_rHDDA.csv", n_max = 1000)
  nOECD <- read.csv("dataset/data_rHDDA.csv")
  names(nOECD)[1] <- c("v1")
  names(nOECD)
}


#animated plot v7-v20
meanOECD <- aggregate(v7 ~ agegp, nOECD, mean, na.rm = T)
meanOECD <- cbind(meanOECD, table(nOECD$agegp))[, -3]
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

ani_OECD <- t_meanOECD[1 : 84, ] %>%
  ggplot(aes(x = agegp, y = value, color = variable)) +
  geom_bar(t_meanOECD[85 : 90, ], mapping = aes(x = agegp, y = value/sum(value) * 10, fill = agegp), stat = "identity", na.rm = T) +
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
  #ylim(c(0, 10)) +
  scale_y_continuous(limits = c(0, 10), sec.axis = sec_axis(~. *10, name = "人數百分比")) +
  ggtitle("社會聯繫與生活層面滿意度") +
  transition_reveal(agegp) 

animate(ani_OECD, height = 500, width = 650)
#anim_save("images/age_to_v7-20.gif")

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

animate(ani_OECD2, height = 500, width = 650)
anim_save("images/age_to_v31-36.gif")