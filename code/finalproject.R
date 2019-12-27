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


#animated plot

ageOECD <- function(var_pos, dataset = nOECD, save = F){
  for (i in var_pos) {
    pl <- ggplot(dataset, aes(x = dataset$agegp, y = dataset[, i], fill = dataset$agegp), na.rm = T) + geom_bar(stat = "summary", na.rm = T, fun.y = "mean") + 
      scale_fill_continuous(name="年齡分布", labels=c( "15-24 歲", "25-34 歲", "35-44 歲", "45-54 歲", "55-64 歲", "65 歲以上")) +
      #geom_text(aes(label = round(tapply(nOECD$v7, nOECD$agegp, mean, na.rm = T), 2)), vjust=1.6, size=5.5, color = "white")+
      xlab("年齡分布") + 
      ylab(paste0(names(dataset)[i], "mean"))+
      ylim(c(0, max(dataset[, i], na.rm = T))) +
      ggtitle(paste0("年齡分布與", names(dataset)[i]))  
    
  }
  if(save == T){
  ggsave(paste0("images/age_with_", names(dataset)[i], ".png"))}
  return(pl)
}

#example
#ageOECD(10, save = T)

#畫多個圖且匯出
#for (k in 8 : 21) {
#  ageOECD(k, save = T)
#}

#animated plot
meanOECD <- aggregate(v7 ~ agegp, nOECD, mean, na.rm = T)
meanOECD <- merge(aggregate(v20 ~ agegp, nOECD, mean, na.rm = T), meanOECD, by = "agegp")
t_meanOECD <- reshape2::melt(meanOECD, id = "agegp")

t_meanOECD %>%
  ggplot(aes(x = agegp, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_linetype_discrete(name = "問卷題目", labels = c("請問您昨天覺得快樂嗎？", "請問您昨天覺得擔憂嗎？", "請問您昨天覺得沮喪嗎？", 
                                                    "您對於目前生活的滿意程度", "對於人生當中所有做過的事情值得嗎?", "您對自己生活水準的滿意程度", 
                                                    "對自己健康狀況", "對自己人生的成就", "對自己的人際關係", "請問您對自己安全感受的滿意程度", 
                                                    "您對自己歸屬於社區一份子的滿意程度", "對自己未來生活的保障", 
                                                    "請問您對可以做自己喜歡事情的時間長短", "您對居住地區環境品質的滿意程度")) +
  #geom_text(aes(label=c( "15-24 歲", "25-34 歲", "35-44 歲", "45-54 歲", "55-64 歲", "65 歲以上"))) +
  ylim(c(0, 10)) #+
  transition_reveal(agegp) 
  anim_save("images/age_to_v7-20.gif")
