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
  #geom_text(aes(label=c( "15-24 歲", "25-34 歲", "35-44 歲", "45-54 歲", "55-64 歲", "65 歲以上"))) +
  ylim(c(0, 10)) +
  transition_reveal(agegp) 
  anim_save("images/age_to_v7-20.gif")
