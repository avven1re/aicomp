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


#animated plot (by age)

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

  animate(ani_OECD, height = 500, width = 650)
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
  
  animate(ani_OECD2, height = 500, width = 650)
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
  
  animate(ani_OECD3, height = 500, width = 650)
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
    scale_color_discrete(name = "問卷題目(1到10分)", labels = rev(c("請問您昨天覺得快樂嗎？", "請問您昨天覺得擔憂嗎？", "請問您昨天覺得沮喪嗎？", 
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
  
  animate(ani_OECD4, height = 500, width = 650)
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
  
  animate(ani_OECD5, height = 500, width = 650)
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
  
  animate(ani_OECD8, height = 500, width = 900)
  anim_save("images/inc_to_v31-36.gif")