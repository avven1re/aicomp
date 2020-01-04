#encoding : CP950
#packages
library(devtools)
#install.packages("readr")
library(readr)
library(ggplot2)
library(gganimate)
library(magrittr)
library("reshape2")
#Import data (weight include)
{
  guess_encoding("dataset/Happiness.csv", n_max = 1000)
  nOECD <- read.csv("dataset/Happiness.csv")
  names(nOECD)[1] <- c("v1")
  names(nOECD)
  
  OECD_weight <- as.vector(nOECD$weight)
  nOECD2 <- as.data.frame(OECD_weight * as.matrix(nOECD[, 2:39]))
  nOECD <- as.data.frame(cbind(nOECD2, nOECD[, 40:44]))}
#animated plot (by age)

ageOECD <- function(var_pos, dataset = nOECD, save = F){
  for (i in var_pos) {
    pl <- ggplot(dataset, aes(x = dataset$agegp, y = dataset[, i], fill = dataset$agegp), na.rm = T) + geom_bar(stat = "summary", na.rm = T, fun.y = "mean") + 
      scale_fill_continuous(name="�~�֤���", labels=c( "15-24 ��", "25-34 ��", "35-44 ��", "45-54 ��", "55-64 ��", "65 ���H�W")) +
      #geom_text(aes(label = round(tapply(nOECD$v7, nOECD$agegp, mean, na.rm = T), 2)), vjust=1.6, size=5.5, color = "white")+
      xlab("�~�֤���") + 
      ylab(paste0(names(dataset)[i], "mean"))+
      ylim(c(0, max(dataset[, i], na.rm = T))) +
      ggtitle(paste0("�~�֤����P", names(dataset)[i]))  
    
  }
  if(save == T){
    ggsave(paste0("images/age_with_", names(dataset)[i], ".png"))}
  return(pl)
}

#example
#ageOECD(10, save = T)

#�e�h�ӹϥB�ץX
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
  scale_color_discrete(name = "�ݨ��D��(���N�קC�찪1��10��)", labels = rev(c("�аݱz�Q��ı�o�ֶּܡH", "�аݱz�Q��ı�o��~�ܡH", "�аݱz�Q��ı�o�q��ܡH", 
                                                                  "�z���ثe�ͬ������N�{��", "���H�ͷ����Ҧ����L���Ʊ��ȱo��?", "�z��ۤv�ͬ����Ǫ����N�{��", 
                                                                  "��ۤv���d���p", "��ۤv�H�ͪ����N", "��ۤv���H�����Y", "�аݱz��ۤv�w���P�������N�{��", 
                                                                  "�z��ۤv�k�ݩ���Ϥ@���l�����N�{��", "��ۤv���ӥͬ����O��", 
                                                                  "�аݱz��i�H���ۤv���w�Ʊ����ɶ����u", "�z��~���a�����ҫ~�誺���N�{��"))) +
  xlab("�~�֤���") +
  ylab("������") +
  scale_x_discrete(limits = 1 : 6, labels= c( "15-24 ��", "25-34 ��", "35-44 ��", "45-54 ��", "55-64 ��", "65 ���H�W")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(0, 15)) +
  ggtitle("���|�pô�P�ͬ��h�����N��") +
  transition_reveal(agegp) 

animate(ani_OECD, height = 500, width = 650, end_pause = 30)
anim_save("images/age_to_weight_v7-20.gif")

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
  scale_color_discrete(name = "�ݨ��D��(�H���װ���C:1��4��)", labels = rev(c("�аݱz��ߪk�|�H���Τ��H���H", "��q�k��פΪk�|?", "���L�������F��?", 
                                                                  "��z�~���a�Ϫ��a��F��?", "��C�骺�~��Τ�����?", "�аݱz��ڰꪺ���|�O�٨�׫H���H���H"))) +
  xlab("�~�֤���") +
  ylab("������") +
  scale_x_discrete(limits = 1 : 6, labels= c( "15-24 ��", "25-34 ��", "35-44 ��", "45-54 ��", "55-64 ��", "65 ���H�W")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(6, 0)) +
  ggtitle("�F�����c�H���{��") + 
  transition_reveal(agegp) 

animate(ani_OECD2, height = 500, width = 650, end_pause = 30)
anim_save("images/age_to_weight_v31-36.gif")

#animated plot 3 v37-v39
meanOECD3 <- aggregate(v37 ~ agegp, nOECD, mean, na.rm = T)
meanOECD3 <- merge(aggregate(v38 ~ agegp, nOECD, mean, na.rm = T), meanOECD3, by = "agegp")
meanOECD3 <- merge(aggregate(v39 ~ agegp, nOECD, mean, na.rm = T), meanOECD3, by = "agegp")
t_meanOECD3 <- reshape2::melt(meanOECD3, id = "agegp")

ani_OECD3 <- t_meanOECD3 %>%
  ggplot(aes(x = agegp, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "�ݨ��D��(�H���װ���C:1��4��)", labels = rev(c("�аݱz�������N�z�b�ڰ�Ҿ֦������D�ͬ��H", "�аݱz�������N�ڰꪺ���צۥ�?", 
                                                                  "�F���x���|�����ڭ̤@��Ѧʩm���Q�k�A�O�_�P�N?"))) +
  xlab("�~�֤���") +
  ylab("������") +
  scale_x_discrete(limits = 1 : 6, labels= c( "15-24 ��", "25-34 ��", "35-44 ��", "45-54 ��", "55-64 ��", "65 ���H�W")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(6, 0)) +
  ggtitle("��a���իH���{��") + 
  transition_reveal(agegp) 

animate(ani_OECD3, height = 500, width = 650, end_pause = 30)
anim_save("images/age_to_weight_v37-39.gif")

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
  scale_color_discrete(name = "�ݨ��D��(���N�קC�찪1��10��)", labels = rev(c("�аݱz�Q��ı�o�ֶּܡH", "�аݱz�Q��ı�o��~�ܡH", "�аݱz�Q��ı�o�q��ܡH", 
                                                                  "�z���ثe�ͬ������N�{��", "���H�ͷ����Ҧ����L���Ʊ��ȱo��?", "�z��ۤv�ͬ����Ǫ����N�{��", 
                                                                  "��ۤv���d���p", "��ۤv�H�ͪ����N", "��ۤv���H�����Y", "�аݱz��ۤv�w���P�������N�{��", 
                                                                  "�z��ۤv�k�ݩ���Ϥ@���l�����N�{��", "��ۤv���ӥͬ����O��", 
                                                                  "�аݱz��i�H���ۤv���w�Ʊ����ɶ����u", "�z��~���a�����ҫ~�誺���N�{��"))) +
  xlab("�Ш|�{�פ���") +
  ylab("������") +
  scale_x_discrete(limits = 1 : 3, labels= c( "�ꤤ�p�ΥH�U", "����(¾)�t���M�e 3 �~", "�j�M�H�W")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(0, 15)) +
  ggtitle("���|�pô�P�ͬ��h�����N��") +
  transition_reveal(edugp) 

animate(ani_OECD4, height = 500, width = 650, end_pause = 30)
anim_save("images/edu_to_weight_v7-20.gif")

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
  scale_color_discrete(name = "�ݨ��D��(�H���װ���C:1��4��)", labels = rev(c("�аݱz��ߪk�|�H���Τ��H���H", "��q�k��פΪk�|?", "���L�������F��?", 
                                                                  "��z�~���a�Ϫ��a��F��?", "��C�骺�~��Τ�����?", "�аݱz��ڰꪺ���|�O�٨�׫H���H���H"))) +
  xlab("�Ш|�{�פ���") +
  ylab("������") +
  scale_x_discrete(limits = 1 : 3, labels= c( "�ꤤ�p�ΥH�U", "����(¾)�t���M�e 3 �~", "�j�M�H�W")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(6, 0)) +
  ggtitle("�F�����c�H���{��") + 
  transition_reveal(edugp) 

animate(ani_OECD5, height = 500, width = 650, end_pause = 30)
anim_save("images/edu_to_weight_v31-36.gif")

#animated plot 6 v37-v39
meanOECD6 <- aggregate(v37 ~ edugp, nOECD, mean, na.rm = T)
meanOECD6 <- merge(aggregate(v38 ~ edugp, nOECD, mean, na.rm = T), meanOECD6, by = "edugp")
meanOECD6 <- merge(aggregate(v39 ~ edugp, nOECD, mean, na.rm = T), meanOECD6, by = "edugp")
t_meanOECD6 <- reshape2::melt(meanOECD6, id = "edugp")

ani_OECD6 <- t_meanOECD6 %>%
  ggplot(aes(x = edugp, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "�ݨ��D��(�H���װ���C:1��4��)", labels = rev(c("�аݱz�������N�z�b�ڰ�Ҿ֦������D�ͬ��H", "�аݱz�������N�ڰꪺ���צۥ�?", 
                                                                  "�F���x���|�����ڭ̤@��Ѧʩm���Q�k�A�O�_�P�N?"))) +
  xlab("�Ш|�{�פ���") +
  ylab("������") +
  scale_x_discrete(limits = 1 : 3, labels= c( "�ꤤ�p�ΥH�U", "����(¾)�t���M�e 3 �~", "�j�M�H�W")) +
  theme(axis.text.x = element_text(angle = 45), axis.text=element_text(size = 18)) +
  ylim(c(6, 0)) +
  ggtitle("��a���իH���{��") + 
  transition_reveal(edugp) 

animate(ani_OECD6, height = 500, width = 650)
anim_save("images/edu_to_weight_v37-39.gif")


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
  scale_color_discrete(name = "�ݨ��D��(1��10��)", labels = rev(c("�аݱz�Q��ı�o�ֶּܡH", "�аݱz�Q��ı�o��~�ܡH", "�аݱz�Q��ı�o�q��ܡH", 
                                                            "�z���ثe�ͬ������N�{��", "���H�ͷ����Ҧ����L���Ʊ��ȱo��?", "�z��ۤv�ͬ����Ǫ����N�{��", 
                                                            "��ۤv���d���p", "��ۤv�H�ͪ����N", "��ۤv���H�����Y", "�аݱz��ۤv�w���P�������N�{��", 
                                                            "�z��ۤv�k�ݩ���Ϥ@���l�����N�{��", "��ۤv���ӥͬ����O��", 
                                                            "�аݱz��i�H���ۤv���w�Ʊ����ɶ����u", "�z��~���a�����ҫ~�誺���N�{��"))) +
  xlab("���J����") +
  ylab("������") +
  scale_x_discrete(limits =c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 125, 175, 250, 350), labels= c( "�L���J", "1�U���H��", "1��2�U��", "2��3�U��", "3��4�U��", "4��5�U��", 
                                                                                                       "5��6�U��", "6��7�U��", "7��8�U��", "8��9�U��", "9��10�U��", "10��15�U��", 
                                                                                                       "15��20�U��", "20��30�U��", "30�U���H�W")) +
  theme(axis.text.x = element_text(angle = 90), axis.text=element_text(size = 10)) +
  ylim(c(0, 15)) +
  ggtitle("���|�pô�P�ͬ��h�����N��") +
  transition_reveal(vI1) 

animate(ani_OECD7, height = 500, width = 900)
anim_save("images/inc_to_weight_v7-20.gif")

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
  scale_color_discrete(name = "�ݨ��D��(�H���װ���C:1��4��)", labels = rev(c("�аݱz��ߪk�|�H���Τ��H���H", "��q�k��פΪk�|?", "���L�������F��?", 
                                                                  "��z�~���a�Ϫ��a��F��?", "��C�骺�~��Τ�����?", "�аݱz��ڰꪺ���|�O�٨�׫H���H���H"))) +
  xlab("���J����") +
  ylab("������") +
  scale_x_discrete(limits =c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 125, 175, 250, 350), labels= c( "�L���J", "1�U���H��", "1��2�U��", "2��3�U��", "3��4�U��", "4��5�U��", 
                                                                                                       "5��6�U��", "6��7�U��", "7��8�U��", "8��9�U��", "9��10�U��", "10��15�U��", 
                                                                                                       "15��20�U��", "20��30�U��", "30�U���H�W")) +
  theme(axis.text.x = element_text(angle = 90), axis.text=element_text(size = 10)) +
  ylim(c(6, 0)) +
  ggtitle("�F�����c�H���{��") + 
  transition_reveal(vI1) 

animate(ani_OECD8, height = 500, width = 900, end_pause = 30)
anim_save("images/inc_to_weight_v31-36.gif")

#animated plot 6 v37-v39
meanOECD9 <- aggregate(v37 ~ vI1, nOECD, mean, na.rm = T)
meanOECD9 <- merge(aggregate(v38 ~ vI1, nOECD, mean, na.rm = T), meanOECD9, by = "vI1")
meanOECD9 <- merge(aggregate(v39 ~ vI1, nOECD, mean, na.rm = T), meanOECD9, by = "vI1")
t_meanOECD9 <- reshape2::melt(meanOECD9, id = "vI1")

ani_OECD6 <- t_meanOECD9 %>%
  ggplot(aes(x = vI1, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  scale_color_discrete(name = "�ݨ��D��(�H���װ���C:1��4��)", labels = rev(c("�аݱz�������N�z�b�ڰ�Ҿ֦������D�ͬ��H", "�аݱz�������N�ڰꪺ���צۥ�?", 
                                                                  "�F���x���|�����ڭ̤@��Ѧʩm���Q�k�A�O�_�P�N?"))) +
  xlab("���J����") +
  ylab("������") +
  scale_x_discrete(limits =c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 125, 175, 250, 350), labels= c( "�L���J", "1�U���H��", "1��2�U��", "2��3�U��", "3��4�U��", "4��5�U��", 
                                                                                                       "5��6�U��", "6��7�U��", "7��8�U��", "8��9�U��", "9��10�U��", "10��15�U��", 
                                                                                                       "15��20�U��", "20��30�U��", "30�U���H�W")) +
  theme(axis.text.x = element_text(angle = 90), axis.text=element_text(size = 10)) +
  ylim(c(6, 0)) +
  ggtitle("��a���իH���{��") + 
  transition_reveal(vI1) 

animate(ani_OECD6, height = 500, width = 900, end_pause = 30)
anim_save("images/inc_to_weight_v37-39.gif")