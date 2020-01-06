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

a_cor <-cor(dataA[, 14:37], use = "complete.obs")
b_cor <-cor(dataB[, 14:37], use = "complete.obs")
c_cor <-cor(dataC[, 14:37], use = "complete.obs")
write.csv(a_cor, "dataset/dataA_cor_matrix")
write.csv(b_cor, "dataset/dataB_cor_matrix")
write.csv(c_cor, "dataset/dataC_cor_matrix")

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