# encoding : UTF-8
install.packages("DataExplorer")
library(DataExplorer)



plot_boxplot(dataA[, 4], by = "agegp")


library(plotly)

p <- plot_ly(
  x = c("giraffes", "orangutans", "monkeys"),
  y = c(20, 14, 23),
  name = "SF Zoo",
  type = "bar"
)

p


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
    ggsave(paste0("images/age_with_dataA_", names(dataset)[i], ".png"))}
  return(pl)
}

ageOECD(14, dataA)

for (i in 14 : 37) {
  ageOECD(i, dataA, save = T)
}










