## packages
library(sf);library(gridExtra);library(ggplot2);library(dplyr);
# library(viridis);library(fieldss)
## 問卷
ques = read.csv("dataset/Happiness.csv")  
ques[,1] <- 109-ques[,1]
names(ques)[1:41] <- c("age",paste('q',1:38,sep=''),"marrgp","incogp")
names(ques)[46] <- "TOWNCODE"
head(ques)

## 土地利用資料
ld1=read.csv("dataset/landdata.csv", skip=1, header=F, fileEncoding = "UTF-8-BOM")
for (i in 1:dim(ld1)[1]){
  for (j in 4:dim(ld1)[2]){
    ld1[i,j] <- ld1[i,j]/ld1[i,3]
  }
}
# head(ld1)
ld2 <- cbind(ld1[,1:3], rowSums(ld1[,4:9]), rowSums(ld1[,10:16]), rowSums(ld1[,17:27])
             , rowSums(ld1[,28:38]), rowSums(ld1[,39:47]), rowSums(ld1[,48:53])
             , rowSums(ld1[,54:57]), rowSums(ld1[,58:60]), rowSums(ld1[,61:66]))
names(ld2) <- c("county","town","area","agri","forest","traffic"
                ,"water","building","gov","leisure","mine","other")
head(ld2)
## 鄉鎮分區界線
taiwan.town.map <- st_read("dataset/town/TOWN_MOI_1070205.shp")
head(taiwan.town.map)

ntw.map <- taiwan.town.map[c("TOWNNAME", "geometry", "TOWNCODE")]
head(ntw.map)

## 將土地的資料合併
truemap <- left_join(ntw.map, ld2,
                     by= c("TOWNNAME"= "town"))
head(truemap)
class(truemap)

## 畫圖
# plt1 <- ggplot(data = truemap) +
#   geom_sf(aes(fill = V13/V4)) +
# #  geom_sf_text(aes(label = TOWNNAME), size = 3) +
#   scale_fill_distiller(palette = "YlGn", direction = 1, name = "闊葉林") +
#   labs(title="分布圖", x ="經度", y = "緯度")
# plt1

## Merge結果
dataA <- merge(truemap, ques, all=TRUE)

## 分ABC
head(dataA)
set.seed(100)
dataB = dataA[sample(nrow(dataA),100),]
head(dataB)
set.seed(200)
dataC = dataA[sample(nrow(dataA),33),]
head(dataC)

