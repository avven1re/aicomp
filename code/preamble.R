## packages
map.pkg <- c("sf", "gridExtra", "dplyr")
#install.packages(pca.pkg)
lapply(map.pkg, library, character.only=T)

## Input Happiness
ques = read.csv("dataset/Happiness.csv")
ques <- ques[, -1]
names(ques)[1:40] <- c(paste('q',1:38,sep=''),"marrgp","incogp")
names(ques)[45] <- "TOWNCODE"
keeps <- c(paste('q',5:19,sep=''), paste('q',30:38,sep=''),
           "marrgp", "incogp", "sexgp", "agegp", "edugp", 
           "weight", "TOWNCODE")
ques <- ques[keeps]
names(ques)[1:6] <- c(paste('a',1:6,sep=''))
names(ques)[7:15] <- c(paste('b',1:9,sep=''))
names(ques)[16:24] <- c(paste('c',1:9,sep=''))

## Input landdata & Calculate 
ld1=read.csv("dataset/landdata.csv", skip=1, header=F, 
             fileEncoding = "UTF-8-BOM")
for (i in 1:dim(ld1)[1]){
  for (j in 4:dim(ld1)[2]){
    ld1[i,j] <- ld1[i,j]/ld1[i,3]
  }
}
ld2 <- cbind(ld1[,1:3], rowSums(ld1[,4:9]), rowSums(ld1[,10:16]), rowSums(ld1[,17:27])
             , rowSums(ld1[,28:38]), rowSums(ld1[,39:47]), rowSums(ld1[,48:53])
             , rowSums(ld1[,54:57]), rowSums(ld1[,58:60]), rowSums(ld1[,61:66]))
names(ld2) <- c("county","town","area","agri","forest","traffic"
                ,"water","building","gov","leisure","mine","other")

## Input Shapefile
taiwan.town.map <- st_read("dataset/town/TOWN_MOI_1070205.shp")
ntw.map <- taiwan.town.map[c("COUNTYNAME","TOWNNAME", "geometry", "TOWNCODE")]

## Merge landdata & Shapefile
truemap <- left_join(ntw.map, ld2,
                     by= c("COUNTYNAME"="county","TOWNNAME"="town"))

## Merge landdata & Shapefile & Happiness
dataA <- merge(truemap, ques, by = "TOWNCODE", all.ques=T)


## ABC
head(dataA)
set.seed(100)
dataB = dataA[sample(nrow(dataA),100),]
head(dataB)
set.seed(200)
dataC = dataA[sample(nrow(dataA),33),]
head(dataC)



## 畫圖
# plt1 <- ggplot(data = truemap) +
#   geom_sf(aes(fill = V13/V4)) +
# #  geom_sf_text(aes(label = TOWNNAME), size = 3) +
#   scale_fill_distiller(palette = "YlGn", direction = 1, name = "闊葉林") +
#   labs(title="分布圖", x ="經度", y = "緯度")
# plt1

