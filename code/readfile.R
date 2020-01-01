# packages
library(sf);library(gridExtra);library(ggplot2);library(dplyr);
# library(viridis);library(fieldss)
# 問卷
ques = read.csv("dataset/Happiness.csv")  
ques[,1] <- 109-ques[,1]
names(ques)[1:41] <- c("age",paste('q',1:38,sep=''),"marrgp","incogp")
names(ques)[46] <- "TOWNCODE"
head(ques)

# land data
ld1=read.csv("data/landdata.csv", skip=1, header=F, fileEncoding = "UTF-8-BOM")
head(ld1)
ld2 <- ld1[-1]
names(ld2) <- c("county","town",paste('x',1:64,sep=''))
head(ld2)

# 鄉鎮分區
taiwan.town.map <- st_read("data/town/TOWN_MOI_1070205.shp")
head(taiwan.town.map)

ntw.map <- taiwan.town.map[c("TOWNNAME", "geometry", "TOWNCODE")]
head(ntw.map)
# mode(taiwan.town.map)
# town <- cbind(as.vector(taiwan.town.map$TOWNNAME),as.vector(taiwan.town.map$TOWNCODE))
# town <- as.data.frame(taiwan.town.map[c("TOWNNAME", "TOWNCODE")])
# head(town)

# 將土地的資料合併
truemap <- left_join(ntw.map, ld2,
              by= c("TOWNNAME"= "town"))
head(truemap)
class(truemap)

# 畫圖
# plt1 <- ggplot(data = truemap) +
#   geom_sf(aes(fill = V13/V4)) +
# #  geom_sf_text(aes(label = TOWNNAME), size = 3) +
#   #scale_fill_distiller(palette = "Spectral", name = "人口(???)") +
#   #scale_fill_gradientn(colours = tim.colors(22), name = "人口(???)") +
#   #scale_fill_viridis(name = "人口(???)") +
#   #scale_fill_distiller(palette = "YlOrRd", name = "人口(???)") +
#   scale_fill_distiller(palette = "YlGn", direction = 1, name = "?????????") +
#   labs(title="?????????", x ="經度", y = "緯度")
# plt1


# final.dat <- left_join(ques, truemap, by= c("t"="TOWNCODE"))
# final.dat <- left_join(ques, town, by= c("t"='[,1]'))

# 總和
tm <- as.data.frame(truemap)
names(tm)
mer.dat <- merge(tm,ques,by="TOWNCODE")
# 分ABC
dataA <- mer.dat[, -68]
head(dataA)
set.seed(100)
dataB = dataA[sample(nrow(dataA),100),]
head(dataB)
set.seed(200)
dataC = dataA[sample(nrow(dataA),33),]
