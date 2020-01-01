# packages
library(sf);library(gridExtra);library(ggplot2);library(dplyr);
library(viridis);library(fieldss)
# 問卷
# dataA = read.csv("data/Happiness.csv")
# set.seed(100)
# dataB = dataA[sample(nrow(dataA),50),]
# set.seed(200)
# dataC = dataA[sample(nrow(dataA),20),]
ques = read.csv("data/Happiness.csv")
head(ques)
# land data
ld1=read.csv("data/landdata.csv", skip=1, header=F, fileEncoding = "UTF-8-BOM")
head(ld1)
# 鄉鎮分布
taiwan.town.map <- st_read("data/town/TOWN_MOI_1070205.shp")
head(taiwan.town.map)
twplt <- ggplot(data = taiwan.town.map) +
  geom_sf(aes(fill = TOWNNAME), show.legend= F) +
#  geom_sf_text(aes(label = TOWNNAME), size = 3) +
  labs(title = "台灣行政區圖")
twplt
ntw.map <- taiwan.town.map[c("TOWNNAME", "geometry", "TOWNCODE")]
head(ntw.map)
mode(taiwan.town.map)
town <- cbind(as.vector(taiwan.town.map$TOWNNAME),as.vector(taiwan.town.map$TOWNCODE))
head(town)
# townf <- cbind(as.vector(taiwan.town.map$TOWNNAME),as.vector(taiwan.town.map$TOWNCODE),taiwan.town.map$geometry)
# head(townf)
# 地圖與土地資料合併
truemap <- left_join(ntw.map, ld1,
              by= c("TOWNNAME"= "V3"))
head(truemap)
class(truemap)
truemap <- as.data.frame(truemap)

# 成品圖
plt1 <- ggplot(data = truemap) +
  geom_sf(aes(fill = V13/V4)) +
#  geom_sf_text(aes(label = TOWNNAME), size = 3) +
  #scale_fill_distiller(palette = "Spectral", name = "人口(萬)") +
  #scale_fill_gradientn(colours = tim.colors(22), name = "人口(萬)") +
  #scale_fill_viridis(name = "人口(萬)") +
  #scale_fill_distiller(palette = "YlOrRd", name = "人口(萬)") +
  scale_fill_distiller(palette = "YlGn", direction = 1, name = "闊葉林") +
  labs(title="分佈圖", x ="經度", y = "緯度")
plt1


head(ques)
class(ques$v4)

final.dat <- left_join(ques, truemap, by= c("t"="TOWNCODE"))
