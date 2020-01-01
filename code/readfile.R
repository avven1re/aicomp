# packages
library(sf);library(gridExtra);library(ggplot2);library(dplyr);
# library(viridis);library(fieldss)
# °Ý¨÷
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

# ??‰éŽ®??†å??
taiwan.town.map <- st_read("data/town/TOWN_MOI_1070205.shp")
head(taiwan.town.map)
# twplt <- ggplot(data = taiwan.town.map) +
#   geom_sf(aes(fill = TOWNNAME), show.legend= F) +
# #  geom_sf_text(aes(label = TOWNNAME), size = 3) +
#   labs(title = "?°?£è¡Œæ”¿??€???")
# twplt
ntw.map <- taiwan.town.map[c("TOWNNAME", "geometry", "TOWNCODE")]
head(ntw.map)
# mode(taiwan.town.map)
# town <- cbind(as.vector(taiwan.town.map$TOWNNAME),as.vector(taiwan.town.map$TOWNCODE))
# town <- as.data.frame(taiwan.town.map[c("TOWNNAME", "TOWNCODE")])
# head(town)

# townf <- cbind(as.vector(taiwan.town.map$TOWNNAME),as.vector(taiwan.town.map$TOWNCODE),taiwan.town.map$geometry)
# head(townf)
# ?œ°??–è?‡å?Ÿåœ°è³‡æ?™å?ˆä½µ
truemap <- left_join(ntw.map, ld2,
              by= c("TOWNNAME"= "town"))
head(truemap)
class(truemap)

# ??å?å??
plt1 <- ggplot(data = truemap) +
  geom_sf(aes(fill = V13/V4)) +
#  geom_sf_text(aes(label = TOWNNAME), size = 3) +
  #scale_fill_distiller(palette = "Spectral", name = "äººå£(?¬)") +
  #scale_fill_gradientn(colours = tim.colors(22), name = "äººå£(?¬)") +
  #scale_fill_viridis(name = "äººå£(?¬)") +
  #scale_fill_distiller(palette = "YlOrRd", name = "äººå£(?¬)") +
  scale_fill_distiller(palette = "YlGn", direction = 1, name = "??Šè?‰æ??") +
  labs(title="??†ä?ˆå??", x ="ç¶“åº¦", y = "ç·¯åº¦")
plt1


# final.dat <- left_join(ques, truemap, by= c("t"="TOWNCODE"))
# final.dat <- left_join(ques, town, by= c("t"='[,1]'))

#
tm <- as.data.frame(truemap)
names(tm)
mer.dat <- merge(tm,ques,by="TOWNCODE")
dataA <- mer.dat[, -68]
head(dataA)
set.seed(100)
dataB = dataA[sample(nrow(dataA),100),]
head(dataB)
set.seed(200)
dataC = dataA[sample(nrow(dataA),33),]
