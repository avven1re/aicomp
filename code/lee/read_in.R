## packages=======================================================
pkg <- c("sf", "gridExtra", "dplyr", "FactoMineR", "factoextra", 
         "corrplot", "vegan","missMDA", "stats", "NbClust", "dr", "glmnet")
lapply(pkg, library, character.only=T)

## Reading & Cleaning Data ========================================
# Input Happiness------------------------------------------------
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
ques <- ques[is.na(ques$TOWNCODE) == 0, ]

# Input landdata & Calculate------------------------------------- 
land=read.csv("dataset/landdata.csv", skip=1, header=F, 
              fileEncoding = "UTF-8-BOM")
names(land) <- c("county","town","area", paste('l',1:63,sep=''))
for (i in 1:dim(land)[1]){
  for (j in 4:dim(land)[2]){
    land[i,j] <- 1000*land[i,j]/land[i,3]
  }
}

# Input Shapefile------------------------------------------------
taiwan.town.map <- st_read("dataset/town/TOWN_MOI_1070205.shp")
ntw.map <- as.data.frame(
  taiwan.town.map[c("COUNTYNAME","TOWNNAME","TOWNCODE")])
ntw.map <- ntw.map[,1:3]

## Merging and Sampling Data ======================================
# Merge landdata & Shapefile & Happiness-------------------------
truemap <- left_join(ntw.map, land,
                     by= c("COUNTYNAME"="county","TOWNNAME"="town"))
dataA <- merge(truemap, ques, by = "TOWNCODE", all.ques=T)
dataA <- dataA[, c(-(1:4))]

# Sampling ABC---------------------------------------------------
# dim(dataA)
# head(dataA)
set.seed(100)
dataB = dataA[sample(nrow(dataA),100),]
# head(dataB)
set.seed(200)
dataC = dataA[sample(nrow(dataA),33),]
# head(dataC)
