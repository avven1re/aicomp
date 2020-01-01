#test wu
library(stringr)
Pop <- read.csv("dataset/Pop.csv", skip=1, nrow=368,
                col.names = c("y", "t", "p", "a", "d"),
                fill = TRUE, fileEncoding = "UTF-8-BOM")
t1 <- str_sub(Pop$t, 1, 3)
t2 <- str_sub(Pop$t, 4, 7)

Land <- read.csv("dataset/Landuse.csv", skip=1,
                 header = F, nrow=368,fill = TRUE, 
                 fileEncoding = "UTF-8-BOM")


