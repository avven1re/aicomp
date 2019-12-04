#API required packages

install.packages("jsonlite")
library(jsonlite)

install.packages("httr")
library(httr)

install.packages(Rcurl)
library(RCurl)

install.packages("RGA")
library(RGA)

library(devtools)

#browseURL("https://github.com/Freguglia/rTwitchAPI)
devtools::install_github("Freguglia/rTwitchAPI")
library(rTwitchAPI)


