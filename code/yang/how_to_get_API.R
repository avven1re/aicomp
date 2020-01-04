#How to get API form Steamspy

library(jsonlite)
library(httr)

browseURL("https://steamspy.com/api.php") #introdution
browseURL("https://steamdb.info/") # search app id in Steam



#examples
##csgo_api <- GET("steamspy.com/api.php?request=appdetails&appid=730") # CSGO's app id : 730

##csgo <- content(csgo_api, as = "parsed") #?content for details. use <as = "parsed"> to make a list for R

##csgo #done


#function
gameAPI <- function(id, As = "parsed"){
  #As = c("raw", "text", "parsed") type ?content and see Arguments : as for more details
  
  URLpatse <- paste0("steamspy.com/api.php?request=appdetails&appid=", id)
  game_api_source <- GET(URLpatse)
  game_api <- content(game_api_source, as = As)
  
  if(sum(is.null(game_api$name)) == 1){
    stop("\n", "Browse URL:", 
    select <- askYesNo("ID not found, Do you want to search it on Steamdb? (https://steamdb.info/)"), 
    if(select == 1) browseURL("https://steamdb.info/"), "\n",  "Error : App ID is missing, please import correctly or find it at https://steamdb.info/")
    }
  
  game_api
}

#csgo <- gameAPI(730)