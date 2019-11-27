#tracking system

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


tracksys <- function(id, track_gap, As = "parsed", t){
  URLpatse <- paste0("https://api.steampowered.com/ISteamUserStats/GetNumberOfCurrentPlayers/v1/?appid=", id)
  gamevec <- rep(NaN, t)
  i <- 1
  
  while (i <= t) {
  
  game_api_source <- GET(URLpatse)
  game_api <- content(game_api_source, as = As)
  gamevec[i] <- as.numeric(unlist(game_api))[1]
  cat("第", i, "次紀錄完成.", "時間:", Sys.time(),". 在線人數:", gamevec[i], "\n")
  if(i != t){
  Sys.sleep(track_gap)
  }
  i <- i + 1
  }
  gamevec
}

csgo_cur2 <- tracksys(730, 50, t = 3)
plot(csgo_cur2, type = "l")

Sys.time()