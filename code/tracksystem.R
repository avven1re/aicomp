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


tracksys <- function(id, per, times, As = "parsed"){
  #URLpatse <- paste0("https://api.steampowered.com/ISteamUserStats/GetNumberOfCurrentPlayers/v1/?appid=", id)
  gamevec <- matrix(NaN, length(id), times)
  rectimevec <- rep(NaN, times)
  for (i in 1 : times) {
    if(i != 1){Sys.sleep(per)}
    
      for (k in 1 : length(id)) {
      
    
      URLpatse <- paste0("https://api.steampowered.com/ISteamUserStats/GetNumberOfCurrentPlayers/v1/?appid=", id[k])
      game_api_source <- GET(URLpatse)
      game_api <- content(game_api_source, as = As)
      gamevec[k, i] <- as.numeric(unlist(game_api))[1]
      cat("ID:", id[k], "的第", i, "次紀錄完成.", "時間:", as.character(Sys.time()), ".", "在線人數:", gamevec[k, i], "\n")

      }
    rectimevec[i] <- as.character(Sys.time())
    cat("\n ", "------------------分隔線---------------------", "\n", "\n")
  }
  out <- list(gamevec, rectimevec)
  return(out)
  #gamevec
}

idl <- c(730, 570, 578080, 1085660, 359550, 271590, 230410, 252490, 1100600, 440, 252950, 
         346110, 218, 218620, 4000, 105600, 381210, 227300, 289070, 872790)

gametracklist <- tracksys(idl, 1800, times = 24)

gametrackmat <- as.data.frame(gametracklist[[1]])
names(gametrackmat) <- gametracklist[[2]]
gametrackmat2 <- cbind(idl, gametrackmat)

write.csv(gametrackmat2, file = "dataset/gametrack1128.csv")
