twitch <- GET("https://api.twitch.tv/helix/analytics/games?first=5")


twitch_api <- content(twitch, as = "parsed")
twitch_api

as <- get_streams(first = 15, language = "en")
as

Sys.getenv("R_LIBS_USER")

install.packages("digest")
# install.packages("devtools")
devtools::install_github("Freguglia/rTwitchAPI")
library(rTwitchAPI)

twitch_auth("vpkmufrbg4dt182aiotamw302jjy6f")
?twitch_auth

a <- GET("https://api.twitch.tv/helix/streams")

twi <- content(a, as = "parsed")
twi

httr::oauth1.0_token(endpoint = "topgame", app = "twitch", key = getOption("vpkmufrbg4dt182aiotamw302jjy6f"))








