twitch <- GET("https://api.twitch.tv/helix/analytics/games?first=5")

t2 <- GET("https://api.twitch.tv/helix/analytics/games?type=overview_v2&game_id=493057&started_at=2018-01-01T00:00:00Z&ended_at=2018-03-01T00:00:00Z")
t21 <- content(t2, as = "parsed")
t21

browseURL(af$box_art_url)

user <- get_users("vpkmufrbg4dt182aiotamw302jjy6f", user_login = "keithkevin")

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

mhw <- GET("https://api.twitch.tv/helix/analytics/games", query = list(id = 497467))
mhw2 <- content(mhw, as = "parsed")
mhw2







