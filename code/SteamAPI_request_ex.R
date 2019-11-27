#steamAPI request examples
browseURL("https://steamwebapi.azurewebsites.net/")

#current players
csgopl_source <- GET("https://api.steampowered.com/ISteamUserStats/GetNumberOfCurrentPlayers/v1/?appid=730")
csgopl <- content(csgopl_source, as = "parsed")

csgoa <- GET("https://api.steampowered.com/ISteamUserStats/GetUserStatsForGame/v2/?appid=730&key=D326796762E1279B72D7B08EBB0F3117&steamid=76561198258206827", )
csgob <- content(csgoa, as = "parsed")

#personal status
a <- GET("https://api.steampowered.com/ISteamUser/GetPlayerSummaries/v2/?key=D326796762E1279B72D7B08EBB0F3117&steamids=76561198258206827")
a2 <- content(a, as = "parsed")
a2

#friendList
b <- GET("https://api.steampowered.com/ISteamUser/GetFriendList/v1/??key=D326796762E1279B72D7B08EBB0F3117&steamid=76561198258206827")
b2 <- content(b, as = "parsed")
b2?content