library(leaflet)
library(htmltools)
library(dplyr)
library(xml2)
library(rvest)

##### BUILD INVADERS LOGO ###########################################################

file.logo <- "invaders-green-logo.png"
invader.icon <- makeIcon(iconUrl = file.logo,   iconWidth = 20, iconHeight = 20)



## SCRAP INVADERS ADRESS LIST #######################################################


url <- "http://wiki.battle.paris/index.php?title=Space_Oddity"
url.sparse <- xml2::read_html(url)

url.list <- rvest::html_text(rvest::html_nodes(x= url.sparse, xpath='//*[@class="new"]'))

# get invaders name and invader address
invaders.df <- data.frame(invader = substr(url.list, 1, regexpr(",", url.list)[1]-1),
                          address = substr(url.list, regexpr(",", url.list)[1]+1, nchar(url.list)))


# save list in a csv
write.csv(invaders.df, file = "invaders.csv")


invaders.df <- read.csv2(file = "invaders.clean.csv")

## GEOCODE LOCATIONS

source("source_geocode.R")


#init



invaders.geo <- apply( as.data.frame(invaders.df$address),1,  gGeoCodeBing)


invader.final <- cbind(invaders.df, invaders.geo)


write.csv2(invader.final, file = "invaders.geo.csv")

#DF.invaders.geo <- as.data.frame(apply(rbind(DF.invaders, DF.invaders.init), 2, geocodeT))
####DF.invaders.geo <- rbind(DF.invaders.geo, as.data.frame(apply(DF.invaders, 2, geocodeT)))

invader.map <- read.csv2( file = "invaders.geo.clean.csv", stringsAsFactors = F)
str(invader.map)
options(digits=9)
invader.map$lon <- as.numeric(invader.map$lon)
invader.map$lat <- as.numeric(invader.map$lat)

## DRAW LOCATION ON A MAP

m <- leaflet(data = invader.map) 
m <- m  %>% addProviderTiles("Esri.WorldImagery")
#m <- m %>% addTiles() 
m <- m %>% addMarkers(~lat, ~lon, icon = invader.icon, popup = ~htmlEscape(paste(invader, address, invaders.geo)))
m

install.packages("caTools")
