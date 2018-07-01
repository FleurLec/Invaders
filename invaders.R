library(leaflet)
library(htmltools)
library(dplyr)
library(xml2)
library(rvest)

##### BUILD INVADERS LOGO ###########################################################

invader.icon <- iconList(
                  got.it = makeIcon(iconUrl = "./img/invaders-green-logo.png",   iconWidth = 20, iconHeight = 20),
                  not.yet = makeIcon(iconUrl = "./img/invaders-pink-logo.png",   iconWidth = 20, iconHeight = 20),
                  not.inv = makeIcon(iconUrl = "./img/not-invader.png",   iconWidth = 20, iconHeight = 20)
                  )


## SCRAP INVADERS ADRESS LIST #######################################################


url <- "http://wiki.battle.paris/index.php?title=Space_Oddity"
url.sparse <- xml2::read_html(url)

url.list <- rvest::html_text(rvest::html_nodes(x= url.sparse, xpath='//*[@class="new"]'))

# get invaders name and invader address
invaders.df <- data.frame(invader = substr(url.list, 1, regexpr(",", url.list)[1]-1),
                          address = substr(url.list, regexpr(",", url.list)[1]+1, nchar(url.list)))


# save list in a csv
write.csv(invaders.df, file = "./data/invaders.csv")



## GEOCODE PLACES  ##################################################################

# read adres file
invaders.df <- read.csv2(file = "./data/invaders.clean.csv")

# source geocoding code
source("source_geocode.R")


# apply geocoding to each line
invaders.geo <- apply( as.data.frame(invaders.df$address),1,  gGeoCodeBing)

# cat to previous file
invader.final <- cbind(invaders.df, invaders.geo)

# save
write.csv2(invader.final, file = "./data/invaders.geo.csv")



## DRAW ON A MAP #####################################################################

## note that some adjustments have been made directly in the csv files (cleaning some
## adresses and duplicating line to get 1 line per invader.
## should be made directly in R using regex. to be finetuned....

# read cleaned file
invader.map <- read.csv2( file = "./data/invaders.geo.clean.csv", stringsAsFactors = F)

# tag status
invader.map <- invader.map %>% mutate( status = ifelse(artist == "PA", "not.yet", "not.inv"))


# read alreadly flashed invaders
invader.flash <- read.csv2( file = "./data/flash_invaders.csv", stringsAsFactors = F)
invader.flash$status2="got.it"

# merge already flashed and to be flashed
invader.final <- invader.map %>% 
                 left_join(invader.flash, by="code") %>% 
                 mutate(status = ifelse(!is.na(status2), status2, status)) %>%
                 select(-status2)


table(invader.final$artist, invader.final$status)


# clean format for lat/long
options(digits=9)
invader.final$lon <- as.numeric(invader.map$lon)
invader.final$lat <- as.numeric(invader.map$lat)

lon.med <- as.numeric(quantile(invader.final$lon, .5))
lat.med <- as.numeric(quantile(invader.final$lat, .5))

## DRAW LOCATION ON A MAP

leaflet(data = invader.final) %>%
  addTiles() %>%
  setView(lng = lon.med, lat = lat.med, zoom = 12) %>%
  addMarkers( ~lon, ~lat, icon = ~invader.icon[status], popup = ~htmlEscape(paste(code, address)))


