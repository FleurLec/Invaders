library(leaflet)
library(htmltools)
library(dplyr)
library(xml2)
library(rvest)
library(knitr)

##### BUILD INVADERS LOGO ###########################################################

invader.icon <- iconList(
  Got.it = makeIcon(iconUrl = "./img/invaders-green-logo.png",   iconWidth = 20, iconHeight = 20),
  Not.yet = makeIcon(iconUrl = "./img/invaders-pink-logo.png",   iconWidth = 20, iconHeight = 20),
  Not.invader = makeIcon(iconUrl = "./img/not-invader.png",   iconWidth = 20, iconHeight = 20),
  Desactivated = makeIcon(iconUrl = "./img/desactivated.png",   iconWidth = 20, iconHeight = 20)
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
invader.all <- read.csv2( file = "./data/invaders.geo.clean.csv", stringsAsFactors = F)

# tag status
invader.all <- invader.all %>% 
  mutate( status = ifelse(artist == "PA", "Not.yet", "Not.invader")) 


# read alreadly flashed invaders
invader.flash <- read.csv2( file = "./data/flash_invaders.csv", stringsAsFactors = F)
invader.flash$status2="Got.it"

# merge already flashed and to be flashed
invader.all2 <- invader.all %>% 
  left_join(invader.flash, by="code") %>% 
  mutate(status = ifelse(!is.na(status2), status2, status)) %>%
  select(-c(status2, dtmaj))


# desactivated
invader.des <- read.csv2( file = "./data/desactivated.csv", stringsAsFactors = F)
invader.des$status2="Desactivated"

# merge desactivated
invader.final <- invader.all2 %>% 
  left_join(invader.des, by="code") %>% 
  mutate(status = ifelse(!is.na(status2), status2, status)) %>%
  select(-c(status2, dtmaj))


# flashed but not on the initial list
invader.add <-  anti_join(invader.flash, invader.all, by="code") %>% 
  mutate(status = status2) %>%
  select(-status2)

write.csv2(invader.add, file = "./data/invader.add.csv")
# find address back

invader.add <- read.csv2("./data/invader.add.clean.csv")



invader.final <- invader.final %>% select(c(code, status, address, lat, lon)) %>% rbind(invader.add)


# clean format for lat/long
options(digits=9)
invader.final$lon <- as.numeric(invader.final$lon)
invader.final$lat <- as.numeric(invader.final$lat)

lon.med <- as.numeric(quantile(invader.final$lon, .5))
lat.med <- as.numeric(quantile(invader.final$lat, .5))

## DRAW LOCATION ON A MAP

leaflet(data = invader.final, width = "100%") %>%
  addTiles() %>%
  setView(lng = lon.med, lat = lat.med, zoom = 12) %>%
  addMarkers( ~lon, ~lat, icon = ~invader.icon[status], popup = ~htmlEscape(paste(code, address)))

## Table to count

count.invaders <- invader.final %>% group_by(status) %>% summarize(n = n())
kable(count.invaders)

