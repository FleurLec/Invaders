##############################
##       CREATE TRIPS       ##
##############################

## Enter a list of POI (with GPX coordinates)
## Specify a starting and end point
## specify a number of kilometers
## Out : a trip

# devtools::install_github("GIScience/openrouteservice-r")
# install.packages("sfheaders")

library(tmaptools)
library(openrouteservice)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(htmltools)

token <- readLines("./OSMtoken")
ors_api_key(token)
load(file="data/invader_final.Rdata")


#### >> CHOOSE YOUR ADDRESS IN  <<
  myAdr <- "1 rue du commerce, 75015 Paris"
  bbox.km <- 1
  nbmax.invader <- 30
#### >>>>>> <<<<<<
  
start <- geocode_OSM(myAdr)
  
## 1) Select invaders in a box framing it
## 0.01 pt de longitude (2.38) = 0,734km -> 1km = 0.01/0.734
## 0.01 pt de latitude (48.85) = 1,112km -> 1km = 0.01/1.112

  lon.start <- start$coords["x"]
  lat.start <- start$coords["y"]
  lonW <- start$coords["x"] - bbox.km*0.01/0.734
  lonE <- start$coords["x"]  + bbox.km*0.01/0.734
  latN <- start$coords["y"]  - bbox.km*0.01/1.112
  latS <- start$coords["y"]  + bbox.km*0.01/1.112
  
  cat(paste("coin : lat/lon", latS, ",", lonW ))
  cat(paste("coin : lat/lon", latN, ",", lonE ))
  cat(paste("centre :", start$coords["y"], ",", start$coords["x"]))  

  
## Get invaders list and select distance to the center

  invader.select <- invader.final %>% filter(latN < lat & lat < latS & lonW < lon & lon < lonE)
  
  invader.select <- invader.select %>% mutate(dist = sqrt( (lon.start - lon)^2 + (lat.start - lat)^2 )) %>% # distance entre invaders & centre
                                       arrange(dist) %>% 
                                       slice(1:nbmax.invader)

## Draw map of invaders
  
  invader.icon <- iconList(
    Got.it = makeIcon(iconUrl = "./img/invaders-green-logo.png",   iconWidth = 20, iconHeight = 20),
    Not.yet = makeIcon(iconUrl = "./img/invaders-pink-logo.png",   iconWidth = 20, iconHeight = 20),
    Not.invader = makeIcon(iconUrl = "./img/not-invader.png",   iconWidth = 20, iconHeight = 20),
    Desactivated = makeIcon(iconUrl = "./img/desactivated.png",   iconWidth = 20, iconHeight = 20)
  )
   start.icon = makeIcon(iconUrl = "./img/start.png",   iconWidth = 20, iconHeight = 20)

  # clean format for lat/long
  options(digits=9)
  invader.select$lon <- as.numeric(invader.select$lon)
  invader.select$lat <- as.numeric(invader.select$lat)
  
  lon.med <- as.numeric(quantile(invader.select$lon, .5))
  lat.med <- as.numeric(quantile(invader.select$lat, .5))
  
  #lat.med <-  48.867114 
  #lon.med <- 2.358201
  ## DRAW LOCATION ON A MAP
  
  ## Add routes
  
  x <- invader.select %>% select(lon, lat) %>% ors_directions(ors_profile(mode = "walking"))
  
  leaflet(data = invader.select, width = "100%") %>%
    addTiles() %>%
    setView(lng = lon.med, lat = lat.med, zoom = 13) %>%
    addMarkers( ~lon, ~lat, icon = ~invader.icon[status], popup = ~htmlEscape(paste(code, address))) %>%
    addMarkers(lon.start, lat.start, icon = start.icon) %>%
    addFullscreenControl()  %>%
    addGeoJSON(x, fill=FALSE) %>%
    fitBBox(x$bbox)
  
  
## need to add a travelling salesman problem implementation here :)
## check out https://www.r-orms.org/mixed-integer-linear-programming/practicals/problem-tsp/
  
  