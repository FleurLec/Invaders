library(mapview)
library(OpenStreetMap)
library(xml2)
library(leaflet)
library(tidyverse)
library(htmltools)
library(webshot2)


load("./data/OUT_invader_final.Rdata")

size = 18
invader.icon.xs <- iconList(
  Got.it = makeIcon(iconUrl = "./img/invaders-green-logo.png",   iconWidth = size, iconHeight = size),
  Not.yet = makeIcon(iconUrl = "./img/invaders-pink-logo.png",   iconWidth = size, iconHeight = size),
  Not.yet.inside = makeIcon(iconUrl = "./img/invaders-yellow-logo.png",   iconWidth = size, iconHeight = size),
  Not.invader = makeIcon(iconUrl = "./img/not-invader.png",   iconWidth = size, iconHeight = size),
  Desactivated = makeIcon(iconUrl = "./img/desactivated.png",   iconWidth = size, iconHeight = size)
)


# clean format for lat/long
invader.final$lon <- as.numeric(invader.final$lon)
invader.final$lat <- as.numeric(invader.final$lat)

lat.center = 48.858883
lon.center = 2.339990

# Remise a Not.yet

invader.paris <- invader.final %>% filter(str_sub(code, 1, 3) == "PA_") %>%
                                   mutate(status.time = if_else(status == "Got.it", "Not.yet", status))

m <- invader.paris %>% 
  leaflet( width = 1000,  height = 1000) %>%
  addTiles() %>%
  setView(lat = lat.center, lng = lon.center, zoom = 13) %>%
  addMarkers( ~lon, ~lat, 
              icon = ~invader.icon.xs[status.time],
              popup = ~htmlEscape(paste(code, invaders.geo)) ) 


htmlwidgets::saveWidget(widget = m, file = paste0("./html/Invaders_000000.html"))
webshot(url = paste0("./html/Invaders_000000.html"), file = paste0("./png/sortie/Invaders_000000.png"), vwidth = 1000, vheight = 1000)
webshot(url = paste0("./html/Invaders_000000.html"), file = paste0("./png/mois/Invaders_000000.png"), vwidth = 1000, vheight = 1000)


## Par mois ######################################################################################

t <- invader.paris %>% group_by(status, dt_flash) %>% tally()

for (dt in unique(t$dt_flash)[!is.na(unique(t$dt_flash))]) {
  
  m <- invader.paris %>% 
    mutate(status.time = if_else(dt_flash <= dt & !is.na(dt_flash), "Got.it", status.time)) %>%
    leaflet( width = 1000,  height = 1000) %>%
    addTiles() %>%
    setView(lat = lat.center, lng = lon.center, zoom = 13) %>%
    addMarkers( ~lon, ~lat, 
                icon = ~invader.icon.xs[status.time],
                popup = ~htmlEscape(paste(code, invaders.geo)) ) 
  
  
  htmlwidgets::saveWidget(widget = m, file = paste0("./html/Invaders_", dt, ".html"))
  webshot(url = paste0("./html/Invaders_", dt, ".html"), file = paste0("./png/mois/Invaders_", dt, ".png"), vwidth = 1000, vheight = 1000)
  
}

## Gif

library(gifski)
png_files <- list.files("./png/mois/", pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = "./gif/animation_mois.gif", width = 1000, height = 1000, delay = 0.5)


## Par sortie ######################################################################################

t <- invader.paris %>% group_by(status, dt_flash2) %>% tally()

for (dt in unique(t$dt_flash2)[!is.na(unique(t$dt_flash2))]) {
  
  m <- invader.paris %>% 
    mutate(status.time = if_else(dt_flash2 <= dt & !is.na(dt_flash2), "Got.it", status.time)) %>%
    leaflet( width = 1000,  height = 1000) %>%
    addTiles() %>%
    setView(lat = lat.center, lng = lon.center, zoom = 13) %>%
    addMarkers( ~lon, ~lat, 
                icon = ~invader.icon.xs[status.time],
                popup = ~htmlEscape(paste(code, invaders.geo)) ) 
  
  
  htmlwidgets::saveWidget(widget = m, file = paste0("./html/Invaders_", dt, ".html"))
  webshot(url = paste0("./html/Invaders_", dt, ".html"), file = paste0("./png/sortie/Invaders_", dt, ".png"), vwidth = 1000, vheight = 1000)
  
}

## Gif

library(gifski)
png_files <- list.files("./png/sortie/", pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = "./gif/animation_sorties_0.3.gif", width = 1000, height = 1000, delay = 0.3)





