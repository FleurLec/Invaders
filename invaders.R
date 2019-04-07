
library(leaflet)
library(htmltools)
library(mapview)
library(dplyr)
library(xml2)
library(rvest)
library(knitr)
library(rmarkdown)


options(stringsAsFactors=FALSE)

##### BUILD INVADERS LOGO ###########################################################

invader.icon <- iconList(
  Got.it = makeIcon(iconUrl = "./img/invaders-green-logo.png",   iconWidth = 20, iconHeight = 20),
  Not.yet = makeIcon(iconUrl = "./img/invaders-pink-logo.png",   iconWidth = 20, iconHeight = 20),
  Not.invader = makeIcon(iconUrl = "./img/not-invader.png",   iconWidth = 20, iconHeight = 20),
  Desactivated = makeIcon(iconUrl = "./img/desactivated.png",   iconWidth = 20, iconHeight = 20)
)



## DRAW ON A MAP #####################################################################

## note that some adjustments have been made directly in the csv files (cleaning some
## adresses and duplicating line to get 1 line per invader.
## should be made directly in R using regex. to be finetuned....

# read cleaned file
invader.all <- read.csv2( file = "./data/invaders.geo.clean.csv", stringsAsFactors = F)

table(invader.all$artist)

# tag status (not perfect, but as invaders are from many city PA, NY, LY..., easier that way)
invader.all <- invader.all %>% 
  mutate( status = ifelse(artist == "XX", "Not.invader", "Not.yet")) 


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

table(invader.final$status)

# flashed but not on the initial list
#invader.add <-  anti_join(invader.flash, invader.all, by="code") %>% 
#  mutate(status = status2) %>%
#  select(-status2)

#write.csv2(invader.add, file = "./data/invader.add.csv")
# find address back

invader.add <- read.csv2("./data/invader.add.clean.csv")
invader.final <- invader.final %>% select(c(code, status, address, lat, lon)) %>% rbind(invader.add)

# which(is.na(invader.final$lat))
# invader.final[20,];


# clean format for lat/long
options(digits=9)
invader.final$lon <- as.numeric(invader.final$lon)
invader.final$lat <- as.numeric(invader.final$lat)

lon.med <- as.numeric(quantile(invader.final$lon, .5))
lat.med <- as.numeric(quantile(invader.final$lat, .5))

## Add pic

invader.final %>% mutate(pic = paste0("./pic/", stringr::str_trim(code), ".png"))  -> invader.final
invader.final %>% mutate_if(is.factor, as.character) -> invader.final

save(invader.final, file="data/invader_final.Rdata")

head(invader.final$pic)

## DRAW LOCATION ON A MAP

test <- invader.final[(1:100),]

#~htmlEscape(paste(code, address)) 
#popup = ~popupImage(pic, width=100, height=100),


#a = paste(sep = "<br/>",
#      "<img src='https://www.dropbox.com/home/invaders/PA_0001.png'>",
#      "<b>PA_0040</b>",
#      "606 5th Ave. S",
#      "Seattle, WA 98138")

leaflet(data = invader.final, width = "100%") %>%
  addTiles() %>%
  setView(lat = lat.med, lng = lon.med, zoom = 12) %>%
  addMarkers( ~lon, ~lat, 
              icon = ~invader.icon[status],
              popup = ~htmlEscape(paste(code, address)) ) 

#leaflet(data = df40, width = "100%") %>%
#  addTiles() %>%
# setView(lat = 48.85, lng = 2.34, zoom = 12) %>%
# addMarkers( ~lon, ~lat, 
#             popup = ~popupImage(pic, width=70, height=70) )



## RENDER MARKDOWN #####################################################################

output_dir <- "./output"
render("./code/invaders.Rmd", output_dir = output_dir, params = list(output_dir = output_dir))



## Table to count

count.invaders <- invader.final %>% group_by(status) %>% summarize(n = n())
kable(count.invaders)


## check which invaders are missing from geoclean
head(invader.final)

all <- data.frame(code = (1:1388)) 
all$code <- paste0("PA_", sprintf("%04d",all$code))
missing <- all %>% anti_join(invader.final, by = "code")
missing$code <- stringr::str_trim(missing$code)
write.csv(missing, file = "data/invader.to.add.csv", row.names=F, quote = F)

nrow(missing)

save(missing, file = "data/missing.Rdata")
