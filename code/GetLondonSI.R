#########################################################
# Draw a map of Invaders in London
#########################################################

library(XML)
library(tmaptools)

## Max invaders in London
LDNmax <- 150

data <- xmlParse("./data/LondonSI.kml")
xml_data <- xmlToList(data)
myl <- xml_data$Document$Folder

si.ldn <- data.frame(code = character(), lon = double(), lat = double())

for (i in (2:110)) {si.ldn[i-1,"code"] = myl[[i]]$name
                    si.ldn[i-1,"lon"] = unlist(str_split(trimws(myl[[i]]$Point$coordinates), ","))[1]
                    si.ldn[i-1,"lat"] = unlist(str_split(trimws(myl[[i]]$Point$coordinates), ","))[2]
}

# REverse Geocode
addresses <- rev_geocode_OSM(si.ldn$lon, si.ldn$lat)
address <- addresses %>% mutate(adr = paste(house_number, road, suburb, postcode)) %>%
                         select(x, y, adr)
si.ldn.geocode <- si.ldn %>% left_join(address, by=(c("lon" = "x", "lat" = "y"))) %>% 
                             mutate(code = paste0("LDN_", sprintf("%03d", as.integer(str_remove(code, "LDN_"))))) %>%
                             select(code, adr, lat, lon) 

write.csv(si.ldn.geocode, file = "./data/RAJ_London.geocode.csv", row.names=F, quote = F)

all <- data.frame(code = (1:LDNmax)) 
all$code <- paste0("LDN_", sprintf("%03d",all$code))
missing <- all %>% anti_join(si.ldn.geocode, by = "code")
missing$code <- stringr::str_trim(missing$code)
write.csv(missing , file = "./data/OUT_invader.to.add.LDN.csv", row.names=F, quote = F)



