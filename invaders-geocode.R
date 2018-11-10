## INVADER GEOCODE

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


