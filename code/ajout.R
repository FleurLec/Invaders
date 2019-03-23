load("data/missing.Rdata")


## Quelques ajouts

ajout <- read.csv2("data/ajout.csv")

head(ajout)

ajout$invader <- substr(ajout$text,1,22)
ajout$code <- substr(stringr::str_trim(ajout$text),15,21)
ajout$artist <- substr(ajout$text,15,17)
ajout$address <- substr(ajout$text,23,length(ajout$text))

# les adresses commencant par , sont cleanées, les autres topées car multi invaders
ajout$address2 <- ifelse(substr(ajout$address,1,1) == ",", 
                          substr(ajout$address,2,length(ajout$address)),
                          ajout$address)

ajout$yena2 <- ifelse(substr(stringr::str_trim(ajout$address2),1,2) == "PA", 
                       "X",
                       "")

 aj <- ajout %>% filter(yena2 =="X")
 aj$code <- substr(stringr::str_trim(aj$address2),1,7)
 aj$address <- stringr::str_trim(substr(aj$address2,11, nchar(aj$address2)))
 aj
 ajout$code <- stringr::str_trim(ajout$code)

 ajout <- rbind(ajout, aj)
 write.csv2(ajout, file="data/ajout.reformatted.csv", row.names=F, quote = F)

## Find new addresses

missing.ajout <- missing %>% 
  inner_join(ajout, by="code")

names(missing.ajout)
head(missing.ajout)

ma <- NULL
ma$invader <- missing.ajout$invader
ma$code <- missing.ajout$code
ma$artist <- missing.ajout$artist
ma$address <- missing.ajout$address2
ma$invaders.geo <- missing.ajout$address2
ma$lat <- missing.ajout$lat
ma$lon <- missing.ajout$long
ma <- as.data.frame(ma)
ma

missing.missing <- missing %>% 
  anti_join(ajout, by="code")

str(ajout)
str(missing)

write.csv2(missing.missing, file="data/missing.missing.csv", row.names=F, quote = F)
write.csv2(ma, file="data/missing.ajout.csv", row.names=F, quote = F)


