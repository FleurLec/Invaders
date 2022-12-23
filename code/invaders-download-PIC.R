## download invaders pic
library(RCurl)
library(XML)
library(dplyr)

#  ------

  url2 <- "http://invader.spotter.free.fr/images/"
  
  for (i in 1:1375 )    
  {
    download.file(url = paste0(url2, "PA_", stringr::str_pad(i, 4, pad = "0"), "-grosplan.png"), paste0("./pic/", paste0("PA_", stringr::str_pad(i, 4, pad = "0"), ".png")))
  }  

  for (i in 1376:1388 )    
  {
    download.file(url = paste0(url2, "PA_", stringr::str_pad(i, 4, pad = "0"), "-grosplan.png"), paste0("./pic/", paste0("PA_", stringr::str_pad(i, 4, pad = "0"), ".png")))
  }  
  
  for (i in 1389:1449 )    
  {
    download.file(url = paste0(url2, "PA_", stringr::str_pad(i, 4, pad = "0"), "-grosplan.png"), paste0("./pic/", paste0("PA_", stringr::str_pad(i, 4, pad = "0"), ".png")))
  }  
  for (i in 1:1467 )    
  {
    download.file(url = paste0(url2, "PA_", stringr::str_pad(i, 4, pad = "0"), "-grosplan.png"), paste0("./pic/", paste0("PA_", stringr::str_pad(i, 4, pad = "0"), ".png")))
  }   
  

  
  
  
  ## Other option
  
 # url <- "http://www.unoeilquitraine.fr/wp-content/gallery/2016-09-invader-ete/"
  
  for (p in seq(700, 1400, by = 100))
  {
  
  url <- paste0("http://www.unoeilquitraine.fr/space-invaders/wp-content/gallery/pa_", p, "/")
  
  time(10)

  ## recup du contenu de la page
  doc.html <- htmlParse(url)
  doc.links <- xpathSApply(doc.html, "//a/@href")  #<a href
  
  ## recup des noms des jpg  
  jpg.url <- as.character(doc.links[grep('jpg', doc.links)])

  ## suppression des backup
  del <- function(x){grep("_backup", x)}
  
  jpg.url.2 <- sapply(jpg.url, del)
  which(jpg.url.2==1)  
  
  jpg <- jpg.url[-as.vector(which(jpg.url.2==1) )]
  
  ## download des photos
  
  for (i in 1:length(jpg) )    
  {
    download.file(url = paste0(url, jpg[i]), paste0("../pic2/", jpg[i]))
  }
  
  }
 
         