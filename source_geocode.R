## Test with Bing

BingKey <- "[BING_KEY]"
# BING_URL_HTTP = 'http://spatial.virtualearth.net/REST/v1/Dataflows'
BING_URL_HTTP = 'http://dev.virtualearth.net/REST/v1/Locations?'
# GEOCODING_ENDPOINT_JSON = '/Geocode?input=csv&output=json'


gGeoCodeBing <- function(location){
  #location = enc2utf8(location)
  location = gsub("[[:punct:]]", "", location)   # suppression de la ponctuation
  location =  urltools::url_encode(location)     # encodage du string dans un format lisible html (%20% au lieu des espaces.)
  
  encodedParams = URLencode(paste0('&q=',location,'&maxResults=1&key=',BingKey))
  
  
  # Create the complete request URL
  signedUrl = paste0(BING_URL_HTTP, encodedParams)
  
  # Call the API
  f_content = RCurl::getURL(signedUrl)
  
  x <- RJSONIO::fromJSON(f_content, simplify = FALSE)
  
  # get only lat and long
  
  ville <- x$resourceSets[[1]]$resources[[1]]$name
  latlon <- x$resourceSets[[1]]$resources[[1]]$geocodePoints[[1]]$coordinates
  
  res <- paste(ville, unlist(latlon)[1], unlist(latlon)[2], sep="#")
  
  return(res)
}

#myLoc <- "5 rue raymond losserand, paris, France"


#r <- gGeoCodeBing(myLoc)


