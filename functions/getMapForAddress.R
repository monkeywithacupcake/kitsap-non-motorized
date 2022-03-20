# function to make an iso15 walking map and iso15 biking map for any location

# load mapping and libraries

library(tidyverse)
library(sf)
library(leaflet)
  
library(mapboxapi)
  
  #source("secret.R") #has my_mapbox token
  #mapboxapi::mb_access_token(my_mapbox, #this will not work for you - need to use a mapbox access token
  #                           install = TRUE)

# load data
psrc_bikeped <- sf::read_sf("./data/ElmerGeo_BikePed_042021.gdb")

transf_psrc_bikeped <- psrc_bikeped %>%
  st_transform(crs = 4326) 


#getMapByType(this_address = "10 Washington Ave, Bremerton, WA 98337", 
#             walkorbike = "bike")
getMapByType <- function(this_address, walkorbike = "walk") {
  # this address must be valid, or we will get an error
  profile <- ifelse(walkorbike == "bike", "cycling", "walking")
  word <- paste0(toupper(substr(profile, 1, 1)), substr(profile, 2, nchar(profile)))
  
  this_title <- paste("15 Minute",word, "Range<br/>",this_address)
  this_15 <- mb_isochrone(this_address,
               time = 15,
               profile = profile)
  some_roads <- transf_psrc_bikeped %>%
    st_intersection(this_15)
  
  # set up legend
  fac_levels = c("No Facilities", "Partial Facilities", "Complete Facilities")
  fac_pal <- 
    leaflet::colorFactor(palette = c("red", "yellow", "green"), 
                         levels = fac_levels)
  fac_legend_title <- paste(word,"Pedestrian Facilities on Arterials <br/> data from PSRC")
  
  leaflet::leaflet() %>%
    addMapboxTiles(style_id = "light-v9", #"streets-v11",
                   username = "mapbox")   %>%
    addPolygons(data = this_15,
                stroke = FALSE, 
                fillColor = "#5D3FD3",
                popup = "15 Minute Range")%>%
    addControl(html = paste0("<b>",this_title,"</b>"),position = c("topright")) %>%
    addLegend(values = fac_levels,
              pal = fac_pal,
              position = c("bottomright"),
              title = fac_legend_title)
}

getMapWalkingCycling <- function(this_address) {
  # this address must be valid, or we will get an error
  this_title <- paste("15 Minute Cycling & Walking Range<br/>",this_address)
  this_15_w <- mb_isochrone(this_address,
                          time = 15,
                          profile = "walking")
  this_15_b <- mb_isochrone(this_address,
                            time = 15,
                            profile = "cycling")
  this_roads <- transf_psrc_bikeped %>%
    st_intersection(this_15_b) #biking always bigger than walking
  
  # set up legend
  fac_levels = c("No Facilities", "Partial Facilities", "Complete Facilities")
  fac_pal <- 
    leaflet::colorFactor(palette = c("red", "yellow", "green"), 
                         levels = fac_levels)
  fac_legend_title <- "Facilities on Arterials <br/> data from PSRC"
  
  leaflet::leaflet() %>%
    addMapboxTiles(style_id = "light-v9", #"streets-v11",
                   username = "mapbox")   %>%
    addPolygons(data = this_15_b,
                stroke = FALSE, 
                fillColor = "#5D3FD3",
                popup = "15 Minute <br/>Cycling Range") %>%
    addPolygons(data = this_15_w,
                stroke = FALSE, 
                fillColor = "#0db5ba",
                popup = "15 Minute <br/>Walking Range") %>%
   addPolygons(data = this_roads,weight = 1,
              smoothFactor = .3, fillOpacity = 1,
              color = ~fac_pal(this_roads$ped_complete)) %>%
    addControl(html = paste0("<b>",this_title,"</b>"),position = c("topright")) %>%
    addLegend(values = fac_levels,
              pal = fac_pal,
              position = c("bottomright"),
              title = fac_legend_title)
}

#getMapWalkingCycling(this_address = "10 Washington Ave, Bremerton, WA 98337")


