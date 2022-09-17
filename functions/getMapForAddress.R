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

#"8196 WA-104, Kingston, WA 98346"

#getMapByType(this_address = "10 Washington Ave, Bremerton, WA 98337", 
#             walkorbike = "bike")
# Silverdale Library
getMapByType(title = "Silverdale Library", this_address = "3650 NW Anderson Hill Rd, Silverdale, WA 98383", walkorbike = "walk")
# Cougar Valley Elementary
getMapByType(title = "Cougar Valley Elementary", this_address = "13200 Olympic View Rd NW, Silverdale, WA 98383", walkorbike = "walk")
getMapByType(title = "Fieldstone Memory Care", 
             this_address = "11313 Clear Creek Rd NW, Silverdale, WA 98383", 
             walkorbike = "walk")
#others
getMapByType(this_address = "8196 WA-104, Kingston, WA 98346", walkorbike = "walk")
getMapByType(this_address="100 SW Lakeway Blvd, Port Orchard WA 98366", walkorbike = "walk")
getMapByType(this_address= "330 NE Foster Rd, Bremerton WA 98311", walkorbike = "walk")
getMapByType(this_address= "9100 Dickey Rd NW, Silverdale, WA 98383", walkorbike = "walk")
getMapByType(this_address = "3999 NW Sunde Rd, Silverdale, WA 98383", walkorbike = "walk")
getMapByType <- function(title = "", this_address, walkorbike = "walk") {
  # this address must be valid, or we will get an error
  profile <- ifelse(walkorbike == "bike", "cycling", "walking")
  word <- paste0(toupper(substr(profile, 1, 1)), substr(profile, 2, nchar(profile)))
  
  this_title <- paste(title, "<br/>15 Minute",word, "Range<br/>",this_address)
  this_15 <- mb_isochrone(this_address,
               time = 15,
               profile = profile)
  meters_in_mile <- 1609 #.34
  this_1mile <- mb_isochrone(this_address,
                             distance = meters_in_mile,
                             profile = profile)
  this_geo <- mb_geocode(this_address)
  #this_15 <- this_15 %>% st_transform(crs = 6597)
  this_roads <- transf_psrc_bikeped %>% #transf_psrc_bikeped %>%
    st_intersection(this_15)
  
  # set up legend
  fac_levels = c("No Facilities", "Shoulders - Wide", 
                 "Sidewalk One Side", "Sidewalks Both Sides")
    #"No Facilities", "Partial Facilities", "Complete Facilities")
  fac_pal <- 
    leaflet::colorFactor(palette = c("red", "yellow", "green", "blue"), 
                         levels = fac_levels)
  fac_legend_title <- paste(word,"Facilities based on observation May 2022")
                            #"Pedestrian Facilities on Arterials <br/> data from PSRC")
  
  leaflet::leaflet() %>%
    addMapboxTiles(style_id = "light-v9", #"streets-v11",
                   username = "mapbox",
                   scaling_factor = "0.5x")   %>%
    addMarkers(this_geo[1], this_geo[2]) %>%
    addPolygons(data = this_15,
                stroke = FALSE, 
                fillColor = "#5D3FD3",
                popup = "15 Minute Range") %>%
    addPolygons(data = this_1mile,
                stroke = FALSE, 
                fillColor = "#FFC0CB",
                popup = "1 Mile Range") %>%
    #addPolygons(data = this_roads,
    #            stroke = TRUE,
    #            weight = 1,
    #            smoothFactor = .3, fillOpacity = 1,
    #            color = ~fac_pal(this_roads$ped_complete)) %>%
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


