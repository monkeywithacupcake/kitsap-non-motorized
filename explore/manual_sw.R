# manually coding sidewalks

# https://www.wsdot.wa.gov/research/reports/fullreports/806.2.pdf

source('~/Documents/non-motorized/functions/getCleanRoadCenterLine.R')
library(tidyverse)
library(sf)
library(mapboxapi)
#mapboxapi::mb_access_token(my_mapbox, #this will not work for you - need to use a mapbox access token
#                           install = TRUE)

library(leaflet)


# work in batches
curr <- tmp_sw %>%
  filter(is.na(SW_LEFT), road_type == "Collector/Arterial" ) %>% #, 
         #MUNI_L =="KI", MUNI_R == "KI") %>%
  group_by(RD_LOG_ID) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  top_n(10, desc(n))

# now, we plot it, and then fill in the tmp_sw table
my_pal <- leaflet::colorNumeric("viridis",  t_roads$SEGMENT_ID, na.color = "transparent")

#my_map <- leaflet() %>%
#  addMapboxTiles(style_id = "satellite-v9",
#                 username = "mapbox") %>%
#  setView(lng = -122.6413,
#          lat = 47.6477,
#          zoom = 11)

working_rd <- 56100 #curr$RD_LOG_ID[5]

t_roads <- clean_roadcl %>%
  filter(RD_LOG_ID %in% working_rd) %>%
  st_transform(crs = 4326)



my_map %>%
  addPolygons(data = t_roads, #weight = 1,
              label = ~paste(RD_LOG_ID, SEGMENT_ID),
              fillOpacity = .5,
              color = ~my_pal(t_roads$SEGMENT_ID),
              labelOptions = labelOptions(noHide = T))


#will code if has sidewalks "FULL", "PARTIAL", "NONE"
#working_rd <- 11300
tmp_sw$SW_LEFT[tmp_sw$RD_LOG_ID == working_rd] <- "PARTIAL"
tmp_sw$SW_RIGHT[tmp_sw$RD_LOG_ID == working_rd] <- "NONE"
tmp_sw$SW_RIGHT[tmp_sw$RD_LOG_ID == working_rd & tmp_sw$SEGMENT_ID == 10823] <- "FULL"
tmp_sw$SW_RIGHT[tmp_sw$RD_LOG_ID == working_rd & tmp_sw$SEGMENT_ID == 10671] <- "PARTIAL"
tmp_sw$SW_LEFT[tmp_sw$RD_LOG_ID == working_rd & tmp_sw$SEGMENT_ID == 18028] <- "PARTIAL"


tmp_sw$SW_DATE[tmp_sw$RD_LOG_ID == working_rd] <- "201804"
tmp_sw$SW_SOURCE[tmp_sw$RD_LOG_ID == working_rd] <- "Google Streetview"

write_csv(tmp_sw, "./data/tmp_sw.csv")

# check my progress
sw_pal <- leaflet::colorFactor(palette = c("green", "yellow", "red"), 
                               levels = c("FULL", "PARTIAL", "NONE"), na.color = "grey")

road_sw <- clean_roadcl %>%
  left_join(tmp_sw)  %>%
  st_transform(crs = 4326)

road_sw %>% st_set_geometry(., NULL) %>% 
  group_by(road_type, SW_LEFT, SW_RIGHT) %>% 
  summarise(miles_centerline = sum(LENGTH, na.rm=TRUE)/5280, 
            .groups = "drop")

# light_map <- leaflet() %>%
#   addMapboxTiles(style_id = "light-v9",
#                  username = "mapbox") %>%
#   setView(lng = -122.6413,
#           lat = 47.6477,
#           zoom = 11)

light_map %>%
  addPolygons(data = road_sw, weight = 2,
              fillOpacity = .5,
              color = ~sw_pal(road_sw$SW_RIGHT))



schools <- sf::read_sf("./data/schools")
cities <- sf::read_sf("./data/cities")
cities_schools = lengths(st_intersects(schools, cities)) > 0
schools <- schools %>% 
   bind_cols(city = cities_schools)
county_schools <- schools %>%
    filter(!city) %>%
    select(-city)

walking_isos <- mb_isochrone(
  county_schools,
  profile = "walking",
  time = 15,
  id = "NAME" #this makes the value of id the name
)

elem_sw <- road_sw %>%
  st_intersection(walking_isos)
elem_sw %>% 
  st_set_geometry(., NULL) %>% 
  mutate(SW = ifelse(is.na(SW_LEFT), NA, 
                     ifelse(SW_LEFT & SW_RIGHT == "FULL", "FULL", 
                     ifelse(SW_LEFT & SW_RIGHT == "NONE", "NONE", "PARTIAL")))) %>%
  group_by(id, road_type, SW) %>% 
  summarise(miles_centerline = sum(LENGTH, na.rm=TRUE)/5280,
            .groups = "drop")

#
