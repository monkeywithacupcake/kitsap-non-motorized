#
#
# read in shapefiles
#
source("secret.R")
#
# prep
library(tidyverse)
library(sf)
library(mapboxapi)
mapboxapi::mb_access_token(my_mapbox, #this will not work for you - need to use a mapbox access token
                           install = TRUE)

library(leaflet)

# read in data
psrc_bikeped <- sf::read_sf("./data/ElmerGeo_BikePed_042021.gdb")
schools <- sf::read_sf("./data/schools")
cities <- sf::read_sf("./data/cities")
#roadcl <- sf::read_sf("./data/roadcl")
#shoulders <- sf::read_sf("./data/KitsapShapefiles03_04_2022/SHOULDERS")
#sidewalks <- sf::read_sf("./data/KitsapShapefiles03_04_2022/SIDEWALKS")

outline <- sf::read_sf("./data/outline")
uga <- sf::read_sf("./data/uga")
uga$isuga = 1
outline$plain = 1
plot(outline["plain"]) + plot(c_outline["NAME"])
c_outline <- outline %>%
  st_intersection(uga) %>%
  st_intersection(cities)
plot(c_outline["NAME"])
ggplot() + 
  geom_sf(data = outline["plain"]) + 
  geom_sf(data = uga["isuga"], 
          aes(fill="isuga"),
          fill="#ccf2fe",
          show.legend = FALSE) +
  geom_sf(data = cities["NAME"], 
          aes(fill="NAME"),
          fill="#58D6FE",
          show.legend = FALSE) +
  geom_sf(data = schools, 
          aes(fill="NAME"),
          fill="#0000d8",
          show.legend = FALSE) +
  theme_void() 
ggsave(filename = "cty.png",  bg = "transparent")

mapbox_map <- leaflet() %>%
  addMapboxTiles(style_id = "light-v9",
                 username = "mapbox") %>%
  setView(lng = -122.6413,
          lat = 47.6477,
          zoom = 11)


#road_lon_lat <- st_transform(psrc_bikeped$Shape, 4326) 
#school_lon_lat <- st_transform(schools, 4326)
#ped_complete <- psrc_bikeped$ped_complete


# pick a school andn look around 
clear_creek_elem <- mb_isochrone("3999 NW Sunde Rd Silverdale, WA 98383 ",
                           time = 1:30,
                           profile = "walking")

isos_proj <- st_transform(clear_creek_elem, crs = crs) #crs = 32611)
template <- raster(isos_proj, resolution = 10)
iso_surface <- fasterize::fasterize(isos_proj, template, field = "time", fun = "min")
mapbox_map %>%
  addRasterImage(iso_surface, colors = pal, opacity = 0.5) %>%
  addLegend(values = clear_creek_elem$time, pal = pal,
            title = "Walk Time to School (min)")
pal <- leaflet::colorNumeric("viridis",  clear_creek_elem$time, na.color = "transparent")


pub_elem_schools <- schools %>%
  filter(TYPE == "PUBLIC" & SCHL_TYPE == "ELEMENTARY") 
# walking to elementary school
walking_isos <- mb_isochrone(
  pub_elem_schools,
  profile = "walking",
  time = 15,
  id = "NAME" #this makes the value of id the name
)
st_crs(walking_isos)$epsg #4326
mapbox_map %>%
  addPolygons(data = walking_isos,
              stroke = FALSE,
              popup = ~id)

# maybe here we could identify the roads within these 30 minute walk areas
# that are not complete pedestrian facilities. 
#walking_isos <- walking_isos %>% st_transform(crs = st_crs(psrc_bikeped)$epsg)
some_roads <- psrc_bikeped %>%
  st_transform(crs = 4326) %>%
  st_intersection(walking_isos)
my_pal <- 
  leaflet::colorFactor(palette = c("red", "yellow", "green"), 
              levels = c("No Facilities", "Partial Facilities", "Complete Facilities"))

# this map is the one that has all of the elmeentary schols and a 30 minute
# walk boundary and all psrc arterials
mapbox_map %>%
  addPolygons(data = walking_isos,
              stroke = FALSE,
              popup = ~id) %>%
  addPolygons(data = some_roads,weight = 1,
              smoothFactor = .3, fillOpacity = 1,
              color = ~my_pal(some_roads$ped_complete)) %>%
  addLegend(values = some_roads$ped_complete,
                     pal = my_pal,
                     title = "Pedestrian Facilities")

elem_school_facilities_arterials <- some_roads %>% 
  group_by(id, ped_complete) %>% summarise(FEET = sum(length_ft)) %>% 
  st_set_geometry(., NULL)

# that is all of the roads that are within a 30 minute walk
# of a public elementary 

# now try adding the other roads
# this was before clean up of raods DO NOT USE
kitsap_roads <- clean_roadcl_w_shoulders %>% #roadcl %>%
  st_transform(crs = 4326) %>%
  st_intersection(walking_isos) %>%
  mutate(road_type = recode(HIERARCHY,
                            `1` = "State HWY",
                            `2` = "State HWY",
                            `3` = "Collector/Arterial",
                            `4` = "Local",
                            `5` = "Easement",
                            .default = "Other"))
road_type_pal <- 
  leaflet::colorFactor(palette = "Accent", 
                       levels = c("State HWY", "Collector/Arterial", "Local", "Easement", "Other"))

yes_no_pal <- 
  leaflet::colorFactor(palette = c("blue", "orange"), 
                       levels = c("Yes", "No"))
mapbox_map %>%
  addPolygons(data = walking_isos,
              stroke = FALSE,
              popup = ~id) %>%
  addPolygons(data = kitsap_roads, weight = 1, 
              fillOpacity = 0,
              color = ~road_type_pal(kitsap_roads$road_type))  %>%
  addPolygons(data = some_roads, weight = 1,
              smoothFactor = .3, fillOpacity = 0,
              color = ~my_pal(some_roads$ped_complete)) %>%
  addLegend(values = some_roads$ped_complete,
            pal = my_pal,
            title = "Pedestrian Facilities") %>%
  addLegend(values = kitsap_roads$road_type,
          pal = road_type_pal,
          title = "Road Types")

# test doing it by school

getMapForSchool <- function(i) {
  school_iso <- mb_isochrone(
    pub_elem_schools[32,],
    profile = "walking",
    time = 15,
    id = "NAME" #this makes the value of id the name
  )
  school_psrc <- psrc_bikeped %>%
    st_transform(crs = 4326) %>%
    st_intersection(school_iso)
  school_roads <- roadcl %>%
    st_transform(crs = 4326) %>%
    st_intersection(school_iso) %>%
    mutate(road_type = recode(HIERARCHY,
                              `1` = "State HWY",
                              `2` = "State HWY",
                              `3` = "Collector/Arterial",
                              `4` = "Local",
                              `5` = "Easement",
                              .default = "Other"))
  school_sidewalks <- sidewalks  %>%
    st_transform(crs = 4326) %>%
    st_intersection(school_iso) %>%
    mutate(exist = ifelse(WIDTH > 0, "Yes", "No"))
  school_shoulders <- shoulders  %>%
    st_transform(crs = 4326) %>%
    st_intersection(school_iso) %>%
    mutate(exist = ifelse(SHLDR_WIDT > 4, "Yes", "No"))
  mapbox_map %>%
    addPolygons(data = school_iso,
                stroke = TRUE,
                popup = ~id, fillOpacity = 0) %>%
    #addPolygons(data = school_roads, weight = 1, 
     #           fillOpacity = 0,color = "black" ) %>%
                #color = ~road_type_pal(school_roads$road_type))  %>%
    addPolygons(data = school_psrc, weight = 1,
                smoothFactor = .3, fillOpacity = 0,
                color = ~my_pal(school_psrc$ped_complete)) %>%
    addPolygons(data = school_shoulders, weight = 1, 
                fillOpacity = 0,
                color = ~palNum(school_shoulders$SHLDR_WIDT)) %>%
    addPolygons(data = school_sidewalks, weight = 3, 
                fillOpacity = 0,
                color = ~yes_no_pal(school_sidewalks$exist))  %>%
   # addMarkers(~Long, ~Lat, label = ~htmlEscape(Name)) %>%
    addLegend(values = school_psrc$ped_complete,
              pal = my_pal,
              title = "Pedestrian Facilities") %>%
    addLegend(values = school_roads$road_type,
              pal = road_type_pal,
              title = "Road Types")
}

getMapForSchool(55)



##### other stuff


mapbox_map <- leaflet::leaflet() %>%
  addMapboxTiles(style_id = "light-v9", #"streets-v11",
                 username = "mapbox") 
mapbox_map %>%
  addPolygons(data = clear_creek_elem,
              fillColor = ~pal(time),
              stroke = FALSE,
              fillOpacity = 0.3) %>%
  leaflet::addLegend(values =  clear_creek_elem$time,
                     pal = pal,
                     title = "Walk Time to School (min)")

my_pal <- leaflet::colorFactor("viridis", ped_complete)
mapbox_map %>%
  leaflet::addPolylines(data = road_lon_lat, #psrc_bikeped$Shape,
              fillColor = ~my_pal(ped_complete),
              fillOpacity = 0.5,
              stroke = FALSE,
              smoothFactor = 0.1) %>%
  leaflet::addLegend(values = ped_complete,
            pal = my_pal,
            title = "Pedestrian Facilities")

leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addPolylines(data = road_lon_lat,
                        #fillColor = ~my_pal(ped_complete),
                        fillOpacity = 0.5,
                        #stroke = FALSE,
                        smoothFactor = 0.1) %>%
  leaflet::addMarkers(data = schools$geometry)
 
plot(psrc_bikeped["ped_complete"], key.pos = 1)
library(tmap)
qtm(psrc_bikeped["ped_complete"])

ggplot() + 
  geom_sf(data = psrc_bikeped, aes(color = ped_complete)) 





## let us look only at county schools
cities_schools = lengths(st_intersects(schools, cities)) > 0
schools <- schools %>% 
  bind_cols(city = cities_schools)
county_schools <- schools %>%
  filter(!city) %>%
  select(-city)
county_pub_elem_schools <- county_schools %>%
  filter(TYPE == "PUBLIC" & SCHL_TYPE == "ELEMENTARY") 
# walking to elementary school
walking_isos <- mb_isochrone(
  county_pub_elem_schools,
  profile = "walking",
  time = 15,
  id = "NAME" #this makes the value of id the name
)
st_crs(walking_isos)$epsg #4326
some_roads <- psrc_bikeped %>%
  st_transform(crs = 4326) %>%
  st_intersection(walking_isos)
my_pal <- 
  leaflet::colorFactor(palette = c("red", "yellow", "green"), 
                       levels = c("No Facilities", "Partial Facilities", "Complete Facilities"))


leaflet::leaflet() %>%
  addMapboxTiles(style_id = "light-v9", #"streets-v11",
                 username = "mapbox")  %>%
  addPolygons(data = walking_isos,
              stroke = FALSE,
              popup = ~id) %>%
  addPolygons(data = some_roads,weight = 1,
              smoothFactor = .3, fillOpacity = 1,
              color = ~my_pal(some_roads$ped_complete)) %>%
  addLegend(values = some_roads$ped_complete,
            pal = my_pal,
            title = "Pedestrian Facilities")

base_map <- leaflet::leaflet() %>%
  addMapboxTiles(style_id = "light-v9", #"streets-v11",
                           username = "mapbox") 
school_list <- county_pub_elem_schools %>% 
  st_set_geometry(., NULL) %>% 
  select(NAME)
lapply(county_pub_elem_schools, function(school){
  print(school$NAME)
  print("blah blah blah")
  })
lapply(walking_isos, function(school_iso){
  print(school_iso$id)
  this_school <- filter(schools, NAME == school_iso$id)
  school_roads <- some_roads %>%
    st_intersection(school_iso)
  base_map %>%
    addPolygons(data = school_iso,
                stroke = FALSE,
                label = ~id) %>%
    addPolygons(data = school_roads,weight = 1,
                smoothFactor = .3, fillOpacity = 1,
                color = ~my_pal(some_roads$ped_complete)) %>%
    addMarkers(data = this_school, label = NAME) %>%
    addLegend(values = school_roads$ped_complete,
              pal = my_pal,
              title = "Pedestrian Facilities")
})
yes_no_pal <- 
  leaflet::colorFactor(palette = c("blue", "orange"), 
                       levels = c("Yes", "No"))

some_shoulders <- clean_roadcl_w_shoulders %>%
  st_transform(crs = 4326) %>%
  st_intersection(walking_isos)%>%
  mutate(exist = ifelse(LEFT > 4 | RIGHT > 4, "Yes", "No")) 
some_shoulders <- some_shoulders[some_shoulders$exist == "Yes",]
library(htmltools)
maps <- lapply(walking_isos$id,function(x){
  school_iso <- walking_isos[walking_isos$id == x,]
  school_roads <- some_roads %>%
    st_intersection(school_iso)
  school_shoulders <- some_shoulders %>%
    st_intersection(school_iso) 
  base_map %>%
    addPolygons(data = school_iso,
                stroke = FALSE,
                label = ~id) %>%
    addPolygons(data = school_shoulders,weight = 1,
                smoothFactor = .3, fillOpacity = 1,
                color = ~yes_no_pal(school_shoulders$exist)) %>%
    addPolygons(data = school_roads,weight = 1,
                smoothFactor = .3, fillOpacity = 1,
                color = ~my_pal(school_roads$ped_complete)) %>%
    addControl(html = paste0("<b>",x,"</b>"),position = c("topright")) %>%
    addControl(html = paste0("<b>","15 minute walk","</b>"),position = c("topright")) %>%
    addLegend(values = school_roads$ped_complete,
              pal = my_pal,
              position = c("bottomright"),
              title = "Pedestrian Facilities\non Arterials") %>%
    addLegend(values = school_shoulders$exist,
            pal = yes_no_pal,
            position = c("bottomright"),
            title = "Shoulders > 4 feet")
   
})
maps

print(tagList(maps))

elem_school_facilities_arterials <- some_roads %>% 
  group_by(id, ped_complete) %>% summarise(FEET = sum(length_ft)) %>% 
  st_set_geometry(., NULL)

mapbox_map %>%
  addPolygons(data = walking_isos,
              stroke = FALSE,
              popup = ~id)




