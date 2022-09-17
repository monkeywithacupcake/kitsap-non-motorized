
source('~/Documents/non-motorized/functions/getCleanRoadCenterLine.R')
library(tidyverse)
library(sf)

nice_comma <- function(x){
  format(round(as.numeric(x), 0), digits = 0, nsmall=0, big.mark=",") 
}
'%ni%' <- function(x,y)!('%in%'(x,y))

groupcl <- clean_roadcl %>%  
  st_set_geometry(., NULL) %>% 
  group_by(RUCODE, road_class) %>% 
  summarise(roads = n_distinct(FULL_NAME), 
            segments = n(), 
            length = sum(LENGTH, na.rm=TRUE))

outline <- sf::read_sf("./data/outline")
outline$plain = 1
kitsap <- ggplot() + 
  geom_sf(data = outline["plain"]) + 
  theme_void()

cities <- sf::read_sf("./data/cities")
uga <- sf::read_sf("./data/uga")
uga$isuga = 1


# start plotting roads
# want always to see same color for same type of road, so
#['#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf']
# NOTE: Also make this a factor so that it is sorted properly!!
road_col <- scale_color_manual(values = c("Freeway / Expressway" = "#e41a1c",
                                          "Principal Arterial" = "#377eb8",
                                          "Minor Arterial" = '#4daf4a',
                                          "Major Collector" = "#984ea3",
                                          "Minor Collector" ='#ff7f00',
                                          "Local Access" = '#a65628',
                                          "Rural Local Access (e.g. easement or other type)" = '#f781bf',
                                          "Urban Local Access (e.g. easement or other type)" = '#ffff33'
                                          ))
clean_roadcl$road_class <- factor(clean_roadcl$road_class, levels = c("Freeway / Expressway",
                                                                      "Principal Arterial",
                                                                      "Minor Arterial",
                                                                      "Major Collector",
                                                                      "Minor Collector",
                                                                      "Local Access",
                                                                      "Rural Local Access (e.g. easement or other type)",
                                                                      "Urban Local Access (e.g. easement or other type)"))
kitsap_all_roads <- kitsap +
  geom_sf(data = clean_roadcl, aes(color = road_class)) + 
  road_col +
  theme_void() +
  labs(title = "Roads in Kitsap County",
       subtitle = paste0(nice_comma(sum(clean_roadcl$LENGTH)/5280),
                         " centerline miles"),
       caption = "Data source: Kitsap GIS",
       color = "Road Classification")

# exclude Local Access EASEMENTS & Highway/Freeway
road_df <-st_as_sf(clean_roadcl) %>%
  filter(FCLASS %ni% c(100,110,1,2))

kitsap_roads <- kitsap +
  geom_sf(data = road_df, aes(color = road_class)) + 
  road_col +
  theme_void() +
  labs(title = "Roads (exc. Highways & Easements) Kitsap County",
       subtitle = paste0(nice_comma(sum(road_df$LENGTH)/5280),
                         " centerline miles"),
       caption = "Data source: Kitsap GIS",
       color = "Road Classification")
ggsave(plot = kitsap_roads, filename = "Kitsap Roads.png", device = png)
# the cities are responsible for their roads
kitsap_roads_cities <- kitsap_roads +   
  geom_sf(data = cities["NAME"], 
          aes(fill="NAME"),
          fill="#808080",
          alpha = 0.5,
          show.legend = FALSE)

#citydf <- st_filter(clean_roadcl, cities, .predicate = st_covered_by)

ncdf <- road_df[!lengths(st_intersects(road_df, cities)), ]

kitsap_roads_no_cities <- kitsap + 
  geom_sf(data = cities["NAME"], 
          aes(fill="NAME"),
          fill="#808080",
          alpha = 0.5,
          show.legend = FALSE) +
  geom_sf(data = ncdf, aes(color = road_class)) + 
  road_col +
  theme_void() +
  labs(title = "Roads (exc.Highways & Easements & Cities) Kitsap County",
       subtitle = paste0(nice_comma(sum(ncdf$LENGTH)/5280),
                         " centerline miles"),
       caption = "Data source: Kitsap GIS",
       color = "Road Classification")
ggsave(plot = kitsap_roads_no_cities, filename = "Kitsap Roads No Cities.png", device = png)


# now we want to see where is the UGA, in UGA -> sidewalks, outside -> shoulders
# these roads SHOULD have sidewalks
uga_ncdf <- st_filter(ncdf, uga, .predicate = st_covered_by)
not_uga_ncdf <- ncdf[!lengths(st_intersects(ncdf, uga)), ]
kitsap_roads_no_cities_uga <- kitsap + 
  geom_sf(data = uga["isuga"], 
          aes(fill="isuga"),
          fill="#ccf2fe",
          alpha = 0.9,
          show.legend = FALSE) +
  geom_sf(data = cities["NAME"], 
          aes(fill="NAME"),
          fill="#808080",
          alpha = 1,
          show.legend = FALSE) +
  geom_sf(data = ncdf, aes(color = road_class)) + 
  road_col +
  theme_void() +
  labs(title = expression(atop("Roads (exc. Highways & Easements & Cities) Kitsap County", 
                               atop(italic("UGAs highlighted: Sidewalks/Curbs suitable in urban environments")))),
       subtitle = paste0(nice_comma(sum(ncdf$LENGTH)/5280),
                         " centerline miles / ",
                         nice_comma(sum(uga_ncdf$LENGTH)/5280),
                         " w/in UGA"),
       caption = "Data source: Kitsap GIS",
       color = "Road Classification")
ggsave(plot = kitsap_roads_no_cities_uga, filename = "Kitsap Roads No Cities UGA.png", device = png)



# now, identify schools. 
schools <- sf::read_sf("./data/schools")
cities_schools = lengths(st_intersects(schools, cities)) > 0
schools <- schools %>% 
  bind_cols(city = cities_schools)
county_schools <- schools %>%
  filter(!city) %>%
  select(-city)

# green mountain elementary
gme <- county_schools %>% 
  filter(NAME == "Manchester Elementary")
gme_circle <-st_buffer(st_as_sf(gme), dist = 1609)
gme_roads <- st_filter(ncdf, gme_circle, .predicate = st_intersects)
gme_circle5 <-st_buffer(st_as_sf(gme), dist = 1609*5)
gme_roads5 <- st_filter(ncdf, gme_circle5, .predicate = st_intersects)
kitsap +
  geom_sf(data = cities["NAME"], 
          aes(fill="NAME"),
          fill="#808080",
          alpha = 0.5,
          show.legend = FALSE) +
  geom_sf(data = gme_roads, aes(color = road_class)) +
  road_col +
  geom_sf(data = gme, alpha = 0.5) +
  theme_void() +
  labs(title = 'Roads w/in 1 Mile of Green Mountain Elem ',
       subtitle = paste0(nice_comma(sum(gme_roads$LENGTH)/5280),
                      " total centerline miles"),
       caption = "Data source: Kitsap GIS",
       color = "Road Classification") 

gme_roads <- st_as_sf(gme_roads) %>%
  mutate(ped_details = 
           ifelse(FULL_NAME %in% c("MADRONE AVE E","E MADRONE AVE"), "- Sidewalk One Side",
                  ifelse(FULL_NAME == "CALIFORNIA AVE E" & FR_ADDR_L < 2000 & TO_ADDR_L < 2000, "- Paved Shoulder > 5 ft One Side","")),
         new_class = paste0(road_class,ped_details))

gme_road_sum <- gme_roads %>%  
  st_set_geometry(., NULL) %>% 
  group_by(new_class) %>%
  summarise(miles = format(round(as.numeric(sum(LENGTH)/5280), 1), 
                           digits = 1, nsmall=0, big.mark=","))

ggplot() + 
  #geom_sf(data = gme_roads5, color = "#999999", alpha = 0.5) +
  geom_sf(data = gme_circle, alpha = 0.6) +
  geom_sf(data = gme_roads, aes(color = new_class), size = 1) +
  #road_col +
  geom_sf(data = gme, alpha = 0.9) +
  theme_void() +
  labs(title = 'Road Segments w/in 1 Mile of Manchester Elem ',
       subtitle = paste0(nice_comma(sum(gme_roads$LENGTH)/5280),
                         " total centerline miles"),
       caption = "Data source: Kitsap GIS",
       color = "Road Classification & Pedestrian Facilities")

gme_circle <-st_buffer(st_as_sf(gme), dist = 1609/2)
gme_roads <- st_filter(ncdf, gme_circle, .predicate = st_intersects)
gme_roadsx <- gme_roads %>%
  st_transform(crs = 4326)
sf_cent <- st_centroid(gme_roadsx)
this_pal <- colorNumeric(palette = "Dark2", domain = gme_roads$SEGMENT_ID)
leaflet() %>%
  addMapboxTiles(style_id = "light-v9",
                 username = "mapbox") %>%
  setView(lng = -122.554739,
          lat = 47.551022,
          zoom = 16) %>%
  leaflet::addPolygons(data = gme_roadsx,
              stroke = TRUE, 
              fillColor = ~this_pal(gme_roads$SEGMENT_ID),
              color = ~this_pal(gme_roads$SEGMENT_ID)) %>%
  addLabelOnlyMarkers(data = sf_cent$geometry, label = sf_cent$SEGMENT_ID,
                      labelOptions = labelOptions(noHide = TRUE, 
                                                  direction = 'top', 
                                                  textOnly = TRUE))
  




ggplot() + 
  geom_sf(data = gme_circle, alpha = 0.6) +
  geom_sf(data = gme_roads, aes(color = as.factor(paste(RD_LOG_ID, SEGMENT_ID))), size = 1) +
  geom_sf(data = gme, alpha = 0.9) +
  geom_sf_label(data = gme_roads, #filter(gme_roads, RD_LOG_ID == 44350), 
                aes(label = SEGMENT_ID, 
                    color = as.factor(paste(RD_LOG_ID, SEGMENT_ID)))) +
  theme_void() +
  labs(title = 'Road Segments w/in 1/2 Mile of Manchester Elem ',
       subtitle = "Road Segments Labeled with Segment ID",
       caption = "Data source: Kitsap GIS",
       color = "Road Log ID & Segment ID")

# + 
#   annotation_custom(tableGrob(gme_road_sum, ttheme_minimal(base_size = 10)),
#                     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)


# this makes 1609m circles around each county school (1609.34m in a mile)
school_circles <- st_buffer(st_as_sf(county_schools), dist = 1609)
# this gets the roads not in a city that are within 1 m of school
roads_1m_county_school <- st_filter(ncdf, school_circles, .predicate = st_intersects)
roads_1m_county_school_uga <- st_filter(roads_1m_county_school, 
                                        uga, .predicate = st_intersects) %>%  
  st_set_geometry(., NULL) %>%
  group_by(road_class) %>% # road_type) %>%
  summarise(miles = sum(LENGTH, na.rm=TRUE)/5280)
total_uga_miles <- nice_comma(sum(roads_1m_county_school_uga$miles))
roads_1m_county_school_nogeo <- roads_1m_county_school %>%  
   st_set_geometry(., NULL)
roads_1m_county_school_nogeo %>% 
  group_by(road_class) %>% # road_type) %>%
  summarise(miles = sum(LENGTH, na.rm=TRUE)/5280)
sum(roads_1m_county_school_nogeo$LENGTH, na.rm=TRUE)/5280 # road miles w/in 1 mi school
sum(ncdf$LENGTH, na.rm=TRUE)/5280 # total road miles outside city
# these road segments are a priority for evaluation
kitsap +
  geom_sf(data = uga["isuga"], 
          aes(fill="isuga"),
          fill="#ccf2fe",
          alpha = 0.9,
          show.legend = FALSE) +
  geom_sf(data = cities["NAME"], 
          aes(fill="NAME"),
          fill="#808080",
          alpha = 1,
          show.legend = FALSE) +
  geom_sf(data = ncdf, color = "#999999", alpha = 0.5) +
  geom_sf(data = school_circles, alpha = 0.6) +
  geom_sf(data = roads_1m_county_school, aes(color = road_class), size = 1) +
  road_col +
  geom_sf(data = county_schools, alpha = 0.5) +
  theme_void() +
  labs(title = paste0('Non-City Roads w/in 1 Mile of school in Kitsap County \n',
                      nice_comma(sum(roads_1m_county_school_nogeo$LENGTH)/5280), " of ",
                      nice_comma(sum(ncdf$LENGTH)/5280),
                      " total centerline miles", "\n",
                      total_uga_miles, " of ",
                      nice_comma(sum(roads_1m_county_school_nogeo$LENGTH)/5280),
                      " miles within UGA"),
       caption = "Data source: Kitsap GIS; Roads and Schools inside of City Boundaries excluded. Roads near schools shown",
       color = "Road Classification")


in_uga_schools <- st_filter(county_schools, uga, .predicate = st_covered_by)
kitsap + 
  geom_sf(data = int_dat2, aes(color = road_class)) +
  road_col +
  geom_sf(data = in_uga_schools, alpha = 0.6) +
  theme_void() 

kitsap_ca +
  geom_sf(data = county_schools) +
  theme_void() +
  labs(title = paste0('Non-City Arterials & Collectors in Kitsap County ',
                      nice_comma(sum(cadf$LENGTH)/5280),
                      " centerline miles"),
       caption = "Data source: Kitsap GIS",
       color = "Road Classification")

lrdf <-st_as_sf(clean_roadcl) %>%
  filter(FCLASS %in% c(7,100,110)) 
kitsap +
  geom_sf(data = lrdf, aes(color = road_class)) + 
  theme_void() +
  labs(title = 'Local Roads in Kitsap County',#{frame_time}',
       caption = "Data source: Kitsap GIS",
       color = "Road Classification")


## DATA THAT WE ALREADY HAVE

psrc_bikeped <- sf::read_sf("./data/ElmerGeo_BikePed_042021.gdb")
school_circles_test <- school_circles %>% st_transform(crs = 2285)
tmp <- st_filter(psrc_bikeped, school_circles_test, .predicate = st_intersects)
kitsap +
  geom_sf(data = tmp, aes(color = ped_complete)) + 
  theme_void() +
  labs(title = 'PSRC Data in Kitsap County',#{frame_time}',
       caption = "Data source: Kitsap GIS & PSRC",
       color = "Pedestrian Facilities")



tmp %>%  
  st_set_geometry(., NULL) %>%
  group_by(ped_complete) %>% # road_type) %>%
  summarise(miles = sum(length_ft, na.rm=TRUE)/5280)

shoulders <- sf::read_sf("./data/KitsapShapefiles03_04_2022/SHOULDERS")
sidewalks <- sf::read_sf("./data/KitsapShapefiles03_04_2022/SIDEWALKS")
tmp_sw <- st_filter(sidewalks, school_circles, .predicate = st_intersects)
tmp_sw_ncdf <- st_filter(sidewalks, ncdf, .predicate = st_intersects) %>% 
  st_as_sf(.) %>%
  filter(WIDTH > 0, LENG_FT > 0) %>%
  mutate(LENGTH = LENG_FT,
         TYPE = paste0("Sidewalk: ",WIDTH, " ft")) %>%
  select(LENGTH, TYPE)
tmp_sh_ncdf <- st_filter(shoulders, ncdf, .predicate = st_intersects) %>% 
  st_as_sf(.) %>%
  filter(SHLDR_WIDT > 0, SHLDR_SURF == "ACP", DISTANCE_F > 0) %>%
  mutate(LENGTH = DISTANCE_F,
         TYPE = paste0("Paved Shoulder: ", SHLDR_WIDT," ft")) %>%
  select(LENGTH, TYPE)
tmp_sw_sh <- bind_rows(tmp_sw_ncdf, tmp_sh_ncdf) 

tmp_sw %>%  
  st_set_geometry(., NULL) %>%
  filter(WIDTH > 0, LENG_FT > 0) %>%
  group_by(SIDE_OF_RO, WIDTH) %>% # road_type) %>%
  summarise(miles = sum(LENG_FT, na.rm=TRUE)/5280)

kitsap +
  geom_sf(data = tmp_sw_ncdf, aes(color = as.factor(WIDTH))) + 
  theme_void() +
  labs(title = 'Kitsap County Sidewalk Data',
       caption = "Data source: Kitsap GIS - data in development",
       color = "Pedestrian Facilities")
kitsap +
  geom_sf(data = uga["isuga"], 
          aes(fill="isuga"),
          fill="#ccf2fe",
          alpha = 0.9,
          show.legend = FALSE) +
  geom_sf(data = cities["NAME"], 
          aes(fill="NAME"),
          fill="#808080",
          alpha = 1,
          show.legend = FALSE) +
  geom_sf(data = ncdf, color = "#999999", alpha = 0.3) +
  geom_sf(data = tmp_sw_sh, aes(color = TYPE, fill = TYPE), width = 1) + 
  theme_void() +
  labs(title = 'Kitsap County Sidewalk & Shoulder Data',
       caption = "Data source: Kitsap GIS - data in development",
       color = "Pedestrian Facilities",
       fill = "Pedestrian Facilities")

tmp_sw_sh_sum <- tmp_sw_sh %>%  
  st_set_geometry(., NULL) %>%
  group_by(TYPE) %>% # road_type) %>%
  summarise(miles = sum(LENGTH, na.rm=TRUE)/5280)
sum(tmp_sw_sh_sum$miles)
# now, just get the road details 

#tmp_sw <- clean_roadcl %>%
#  st_set_geometry(., NULL) %>%
#  select(road_type, SEGMENT_ID:MUNI_R) %>%
#  mutate(SW_LEFT = NA, SW_RIGHT = NA)

# sidewalks_nogeo <- sidewalks %>%  
#   st_set_geometry(., NULL)
# shoulders_nogeo <- shoulders %>%  
#   st_set_geometry(., NULL)
# roadcl_nogeo <- clean_roadcl %>%  
#   st_set_geometry(., NULL)
# write_csv(sidewalks_nogeo, "~/Desktop/sidewlks_nogeo.csv")
# write_csv(shoulders_nogeo, "~/Desktop/shoulders_nogeo.csv")
# write_csv(roadcl_nogeo, "~/Desktop/roadcl_nogeo.csv")