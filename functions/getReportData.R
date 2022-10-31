# report functions to return main data

library(tidyverse)
library(sf)

########
# kitsap

getDataCities <- function() {
  sf::read_sf(paste0(this_dir,"data/cities"))
}

getMapKitsap <- function() {
  outline <- sf::read_sf(paste0(this_dir,"data/outline"))
  outline$plain = 1
  ggplot() + 
    geom_sf(data = outline["plain"], color = NA, alpha = 0.6) + 
    theme_void()
}

getDataCountyNoCities <- function() {
  cities <- getDataCities()
  outline <- sf::read_sf(paste0(this_dir,"data/outline"))
  st_difference(outline, st_union(cities))
}


########
# handl road data

getDataRoad <- function() {
roadcl <- sf::read_sf("./data/roadcl")
clean_roadcl <- roadcl %>%
  filter(RD_LOG_ID != 0 & RD_LOG_ID != 200) %>% #not county roads
  mutate(road_type = recode(HIERARCHY,
                            `1` = "State HWY",
                            `2` = "State HWY",
                            `3` = "Collector/Arterial",
                            `4` = "Local",
                            `5` = "Easement",
                            `6` = "Proposed",
                            `7` = "Ramp",
                            `8` = "Walking Path", # there should be 0
                            .default = "Other"),
         road_class = recode(FCLASS,
                             `2` = "Freeway / Expressway",
                             `3` = "Principal Arterial",
                             `4` = "Minor Arterial",
                             `5` = "Major Collector",
                             `6` = "Minor Collector",
                             `7` = "Local Access",
                             `100` = "Rural Local Access (e.g. easement or other type)",
                             `110` = "Urban Local Access (e.g. easement or other type)",
                             .default = "Other")
  )
# FCLASS - Federal Functional Classifications as referenced in WSDOT document 
# "Guidelines For Amending Functional Classification in Washington State" from October 2013
# (available at https://wsdot.wa.gov/mapsdata/travel/hpms/pdf/GuidelinesForAmendingFunctionalClassification_WSDOT.pdf)
# get them in order
clean_roadcl$road_class <- factor(clean_roadcl$road_class, levels = c("Freeway / Expressway",
                                                                      "Principal Arterial",
                                                                      "Minor Arterial",
                                                                      "Major Collector",
                                                                      "Minor Collector",
                                                                      "Local Access",
                                                                      "Rural Local Access (e.g. easement or other type)",
                                                                      "Urban Local Access (e.g. easement or other type)"))
clean_roadcl
}
getDataRoadNoCities <- function(clean_roadcl, county_no_cities) { #ncdf
# exclude Local Access EASEMENTS & Highway/Freeway
road_df <-st_as_sf(clean_roadcl) %>%
  filter(FCLASS %ni% c(100,110,1,2), RD_LOG_ID > 0) %>%
  mutate(LENGTH = as.integer(st_length(.)))

# need to make sure that this is right

 st_intersection(road_df, st_buffer(county_no_cities,0)) %>%
  mutate(LENGTH = as.integer(st_length(.)))
}

getDataSidewalks <- function(county_no_cities){
  sidewalks <- sf::read_sf("./data/KitsapShapefiles03_04_2022/SIDEWALKS")
  #sidewalks_nc <-
    st_intersection(sidewalks, st_buffer(county_no_cities,0)) %>%
    st_as_sf(.) %>%
    filter(WIDTH > 0, LENG_FT > 0) %>%
    mutate(LENGTH = as.integer(st_length(.)),
           TYPE = paste0("Sidewalk: ",WIDTH, " ft"),
           RD_LOG_ID = as.integer(ROADLOGID),
           SEGMENT_ID = as.integer(SEG_ID)) %>%
    select(RD_LOG_ID,SEGMENT_ID,BMP,EMP,SIDE_OF_RO, WIDTH, LENGTH, TYPE)
    
}

getDataShoulders <- function(county_no_cities){
  shoulders <- sf::read_sf("./data/KitsapShapefiles03_04_2022/SHOULDERS")
  
  
  # SHLDR_SURF 
  # ACP - paved
  # BST - bituminous surface (chip seal), like paved
  # GRV - gravel
  # PAP - porous asphalt
  #shoulders_nc <- 
    st_intersection(shoulders, st_buffer(county_no_cities,0)) %>%
    st_as_sf(.) %>%
    filter(SHLDR_WIDT > 0, 
           SHLDR_SURF %in% c("ACP", "BST", "PAP", "GRV"), # these are the "paved" ones
           DISTANCE_F > 0) %>%
    mutate(LENGTH = as.integer(st_length(.)),
           TYPE = paste0("Shoulder: ", SHLDR_WIDT," ft"),
           RD_LOG_ID = as.integer(ROAD_LOG_I),
           SEGMENT_ID = as.integer(SEG_ID)) %>%
    select(RD_LOG_ID,SEGMENT_ID,BMP,EMP,SIDE_OF_RO, WIDTH = SHLDR_WIDT, LENGTH, TYPE)
}


getDataFacilities <- function(sw,sh) {
  df <- bind_rows(sw, sh) 
  
  # NOTE: Also make this a factor so that it is sorted properly!!
  sw_sh_col <- scale_color_manual(values = c("Sidewalk: 3 ft"="#E41A1C",
                                             "Sidewalk: 4 ft"="#874F6F",
                                             "Sidewalk: 5 ft"="#3881B0",
                                             "Sidewalk: 8 ft"="#449B75",
                                             "Shoulder: 1 ft"="#56A255",
                                             "Shoulder: 2 ft"="#7E6E85",
                                             "Shoulder: 3 ft"="#AC5782",
                                             "Shoulder: 4 ft"="#E3712B", 
                                             "Shoulder: 5 ft"="#FFA10D",
                                             "Shoulder: 6 ft"="#FFE528",
                                             "Shoulder: 7 ft"="#E1C62F",
                                             "Shoulder: 8 ft"="#B16C29",
                                             "Shoulder: 9 ft"="#A16C25",
                                             "Shoulder: 10 ft"="#C66764",
                                             "Shoulder: 12 ft"="#F17EB4",
                                             "Shoulder: 15 ft"="#CB8CAD",
                                             "Shoulder: 17 ft"="#999999"))
  df$TYPE <- factor(df$TYPE, levels = c("Sidewalk: 3 ft",
                                        "Sidewalk: 4 ft",
                                        "Sidewalk: 5 ft",
                                        "Sidewalk: 8 ft" ,
                                        "Shoulder: 1 ft",
                                        "Shoulder: 2 ft",
                                        "Shoulder: 3 ft",
                                        "Shoulder: 4 ft", 
                                        "Shoulder: 5 ft",
                                        "Shoulder: 6 ft",
                                        "Shoulder: 7 ft",
                                        "Shoulder: 8 ft",
                                        "Shoulder: 9 ft",
                                        "Shoulder: 10 ft",
                                        "Shoulder: 12 ft",
                                        "Shoulder: 15 ft",
                                        "Shoulder: 17 ft"))
  df
  
}


# smaller units
county_no_cities <- getDataCountyNoCities()
commdists <- sf::read_sf(paste0(this_dir,"data/commdist"))
commdists <- st_intersection(commdists, st_buffer(county_no_cities,0)) 
commdists <- st_as_sf(commdists) %>% 
  mutate(NAME = ifelse(DISTRICT == "1", "1 North",
                       ifelse(DISTRICT == "2", "2 South", "3 Central"))) %>%
  group_by(NAME) %>%  # ".x" is refers to the current group:
  group_modify(~ st_union(.x) %>% as_tibble()) %>%
  ungroup() %>%
  st_as_sf() %>%
  arrange(NAME) 

the_commdists <- unique(st_set_geometry(commdists, NULL)$NAME)
loop_commdists <- st_as_sf(commdists) %>% mutate(tomatch = NAME)



uga <- sf::read_sf(paste0(this_dir,"data/uga"))
uga$isuga = 1
uga_no_city <- st_as_sf(uga) %>%
  filter(substring(GMA_JURISD, 1, 4) != "City") %>%
  group_by(GMA_JURISD) %>%  # ".x" is refers to the current group:
  group_modify(~ st_union(.x) %>% as_tibble()) %>%
  ungroup() %>%
  st_as_sf() %>%
  arrange(GMA_JURISD)
the_ugas <- unique(st_set_geometry(uga_no_city, NULL)$GMA_JURISD)
loop_ugas <- st_as_sf(uga_no_city) %>% mutate(tomatch = GMA_JURISD)



lamird <- sf::read_sf(paste0(this_dir,"data/lamird"))
lamird$islamird = 1
the_lamirds <- unique(st_set_geometry(lamird, NULL)$PLAN_AREA)
loop_lamirds <- st_as_sf(lamird) %>% mutate(tomatch = PLAN_AREA)



schools <- sf::read_sf(paste0(this_dir,"data/schools"))
pub_schools <- schools %>%
  filter(TYPE == "PUBLIC") %>%
  arrange(NAME)
pub_schools2 <- getWalkingPolygonInCounty(pub_schools, "NAME")
the_schools <- unique(st_set_geometry(pub_schools2, NULL)$id)
loop_schools <- st_as_sf(pub_schools2) %>% mutate(tomatch = id)
rm(schools)

community <- sf::read_sf(paste0(this_dir,"data/community"))
libraries <- community %>%
  filter(TYPE == "PUBLIC LIBRARY") %>%
  arrange(NAME)
libraries2 <- getWalkingPolygonInCounty(libraries, "NAME")
the_libraries <- unique(st_set_geometry(libraries2, NULL)$id)
loop_libraries <- st_as_sf(libraries2) %>% mutate(tomatch = id)
rm(community)

transit_cp <- tribble(
  ~NAME, ~ADDRESS,
  "Bremerton Transportation Center (Ferry Terminal)","10 Washington Ave, Bremerton 98337",
  "Bainbridge Island Ferry Terminal","270 Olympic Drive SE, Bainbridge 98110",
  "Wheaton Way Transit Center","3915 Wheaton Way, Bremerton 98310",
  "West Bremerton Transit Center","540 Bruenn Ave, Bremerton 98312",
  "Silverdale Transit Center","Greaves Way & Kitsap Mall Blvd, Silverdale 98383",
  "North Viking Transit Center","21992 Viking Ave NW, Poulsbo 98370",
  "Kingston Ferry Terminal","11264 State Route 104, Kingston 98346",
  "Port Orchard Ferry Dock","73 Sidney Ave, Port Orchard 98366",
  "Southworth Ferry Terminal","11564 SE State Hwy. 160, Southworth 98386",
  "Annapolis Ferry Dock","1076 Bay Street, Port Orchard 98366",
  "Hwy. 305 & Suquamish Way","16003 WA-305, Poulsbo, Washington 98370",
  "McWilliams Park & Ride","1601 NE McWilliams Rd, Bremerton, Washington 98311",
  "Suquamish Park & Ride","18829 Division Ave NE, Suquamish, Washington 98392",
  "Port Orchard Wal-Mart","3497 Bethel Rd SE, Port Orchard 98366",
  "George's Corner Park & Ride","27618 Hansville Rd NE, Kingston 98346",
  "Gateway Fellowship Park & Ride","18901 8th Ave NE, Poulsbo 98370",
  "Miller Bay & Indianola","23404 Miller Bay Rd NE Poulsbo, Washington 98370",
  "St. Gabriel's Church","1150 Mitchell Ave SE, Port Orchard 98366"
)
# geocode the addresses into geocode = c(lon, lat)
transit_cp <- transit_cp %>% #tmaptools::geocode_OSM(transit_cp$ADDRESS)
  rowwise() %>%
  mutate(geocode = list(set_names(mapboxapi::mb_geocode(ADDRESS),
                                  c("lon","lat")))) %>%
  unnest_wider(geocode)

# convert the lat lon into a point geometry
transits <- st_as_sf(transit_cp, 
                     coords = c(x = "lon", y = "lat"), 
                     crs = 4326) 
transits2 <- getWalkingPolygonInCounty(transits, "NAME")
the_transits <- unique(st_set_geometry(transits2, NULL)$id)
loop_transits <- st_as_sf(transits2) %>% mutate(tomatch = id)


# combine data into tibble for this report
#These regions and pedestrian generators are:
kds <- "Kitsap County GIS"
kt <- "Kitsap Transit Website"
wd <- "Walking distance polygons from Mapbox"
breakdowns <- tribble(
  ~Name,~pointdf,~polydf,~idcolpoly,~items,~data_source,
  "Commissioner Districts",NA,"commdists","NAME","the_commdists",kds,
  "UGAs",NA,"uga_no_city","GMA_JURISD","the_ugas",kds,
  "LAMIRDs",NA,"lamird","PLAN_AREA","the_lamirds",kds,
  "Public Schools","pub_schools","pub_schools2","id","the_schools",paste(kds,wd),
  "Public Libraries","libraries","libraries2","id","the_libraries",paste(kds,wd),
  "Transit Centers","transits","transits2","id","the_transits",paste(kt,wd),
)

rm(kds, kt, wd)









