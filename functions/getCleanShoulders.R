# explore shoulders data

library(tidyverse)
library(sf)

# read in data
shoulders <- sf::read_sf("./data/KitsapShapefiles03_04_2022/SHOULDERS") # shoulder data (email)
#
colnames(shoulders)
#[1] "ROAD_LOG_I" "BMP"        "EMP"        "ROAD_NAME"  "DISTANCE_M" "DISTANCE_F" "SIDE_OF_RO"
#[8] "SEG_ID"     "COMPOSITE_" "SHLDR_SURF" "SHLDR_WIDT" "MODIFIED_B" "MODIFIED_D" "CREATED_BY"
#[15] "CREATED_DT" "DISTRICT"   "DIST_SECTI" "CRIT_AREA"  "SHAPE_LEN"  "geometry" 

# build a dictionary backwards
shoulders_dict <- data.frame(col = colnames(shoulders),
                             def = c("Road Log Id - from Public Works",
                                     "Beginning Mile Point",
                                     "Ending Mile Point",
                                     "Name used for road",
                                     "Distance of segment, in Miles",
                                     "Distance of segment, in Feet",
                                     "Side of road shoulder desc for",
                                     "Unique ID for Road Segment",
                                     "combination of Road Log Id, BMP, and EMP",
                                     "Surface material of shoulder",
                                     "Width of shoulder surface",
                                     "who modified data",
                                     "date of modification of data",
                                     "who created data",
                                     "date of creation of data",
                                     "Commissioner District",
                                     "?",
                                     "?",
                                     "",
                                     "MULTILINESTRING geometry"),
                             vals = apply(shoulders, 2, function(x) ifelse(n_distinct(x) > 10, "many", paste(unique(x), collapse = ", "))) %>% as.vector()
                             )


#plot(shoulders) 
# plots first 10 attributes with geometry

# check for negative distance
shoulders %>%
  st_set_geometry(., NULL) %>% 
  rowwise() %>% 
  mutate(negative_dist = BMP > EMP) %>% 
  group_by(negative_dist) %>% 
  summarise(n = n())
# negative_dist     n
# <lgl>         <int>
# 1 FALSE          3390
# good - no negative, but there are 15 with 0 distance in Miles

# check for miles v feet
shoulders %>%
  st_set_geometry(., NULL) %>% 
  rowwise() %>% 
  mutate(dif = EMP - BMP,
         f_miles = DISTANCE_F/5280,
         miles_equal_feet = dif > 0.9 * f_miles & dif < 1.1 * f_miles) %>%
  group_by(miles_equal_feet) %>% #within 10% of f_miles
  summarise(n = n())
# miles_equal_feet     n
# <lgl>            <int>
# 1 FALSE              654
# 2 TRUE              2736


by_dist_road_side <- shoulders %>% 
  st_set_geometry(., NULL) %>%  # we don't need to keep the geometry here
  group_by(ROAD_LOG_I, ROAD_NAME, BMP, EMP, SIDE_OF_RO, SHLDR_WIDT) %>%
  summarise(LINEAR_FEET = sum(DISTANCE_F, na.rm=TRUE), 
            .groups = "drop") %>%
  filter(!is.na(SIDE_OF_RO), !is.na(SHLDR_WIDT)) %>% # should not include
  mutate(SIDE_OF_RO = recode(SIDE_OF_RO,
                             "Left" = "LEFT",
                             .default = SIDE_OF_RO))

by_dist_road_distance <- by_dist_road_side %>%
  pivot_wider(names_from = "SIDE_OF_RO", values_from = "SHLDR_WIDT"
              ) 

# quick summary of linear feet of road with different shoulder width combinations. 
# Would have to be paired with road type to be useful for judge of pedestrian LOS
by_dist_road_distance %>%
  group_by(LEFT, RIGHT) %>%
  summarise(LINEAR_FEET = sum(LINEAR_FEET, na.rm = TRUE), 
            .groups = "drop") 

# shoulder data has to be matched with centerline data to get road information

clean_shoulders <- by_dist_road_distance

rm(shoulders, by_dist_road_distance, by_dist_road_side)

