# get Sidewalks
library(tidyverse)
library(sf)

sidewalks <- sf::read_sf("./data/KitsapShapefiles03_04_2022/SIDEWALKS")

colnames(sidewalks)
# [1] "SURFTYPE"   "ADACOMPLY"  "CONDITION"  "WIDTH"      "INSTALLDAT" "OWNEDBY"    "MAINTBY"   
# [8] "SIDEWALKID" "MODIFIED_B" "MODIFIED_D" "CREATED_BY" "CREATED_DT" "ROADNAME"   "ROADLOGID" 
# [15] "LENG_FT"    "EDGETYPE"   "DISTRICT"   "DISTRICT_S" "SEG_ID"     "BMP"        "EMP"       
# [22] "DEFICIENCY" "SIDE_OF_RO" "CURBWIDTH"  "SHAPE_LEN"  "geometry" 

# build a dictionary backwards
sidewalks_dict <- data.frame(col = colnames(sidewalks),
                             def = c("Surface Type",
                                     "ADA Compliant Sidewalk",
                                     "Condition of Sidewalk",
                                     "Width of Sidewalk Surface",
                                     "Not Used",
                                     "Not Used",
                                     "Not Used",
                                     "Unique Id for Sidewalk",
                                     "who modified data",
                                     "date of modification of data",
                                     "who created data",
                                     "date of creation of data",
                                     "Name used for road",
                                     "Road Log Id - from Public Works",
                                     "Distance of segment, in Feet",
                                     "Edge of sidewalk format",
                                     "Commissioner District",
                                     "Not Used",
                                     "Unique ID for Road Segment In theory, but 0,1, NA",
                                     "Beginning Mile Point",
                                     "Ending Mile Point",
                                     "Not Used",
                                     "Side of road sidewalk desc for",
                                     "Width of Curb (units?)",
                                     "?",
                                     "MULTILINESTRING geometry"),
                             vals = apply(sidewalks, 2, function(x) ifelse(n_distinct(x) > 10, "many", paste(unique(x), collapse = ", "))) %>% as.vector()
)

# check for negative distance
sidewalks %>%
  st_set_geometry(., NULL) %>% 
  rowwise() %>% 
  mutate(negative_dist = BMP > EMP) %>% 
  group_by(negative_dist) %>% 
  summarise(n = n())
# negative_dist     n
# <lgl>         <int>
# 1 FALSE          1349
# 2 TRUE            395
# why negative distance? ASK GIS

# check for miles v feet
sidewalks %>%
  st_set_geometry(., NULL) %>% 
  rowwise() %>% 
  mutate(dif = EMP - BMP,
         f_miles = LENG_FT/5280,
         miles_equal_feet = dif > 0.9 * f_miles & dif < 1.1 * f_miles) %>%
  group_by(miles_equal_feet) %>% #within 10% of f_miles
  summarise(n = n())
# miles_equal_feet     n
# <lgl>            <int>
# 1 FALSE             1216
# 2 TRUE               528
# why so many false? ASK GIS


# probably should not do this - BMP and EMP are unreliable
by_dist_road_side <- sidewalks %>% 
  st_set_geometry(., NULL) %>%  # we don't need to keep the geometry here
  filter(WIDTH > 0) %>% # if doing it, only keep ones worth marking
  group_by(ROADLOGID, ROADNAME, BMP, EMP, SIDE_OF_RO, WIDTH) %>%
  summarise(LINEAR_FEET = sum(LENG_FT, na.rm=TRUE), 
            .groups = "drop") %>%
  filter(!is.na(SIDE_OF_RO), !is.na(WIDTH)) %>% # should not include
  mutate(SIDE_OF_RO = recode(SIDE_OF_RO,
                             "Left" = "LEFT",
                             .default = SIDE_OF_RO))

by_dist_road_distance <- by_dist_road_side %>%
  pivot_wider(names_from = "SIDE_OF_RO", values_from = "WIDTH"
  ) 
# there are NO ROADLOGID with both sides
nrow(by_dist_road_side) == nrow(by_dist_road_distance)
# [1] TRUE

# clean_sidewalks are not very clean
clean_sidewalks <- by_dist_road_distance

rm(sidewalks, by_dist_road_distance, by_dist_road_side)

# where do ROAD_LOGS match, not match
sidewalks_road_log <- clean_sidewalks$ROADLOGID
cl_road_log <- clean_roadcl$RD_LOG_ID
setdiff(sidewalks_road_log,cl_road_log) #road logs in shoulder data NOT in centerline data
#[1] "16010" "41950" "56797" "56802" "57750" "83699" "84820" "84821" "86674" "86675" "86676"

sidewalks_not_road <- clean_sidewalks %>% 
  filter(ROADLOGID %in% 
           setdiff(sidewalks_road_log,cl_road_log))
# plot(sidewalks_not_road)  #sparse
sum(sidewalks_not_road$LINEAR_FEET, na.rm=TRUE) # total linear road foot
# [1] 4532.701 # total linear feet of the road segments where we have sidewalk data but no road


# what if we combined them
clean_sidewalks <- clean_sidewalks %>%
  mutate(SW_LEFT = LEFT,
         SW_RIGHT = RIGHT,
         SW_BMP = BMP,
         SW_EMP = EMP,
         ROADLOGID = as.numeric(ROADLOGID))
tmp <- clean_roadcl %>% 
  st_set_geometry(., NULL) %>%  # we don't need to keep the geometry here
  select(RD_LOG_ID, FULL_NAME, CRAB_BMP, CRAB_EMP) %>%
  left_join(clean_sidewalks, by=c("RD_LOG_ID" = "ROADLOGID")) %>% 
  filter((SW_BMP >= CRAB_BMP | SW_EMP <= CRAB_EMP) & SW_BMP < CRAB_EMP & SW_EMP > CRAB_BMP) %>% #or
  mutate(SW_miles = if_else((SW_BMP >= CRAB_BMP & SW_EMP <= CRAB_EMP), SW_EMP - SW_BMP, # entirely within
                            if_else(SW_BMP < CRAB_BMP & SW_EMP <= CRAB_EMP, SW_EMP - CRAB_BMP, # starts before roadcl
                                    ifelse(SW_BMP < CRAB_BMP, CRAB_EMP - CRAB_BMP, # shoulder before after roadcl
                                           CRAB_EMP - SW_BMP)))) %>% # start safter ends after roadcl
  group_by(RD_LOG_ID, FULL_NAME, CRAB_BMP, CRAB_EMP, SW_LEFT, SW_RIGHT) %>%
  summarise(SW_miles = sum(SW_miles, na.rm = TRUE), 
            .groups = "drop") %>% # all of the shoulder included
  rowwise() %>%
  mutate(R_miles = CRAB_EMP - CRAB_BMP,
         Portion_Known_Sidewalk = SW_miles/R_miles)

tmp <- clean_roadcl %>% 
  st_set_geometry(., NULL) %>%  # we don't need to keep the geometry here
  select(RD_LOG_ID, FULL_NAME, CRAB_BMP, CRAB_EMP) %>%
  left_join(select(clean_sidewalks, -LINEAR_FEET), by=c("RD_LOG_ID" = "ROADLOGID")) %>% 
  filter((SW_BMP >= CRAB_BMP | SW_EMP <= CRAB_EMP) & SW_BMP < CRAB_EMP & SW_EMP > CRAB_BMP) %>% #or
  mutate(SW_miles = if_else((SW_BMP >= CRAB_BMP & SW_EMP <= CRAB_EMP), SW_EMP - SW_BMP, # entirely within
                           if_else(SW_BMP < CRAB_BMP & SW_EMP <= CRAB_EMP, SW_EMP - CRAB_BMP, # starts before roadcl
                                   ifelse(SW_BMP < CRAB_BMP, CRAB_EMP - CRAB_BMP, # shoulder before after roadcl
                                          CRAB_EMP - SW_BMP)))) %>% # start safter ends after roadcl
  group_by(RD_LOG_ID, FULL_NAME, CRAB_BMP, CRAB_EMP, SW_LEFT, SW_RIGHT) %>%
  summarise(SW_miles = sum(SW_miles, na.rm = TRUE), 
            .groups = "drop") %>% # all of the shoulder included
  rowwise() %>%
  mutate(R_miles = CRAB_EMP - CRAB_BMP,
         Portion_Known_Sidewalk = SW_miles/R_miles)

#portion that we have with sidewalk data
nrow(tmp)/nrow(clean_roadcl) 
#[1] 0.009929765
sum(tmp$R_miles, na.rm = TRUE)/(sum(clean_roadcl$LENGTH, na.rm = TRUE)/5280)
#[1] 0.007842078

# combine that back with the roadcl data
clean_roadcl_w_both <- left_join(clean_roadcl, select(tmp, -R_miles))

# caution!! clean_sidwalks is NOT CLEAN