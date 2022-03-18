# combine shoulders and roadcl

library(tidyverse)
library(sf)
# get clean_roadcl
source("./functions/getCleanRoadCenterLine.R")
# get clean_shoulders
source("./functions/getCleanShoulders.R")


# where do ROAD_LOGS match, not match
shoulders_road_log <- clean_shoulders$ROAD_LOG_I
cl_road_log <- clean_roadcl$RD_LOG_ID
setdiff(shoulders_road_log,cl_road_log) #road logs in shoulder data NOT in centerline data
#[1] 76330 74072 10560 72990 29005 26135 80075 86676 86674 59070 86675 86673

shoulders_not_road <- clean_shoulders %>% 
  filter(ROAD_LOG_I %in% 
           setdiff(shoulders_road_log,cl_road_log))
# plot(shoulders_not_road)  #sparse
sum(shoulders_not_road$LINEAR_FEET, na.rm=TRUE) # total linear road foot
# [1] 10945.22# total linear feet of the road segments where we have shoulder data but no road

# what if we combined them
tmp <- clean_roadcl %>% 
  st_set_geometry(., NULL) %>%  # we don't need to keep the geometry here
  select(RD_LOG_ID, FULL_NAME, CRAB_BMP, CRAB_EMP) %>%
  left_join(select(clean_shoulders, -LINEAR_FEET), by=c("RD_LOG_ID" = "ROAD_LOG_I")) %>% 
  filter((BMP >= CRAB_BMP | EMP <= CRAB_EMP) & BMP < CRAB_EMP & EMP > CRAB_BMP) %>% #or
  mutate(S_miles = if_else((BMP >= CRAB_BMP & EMP <= CRAB_EMP), EMP - BMP, # entirely within
                           if_else(BMP < CRAB_BMP & EMP <= CRAB_EMP, EMP - CRAB_BMP, # starts before roadcl
                                   ifelse(BMP < CRAB_BMP, CRAB_EMP - CRAB_BMP, # shoulder before after roadcl
                                          CRAB_EMP - BMP)))) %>% # start safter ends after roadcl
  group_by(RD_LOG_ID, FULL_NAME, CRAB_BMP, CRAB_EMP, LEFT, RIGHT) %>%
  summarise(S_miles = sum(S_miles, na.rm = TRUE), 
            .groups = "drop") %>% # all of the shoulder included
  rowwise() %>%
  mutate(R_miles = CRAB_EMP - CRAB_BMP,
         Portion_Known_Shoulder = S_miles/R_miles)

#portion that we have with shoulder data
nrow(tmp)/nrow(clean_roadcl) 
#[1] 0.1006701
sum(tmp$R_miles, na.rm = TRUE)/(sum(clean_roadcl$LENGTH, na.rm = TRUE)/5280)
#[1] 0.1421125

# combine that back with the roadcl data
clean_roadcl_w_shoulders <- left_join(clean_roadcl, select(tmp, -R_miles))


rm(tmp)