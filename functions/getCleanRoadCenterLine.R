# clean roadcl

library(tidyverse)
library(sf)

roadcl <- sf::read_sf("./data/roadcl") # this is the road centerline data (kitsap GIS)


# first, have to address issues with roadcl where there are 
# dummy RD_LOG_IDs and there are repeated BMP/EMP for different segments (looks like error)
roadcl_mileage_issue <- roadcl %>%
  st_set_geometry(., NULL) %>%  # we don't need to keep the geometry here
  group_by(RD_LOG_ID, FULL_NAME, CRAB_BMP, CRAB_EMP) %>% 
  summarise(n_segments = n(), .groups = "drop") %>%
  filter(n_segments > 1)
roadcl_mileage_issue_exclude_0 <- roadcl_mileage_issue %>%
  filter(CRAB_EMP != 0)
# need to redo these ones. too many to do manual, as I started
tmp <- roadcl %>%
  filter(RD_LOG_ID %in% roadcl_mileage_issue_exclude_0$RD_LOG_ID) %>% 
  arrange(RD_LOG_ID, FR_ADDR_L, FR_ADDR_R) %>%
  filter(TO_ADDR_L != 0 & TO_ADDR_R != 0) %>%
  filter(RD_LOG_ID != 0 & RD_LOG_ID != 200) %>%
  group_by(RD_LOG_ID) %>% 
  arrange(FR_ADDR_L, FR_ADDR_R) %>%
  mutate(CUM_FT = cumsum(LENGTH),
         RANK = 1:n(),
         MILES = LENGTH/5280, # this is the miles of THIS STRETCH,
         CUM_MILES = cumsum(MILES),
         CUM_MILES_FROM_START = CUM_MILES + first(CRAB_BMP),
         BMP = ifelse(RANK == 1, CRAB_BMP, lag(CUM_MILES_FROM_START)),
         EMP = BMP + MILES) %>% 
  arrange(RD_LOG_ID, FR_ADDR_L, FR_ADDR_R) %>%
  mutate(CRAB_BMP = BMP, CRAB_EMP = EMP) %>%
  select(-CUM_FT, -RANK, -MILES, -CUM_MILES, -CUM_MILES_FROM_START, -BMP, -EMP) %>%
  ungroup()

# clean up
clean_roadcl <- roadcl %>%
  filter(TO_ADDR_L != 0 & TO_ADDR_R != 0) %>%
  filter(RD_LOG_ID != 0 & RD_LOG_ID != 200) %>% #place
  filter(!RD_LOG_ID %in% tmp$RD_LOG_ID) %>%
  bind_rows(tmp)



# and now get some categories

clean_roadcl <- clean_roadcl %>%
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


rm(tmp, roadcl, roadcl_mileage_issue, roadcl_mileage_issue_exclude_0)

