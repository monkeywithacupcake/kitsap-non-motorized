# explore shoulder data

# AASHTO minimum shoulder widths (ft)
shoulder_mins <- tribble( ~road_class, ~min_ft,
                          "Minor Collector", 5,
                          "Major Collector", 6.5,
                          "Minor Arterial", 7,
                          "Principal Arterial", 8
)

df <- clean_roadcl_w_shoulders %>% 
  left_join(shoulder_mins)

df %>%
  st_set_geometry(., NULL) %>% 
  mutate(LEFT_m = LEFT >= min_ft,
         RIGHT_m = RIGHT >= min_ft,
         shoulder_status = if_else(LEFT_m & RIGHT_m, "Both Sides",
                                   if_else(!LEFT_m & !RIGHT_m, "Neither",
                                           "One Side"))) %>%
  group_by(road_class, shoulder_status) %>%
  summarise(R_miles = sum(LENGTH, na.rm = TRUE)/5280,
            S_miles = sum(S_miles, na.rm = TRUE),
            .groups = "drop") %>% 
  pivot_wider(names_from = shoulder_status, values_from = S_miles) %>%
  group_by(road_class) %>%
  summarise(across(everything(), .f = sum, na.rm = TRUE),
            .groups = "drop") %>%
  select(-`NA`) %>%
  mutate(Unknown = R_miles - `Both Sides` - Neither - `One Side`,
         Percent_Complete = scales::percent(`Both Sides`/R_miles))


## what about according to the PSRC data
outline <- sf::read_sf("./data/outline")
st_crs(outline)$epsg
outline <- outline %>%
  st_transform(crs = 2285)
kitsap_psrc <- psrc_bikeped %>%
  st_intersection(outline)
kitsap_psrc %>%
  st_set_geometry(., NULL) %>% 
  group_by(facility_type, ped_complete) %>%
  summarise(R_miles = sum(length_ft, na.rm = TRUE)/5280,
            S_miles = sum(length_ft, na.rm = TRUE)/5280,
            .groups = "drop") %>% 
  pivot_wider(names_from = ped_complete, values_from = S_miles) %>%
  group_by(facility_type) %>%
  summarise(across(everything(), .f = sum, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Percent_Complete = scales::percent(`Complete Facilities`/R_miles))

# just county arterials
st_crs(psrc_bikeped)$epsg
st_crs(cities)$epsg
cities <- cities %>%
  st_transform(crs = 2285)
cities_psrc <- lengths(st_intersects(kitsap_psrc, cities)) > 0
kitsap_psrc <- kitsap_psrc %>% 
  bind_cols(city = cities_psrc)
county_psrc <- kitsap_psrc %>%
  filter(!city) %>%
  select(-city)


county_psrc %>%
  st_set_geometry(., NULL) %>% 
  group_by(facility_type, ped_complete) %>%
  summarise(R_miles = sum(length_ft, na.rm = TRUE)/5280,
            S_miles = sum(length_ft, na.rm = TRUE)/5280,
            .groups = "drop") %>% 
  pivot_wider(names_from = ped_complete, values_from = S_miles) %>%
  group_by(facility_type) %>%
  summarise(across(everything(), .f = sum, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Percent_Complete = scales::percent(`Complete Facilities`/R_miles))
