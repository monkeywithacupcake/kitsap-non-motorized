# functions for the document

library(kableExtra)
library(tidyverse)
library(sf)

nice_comma <- function(x, dec = 1){
  format(round(as.numeric(x), dec), digits = NULL, nsmall=dec, big.mark=",") 
}
'%ni%' <- function(x,y)!('%in%'(x,y))


###########################
# number functions

getAggregateScore <- function(the_df) {
  # calculates with any df with w_score and the_length
  
  as.numeric(
        sum(the_df$w_score, na.rm = TRUE)/
          sum(the_df$the_length, na.rm=TRUE)
        )
}

getFormattedAggregateScore <- function(the_df) {
  # calculates with any df with w_score and the_length
  
  format(
    round(
      getAggregateScore(the_df),
      1), 
    digits = NULL, nsmall=1) 
}

getSideScore <- function(standard_sidewalk, standard_shoulder,
                         sidewalk, shoulder) {
  ifelse(is.na(standard_sidewalk), # dealing with shoulder
         case_when(
           !is.na(sidewalk) ~ 5, # sidewalk present when a shoulder is the standard
           shoulder >= standard_shoulder ~ 5,
           shoulder < standard_shoulder & shoulder >= 4 ~ 3, #aashto
           shoulder < standard_shoulder & shoulder > 0 ~ 1,
           TRUE ~ 0
         ),
         case_when(
           sidewalk >= standard_sidewalk ~ 5,
           sidewalk < standard_sidewalk & sidewalk > 4 ~ 3,
           sidewalk < standard_sidewalk & sidewalk > 0 ~ 1,
           shoulder >= standard_sidewalk ~ 2, # add for shoulder exist
           shoulder < standard_sidewalk & shoulder > 0 ~ 1,
           TRUE ~ 0
         )
  )
}

getScoreInterpretationTibble <- function(){
  score_interpretation <- tribble(
    ~score_at_least, ~interpretation,
    10, "Good",
    5, "Adequate",
    3, "Minimum",
    1, "Poor",
    0.001, "Very Poor",
    0, "Nothing"
  )
  score_interpretation
}

getRoadStandardsGivenTibble <- function(){
  # these are standards from Kitsap Road Standards
  road_standards_given <- tribble(
    ~setting, ~class, ~characteristics, ~sidewalk, ~shoulder, 
    "Urban", "Local Road", NA, 5, NA,
    "Urban", "Local Sub-collector", NA, 5, NA,
    "Urban", "Arterial", NA, 6, NA,
    "Urban", "Collector", NA, 6, NA,
    "Rural", "Local Road", NA, NA, 3,
    "Rural", "Local Sub-collector", NA, NA, 4,
    "Rural", "Collector", "ADT 400-750", NA, 3,
    "Rural", "Collector", "ADT 751-1000", NA, 4,
    "Rural", "Collector", "DHV 100-200", NA, 6,
    "Rural", "Collector", "DHV > 200", NA, 8,
    "Rural", "Minor Arterial", "DHV < 100", NA, 4,
    "Rural", "Minor Arterial", "DHV 100-200", NA, 6,
    "Rural", "Minor Arterial", "DHV > 200", NA, 8,
    "Rural", "Principal Arterial", "DHV <200", NA, 6,
    "Rural", "Principal Arterial", "DHV > 200", NA, 8,
  )
  road_standards_given
}

getRoadStandardsTibble <- function(){
  # this is a simplification of standards used in eval
  # because not enough data to use actual standards
  road_standards <- tribble(
    ~setting, ~class, ~sidewalk, ~shoulder,
    "Urban", "Local Access", 5, NA,
    "Urban", "Minor Collector",6, NA,
    "Urban", "Major Collector",6, NA,
    "Urban", "Minor Arterial", 6, NA,
    "Urban", "Principal Arterial", 6, NA,
    "Rural", "Local Access", NA, 3,
    "Rural", "Minor Collector",NA, 4,
    "Rural", "Major Collector",NA, 6,
    "Rural", "Minor Arterial", NA, 4,
    "Rural", "Principal Arterial", NA, 6,
  )
  road_standards
}

getScoreInterpretation <- function(x){
  si <- getScoreInterpretationTibble()
  first(si$interpretation[
    si$score_at_least <= x]
  )
}
# getScoreInterpretation(5)

################### 
# df functions


getCombScore2 <- function(ncdf, nc_sw_sh) { #needs ncdf and nc_sw_sh
  road_standards <- getRoadStandardsTibble() #above
  rs <- road_standards %>%
    rename(standard_sidewalk = sidewalk, 
           standard_shoulder = shoulder, 
           road_class = class)
  x <- st_as_sf(nc_sw_sh) %>% mutate(LENGTH = as.integer(LENGTH)) %>% filter(RD_LOG_ID == 49435 & LENGTH > 600)
  x$RD_LOG_ID <- 43809
  comb <- st_intersection(select(ncdf, RD_LOG_ID_R = RD_LOG_ID, 
                                 SEGMENT_ID_R = SEGMENT_ID,
                                 FULL_NAME_R = FULL_NAME, 
                                 LENGTH_R = LENGTH,
                                 road_class, RUCODE), 
                          st_buffer(bind_rows(nc_sw_sh, x), 50, endCapStyle="FLAT")) %>% 
    filter(RD_LOG_ID == RD_LOG_ID_R) %>%
    distinct(RD_LOG_ID_R, SEGMENT_ID_R, RD_LOG_ID, SEGMENT_ID, SIDE_OF_RO, .keep_all = TRUE) %>%
    mutate(distance = as.integer(st_length(.)))
  
  comb_stand <- comb %>% 
    st_set_geometry(., NULL) %>% 
    mutate(setting = ifelse(RUCODE == 1, "Rural", 
                            ifelse(RUCODE == 2, "Urban", "Unknown")),
           ped_type = sub("\\:.*", "", TYPE),
           nn = paste(sub("paved ","",tolower(ped_type)), tolower(SIDE_OF_RO),sep ="_")) %>%
    pivot_wider(id_cols = c(-RD_LOG_ID:-TYPE,-ped_type), 
                names_from = nn, 
                values_from = WIDTH,
                values_fn = mean) %>%
    left_join(rs) %>%
    rowwise() %>% # now make sure that have all the columns
    mutate(sidewalk_left = ifelse("sidewalk_left" %in% names(.), sidewalk_left, NA),
           sidewalk_right = ifelse("sidewalk_right" %in% names(.), sidewalk_right, NA),
           shoulder_left = ifelse("shoulder_left" %in% names(.), shoulder_left, NA),
           shoulder_right = ifelse("shoulder_right" %in% names(.), shoulder_right, NA)
    )
  comb_score <- comb_stand %>%
    mutate(score_left = getSideScore(standard_sidewalk, standard_shoulder, 
                                     sidewalk_left, shoulder_left),
           score_right = getSideScore(standard_sidewalk, standard_shoulder, 
                                      sidewalk_right, shoulder_right),
           score = score_left + score_right)
  st_set_geometry(ncdf, NULL) %>% 
    select(RD_LOG_ID_R = RD_LOG_ID, SEGMENT_ID_R = SEGMENT_ID,
           FULL_NAME_R = FULL_NAME, road_class, RUCODE, LENGTH) %>%
    mutate(setting = ifelse(RUCODE == 1, "Rural", 
                            ifelse(RUCODE == 2, "Urban", "Unknown"))) %>%
    left_join(comb_score) %>%
    mutate(the_score = ifelse(is.na(score), 0, score),
           the_length = ifelse(is.na(distance), LENGTH, distance),
           w_score = the_score*the_length)
}

getCombMapSFFromScoreAndNCDF <- function(ncdf, comb_score2){
  # requires ncdf
  # requires comb_score2
  score_interpretation <- getScoreInterpretationTibble()
  comb_map <- st_as_sf(ncdf) %>% 
    select(RD_LOG_ID_R = RD_LOG_ID, SEGMENT_ID_R = SEGMENT_ID,
           FULL_NAME_R = FULL_NAME, road_class, RUCODE, LENGTH) %>%
    mutate(setting = ifelse(RUCODE == 1, "Rural", 
                            ifelse(RUCODE == 2, "Urban", "Unknown"))) %>% 
    left_join(comb_score2) %>%
    rowwise() %>%
    mutate(interpretation = getScoreInterpretation(the_score))
  
  comb_map$interpretation <- factor(comb_map$interpretation, 
                                    levels =  score_interpretation$interpretation)
  
  write_csv(comb_map %>% st_set_geometry(., NULL), "comb_map.csv")
  
  
  comb_map %>%
    group_by(interpretation) %>%  # ".x" is refers to the current group:
    group_modify(~ st_union(.x) %>% as_tibble()) %>%
    ungroup() %>%
    st_as_sf() 
}

getGeoSFForRegion <- function(the_loop, region_title){
  #st_as_sf(loop_commdists) %>% filter(tomatch == "3 Central")
  st_as_sf(the_loop) %>% filter(tomatch == region_title)
}


getCombSFForRegion <- function(region_geo){
  # will be combscore2 for region
  if (!is.na(st_crs(region_geo)$epsg)) {
    ncdf <- st_transform(ncdf, st_crs(this_geo)$epsg)
    nc_sw_sh <- st_transform(nc_sw_sh, st_crs(this_geo)$epsg)
  }
  this_roads <- st_intersection(ncdf, st_buffer(this_geo,0))
  this_sw_sh <- st_intersection(nc_sw_sh, st_buffer(this_geo,0))
  if (nrow(this_roads) & nrow(this_sw_sh) > 0) {
    out <- getCombScore2(this_roads, this_sw_sh)
  }
  else{
    out <- FALSE
  }
  
  out

}
getCombMapForRegion <- function(comb_map, region_geo){
  # will be comb map limited to the region
  if (!is.na(st_crs(region_geo)$epsg)) {
    comb_map <- st_transform(comb_map, st_crs(region_geo)$epsg)
  }
  st_intersection(comb_map, st_buffer(region_geo,0))
  
}

############################
# map functions

getPaletteSWSH <- function() {
  #sw_sh_col <- 
  scale_color_manual(values = c("Sidewalk: 3 ft"="#E41A1C",
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
}

getKitsapRoadMap <- function(kitsap, clean_roadcl, ncdf) {
  # kitsap is a base map
  # clean_roadcl is all county roads
  # ncdf is non city non highway non easement roads
  road_col <- scale_color_manual(values = c("Freeway / Expressway" = "#e41a1c",
                                            "Principal Arterial" = "#377eb8",
                                            "Minor Arterial" = '#4daf4a',
                                            "Major Collector" = "#984ea3",
                                            "Minor Collector" ='#ff7f00',
                                            "Local Access" = '#a65628',
                                            "Rural Local Access (e.g. easement or other type)" = '#f781bf',
                                            "Urban Local Access (e.g. easement or other type)" = '#ffff33'
  ))
  kitsap_all_roads <- kitsap +
    geom_sf(data = clean_roadcl, aes(color = road_class)) + 
    road_col +
    theme_void() +
    labs(title = "Roads in Kitsap County",
         subtitle = paste0(nice_comma(sum(st_length(clean_roadcl))/5280),
                           " centerline miles"),
         #caption = "Data source: Kitsap GIS",
         color = "")
  
  kitsap_roads_no_cities <- kitsap + 
    geom_sf(data = cities["NAME"], 
            aes(fill="NAME"),
            fill="#808080",
            alpha = 0.5,
            show.legend = FALSE) +
    geom_sf(data = ncdf, aes(color = road_class), show.legend = FALSE) + 
    road_col +
    theme_void() +
    labs(title = "No Cities, Easements, & Highways",
         subtitle = paste0(nice_comma(sum(st_length(ncdf))/5280),
                           " centerline miles"),
         caption = "Data source: Kitsap GIS")
  
  
  library(patchwork)
  
  p <- kitsap_all_roads + kitsap_roads_no_cities +
    plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
  
  detach("package:patchwork", unload=TRUE)
  p
}

getMapForRegionInCounty <- function(region_geo, region_title) {
  kitsap +
    geom_sf(data = region_geo, aes(fill = 1), lwd = 0, alpha = 0.8, show.legend = FALSE) +
    theme_void() +
    labs(title = region_title,
         color = "")
}

getMapFacilitiesSideForRegion <- function(region_geo, side) {
  if (!is.na(st_crs(region_geo)$epsg)) {
    ncdf <- st_transform(ncdf, st_crs(this_geo)$epsg)
    nc_sw_sh <- st_transform(nc_sw_sh, st_crs(this_geo)$epsg)
  }
  this_roads <- st_intersection(ncdf, st_buffer(this_geo,0))
  this_sw_sh <- st_intersection(nc_sw_sh, st_buffer(this_geo,0))
  
  sw_sh_col <- getPaletteSWSH()
  
  ggplot() +
    geom_sf(data = region_geo, aes(fill = 1), alpha = 0.4, lwd = 0, show.legend = FALSE) +
    geom_sf(data = this_roads, aes(color = 1), color = "#999999", alpha = 0.5) + 
    geom_sf(data = filter(this_sw_sh, 
                          SIDE_OF_RO == str_to_upper(side)), 
            aes(color = as.factor(TYPE)),
            show.legend = "line") +
     sw_sh_col +
    labs(title = str_to_title(side),
         color = "") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
}

getMapPatchworkForRegionWithScores <- function(region_geo, region_comb, region_title){
  p1 <- getMapForRegionInCounty(region_geo, region_title)
  p2 <- getMapScoreSummaryForRegion(region_geo, region_comb, region_title)
  library(patchwork)
  
  p1 + p2 +
    plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
  
  detach("package:patchwork", unload=TRUE)
}

getScoreSummaryMap <- function(base_plot, the_map_sf, geo_title) {
  # base_plot may be kitsap for whole county or
  # ggplot() and this_geo for smaller geometry
  # the_map_sf is a simple feature df with interpretation col
  
  score_col <- scale_color_manual(values = c("Good"="#4d9221",
                                             "Adequate"="#a1d76a",
                                             "Minimum" = "#e6f5d0",
                                             "Poor" = "#fde0ef",
                                             "Very Poor" = "#e9a3c9",
                                             "Nothing" = "#c51b7d"),
                                  na.value = "#808080")
  base_plot +
    geom_sf(data = the_map_sf, 
            aes(color = interpretation), 
            #size = 1.5, #make the line thicker
            show.legend = "line") + 
    score_col +
    theme_void() +
    labs(title = paste("Calculated Pedestrian Facility Score:",geo_title),
         caption = "Data source: Kitsap GIS, scores calculated",
         color = "Score Interpretation") 
  
}
#getScoreSummaryMapForRegion(this_geo, this_comb, "3 Central")
getMapScoreSummaryForRegion <- function(region_geo, region_comb, region_title) {
  # region_sf is this_geo, region_comb is this_comb, region_title is {{current_geo}}
  b <- ggplot() +
    geom_sf(data = region_geo, aes(fill = 1), color = NA, alpha = 0.6, show.legend = FALSE)
  getScoreSummaryMap(b, region_comb, region_title)
}

#getScoreSummaryMapOverall()
getMapScoreSummaryOverall <- function(kitsap, uga, cities, comb_map) {
  # region_sf is this_geo, region_comb is this_comb, region_title is {{current_geo}}
  b <- kitsap + 
    geom_sf(data = uga, aes(fill = "isuga"), 
            lty = 0,  
            show.legend = "fill") + 
    scale_fill_manual(values = c("#CCDFFB","#808080"), 
                      labels = c("In Urban Growth Area", "Cities"),
                      drop = FALSE) +
    geom_sf(data = cities["NAME"], 
            aes(fill="NAME"),
            fill="#808080",
            color = NA,
            show.legend = FALSE)
  
    p <- getScoreSummaryMap(b, comb_map, "Unincorporated Kitsap") 
    return(p + 
             labs(fill = "") +
             guides(
               color=guide_legend(override.aes=list(fill=NA), order = 1),
               fill=guide_legend(override.aes=list(color=NA), order = 0)) 
           ) 
  
}

############################
# kable functions

getKableRoadMiles <- function(road_df) { # road_df is ncdf
  groupcl <- road_df %>%  
    st_set_geometry(., NULL) %>% 
    group_by(road_class) %>% 
    summarise(roads = nice_comma(n_distinct(FULL_NAME),0), 
              segments = nice_comma(n(),0), 
              length = nice_comma(sum(LENGTH, na.rm=TRUE)/5280, 1),
              .groups = "drop") 
  df <- road_df %>% #clean_roadcl %>%  
    st_set_geometry(., NULL) %>% 
    group_by(road_class = "Total") %>% 
    summarise(roads = nice_comma(n_distinct(FULL_NAME),0), 
              segments = nice_comma(n(),0), 
              length = nice_comma(sum(LENGTH, na.rm=TRUE)/5280, 1),
              .groups = "drop") 
  groupcl <- bind_rows(groupcl, df)
  
  colnames(groupcl) <- c("Road Classification", 
                         "Count of Roads", "Count of Segments", 
                         "Length in Miles") 
  knitr::kable(groupcl, 
               caption = "Roads by Class in Kitsap County, excluding cities, highways, and easements")  %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
}

getKableFacilityMiles <- function(facility_df, facility_name, facility_footnote) {
  df <- facility_df %>%  
    st_set_geometry(., NULL) %>%
    mutate(`Width in Feet` = paste(WIDTH, "ft")) %>%
    group_by(WIDTH, `Width in Feet` ) %>% # road_type) %>%
    summarise(miles = nice_comma(sum(LENGTH, na.rm=TRUE)/5280,1),
              .groups = "drop") %>%
    select(-WIDTH)
  df2 <- facility_df %>%  
    st_set_geometry(., NULL) %>%
    mutate(`Width in Feet` = "Total", WIDTH = 100) %>%
    group_by(WIDTH, `Width in Feet` ) %>% # road_type) %>%
    summarise(miles = nice_comma(sum(LENGTH, na.rm=TRUE)/5280,1),
              .groups = "drop") %>%
    select(-WIDTH)
  
  df <- bind_rows(df, df2)
  
  knitr::kable(df, 
               caption = paste("Miles of ",facility_name, "by Width (single side)")) %>%
    footnote(general = facility_footnote) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
}

getKableRoadStandardsGiven <- function() {
  road_standards_given <- getRoadStandardsGivenTibble()
  knitr::kable(road_standards_given,
               caption = "Summary of Tables 3.3 and 3.4 in Kitsap County Road Standards [@kc_road_standards]")  %>%
    footnote(general = "Within the road standards, the minimum shoulder width may be reduced to the minimum required by AASHTO, which would be 4-8 feet, depending on the ADT. Note: sub-collector, ADT, and DHV are not defined in the publicly available road data.") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
}
getKableRoadStandards <- function() {
  road_standards <- getRoadStandardsTibble()
  knitr::kable(road_standards,
               caption = "Simplified Assumptions to Allow for Evaluation")  %>%
    footnote(general = "This is only a simplification to allow for an evaluation without knowledge of ADT or DHV on the roads under evaluation") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
}

getScoreInterpretationKable <- function() {
  df <- getScoreInterpretationTibble() %>%
    mutate(`Score At Least` = ifelse(score_at_least == 0.001, "> 0", 
                                     nice_comma(score_at_least,0))) %>%
    select(`Score At Least`, Interpretation = interpretation)
  
  knitr::kable(df,
               caption = "Interpretation of Scores for Segments and for Areas")  %>%
    footnote(general = "For a segment, adequate means one side is good or both sides meet AASHTO minimums and minimal means one side meets AASHTO minimums") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")
    )
}

getKableExampleAggregateScore <- function(){
  example_tbl <- tribble(
    ~Segment, ~"Segment Score", ~"Segment Centerline Linear Feet",
    "A", 6, 100,
    "B", 0, 10,
    "C",10,50,
    "D",8,200,
    "Total",7.5 ,360,
  )
  
  knitr::kable(example_tbl) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = F, 
                  position = "float_left")
}


getKableScoreSummaryForSetting <- function(df) {
  comb_score_by_setting <- df %>%
    group_by(setting) %>%
    summarise(score = sum(w_score)/sum(the_length),
              .groups = "drop") %>%
    rowwise() %>%
    mutate(interpretation = getScoreInterpretation(score))
  knitr::kable(comb_score_by_setting %>%
                 rename(Setting = setting,
                        Score = score,
                        Interpretation = interpretation) %>%
                 mutate(Score = ifelse(Score == 0, "0", nice_comma(Score, 1))),
               caption = "Weighted Scores by Setting")  %>%
    footnote(general = "Unknown setting is scored as if it is Urban."
    ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  
}

# getKableScoreSummaryForStandards(comb_score2)
getKableScoreSummaryForStandards <- function(df) {
  comb_score_by_setting_class <- df %>%
    group_by(setting, road_class) %>%
    summarise(score = sum(w_score)/sum(the_length),
              .groups = "drop") %>%
    rowwise() %>%
    mutate(interpretation = getScoreInterpretation(score))
  knitr::kable(comb_score_by_setting_class %>%
                 rename(Setting = setting,
                        Class = road_class,
                        Score = score,
                        Interpretation = interpretation) %>%
                 mutate(Score = ifelse(Score == 0, "0", nice_comma(Score, 1))),
               caption = "Weighted Scores by Standard Group")  %>%
    footnote(general = "Unknown setting is scored as if it is Urban."
    ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
}


getKableScoreSummaryForMap <- function(the_map) {
  # map is a simple feature df with 
  # interpretation col for each row
  # used overall and for any smaller geometry
  k <- the_map %>% 
    mutate(LENGTH = as.integer(st_length(.))) %>%
    st_set_geometry(., NULL)
  knitr::kable(
    k %>% 
      group_by(interpretation) %>%
      summarise(Miles = nice_comma(sum(LENGTH)/5280,2),
                .groups = "drop") %>%
      bind_rows(k %>% 
                  mutate(interpretation = "Total") %>%
                  group_by(interpretation) %>%
                  summarise(Miles = nice_comma(sum(LENGTH)/5280,1),
                            .groups = "drop")
      ) %>%
      rename(`Score Interpretation` = interpretation)
    
  ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")
                  ) %>%
    footnote(general = "If Total miles here is more than overall miles it is indicative of multiple shoulder or sidewalk geometries matching the same portion of road centerline on the same side of the road. This kind of data error is expected given the state of the sidewalk and shoulder data.")
  
}


######################
# mapbox functions
getWalkingPolygonInCounty <- function(locdf, idcol) {
  # finds the items in locdf that are in the target area
  # then finds the polygon around them
  # then limits that again to the area in the target (because the polygon may go into a city)
  if (!is.na(st_crs(locdf)$epsg)) { # the locdf has a epsg and cnc is NA
    cnc <- st_transform(county_no_cities, st_crs(locdf)$epsg)
    fdf <- st_intersection(locdf, st_buffer(cnc,0)) 
  } else {
    fdf <- st_intersection(locdf, st_buffer(county_no_cities,0)) 
  }
  fdf2 <- mapboxapi::mb_isochrone(fdf,
                                  time = 15, "walking", id_column=idcol)
  cnc <- st_transform(county_no_cities, st_crs(fdf2)$epsg)
  st_intersection(fdf2, st_buffer(cnc,0)) 
}







### END OF FILE