## code to prepare `park_trail_geog` dataset goes here

pkgload::load_all()
requireNamespace("readxl", quietly = TRUE)
requireNamespace("fs", quietly = TRUE)
requireNamespace("tigris", quietly = TRUE)
requireNamespace("janitor", quietly = TRUE)

library(dplyr)
library(fs)
library(sf)
library(tigris)
library(janitor)

## NameCleaner-----------------------------------------------------------------------
#update from Darcie conversation 16 oct 2020
namecleaner <- tribble(~AGENCY, ~consistentagency,
                       "Anoka County Parks and Recreation",  "Anoka County",
                       "Anoka County Parks" , "Anoka County",
                       "Anoka County" , "Anoka County",
                       "Bloomington Parks and Recreation", "Bloomington",
                       "Bloomington" , "Bloomington",
                       "City of Bloomington" , "Bloomington",
                       "Carver County Parks and Recreation","Carver County",
                       "Carver County Parks" , "Carver County",
                       "Carver County" , "Carver County",
                       "Ramsey County Parks and Recreation","Ramsey County",
                       "Ramsey County" , "Ramsey County",
                       "Dakota County Parks","Dakota County",
                       "Dakota County" , "Dakota County",
                       "Minneapolis Park and Recreation Board","Minneapolis Park and Recreation Board",
                       "Minneapolis" , "Minneapolis Park and Recreation Board",
                       "Washington County Parks","Washington County",
                       "Washington County" , "Washington County",
                       "St. Paul Parks and Recreation","St. Paul",
                       "St Paul Parks And Recreation" , "St. Paul",
                       "St Paul Parks and Recreation" , "St. Paul",
                       "St. Paul" , "St. Paul",
                       "Scott County / Three Rivers Park District" , "Scott County", #this is the Scott County Regional Trail
                       "Scott County/Three Rivers Park District", "Scott County",
                       "Scott County Parks","Scott County",
                       "Scott County" , "Scott County",
                       "Three Rivers Park District", "Three Rivers Park District",
                       "Three Rivers" , "Three Rivers Park District")

# namecleaner <- tribble(~AGENCY, ~consistentagency,
#                        "Anoka County Parks and Recreation",  "Anoka County Parks and Recreation",
#                        "Anoka County Parks" , "Anoka County Parks and Recreation",
#                        "Anoka County" , "Anoka County Parks and Recreation",
#                        "Bloomington Parks and Recreation", "Bloomington Parks and Recreation",
#                        "Bloomington" , "Bloomington Parks and Recreation",
#                        "City of Bloomington" , "Bloomington Parks and Recreation",
#                        "Carver County Parks and Recreation","Carver County Parks and Recreation",
#                        "Carver County Parks" , "Carver County Parks and Recreation",
#                        "Carver County" , "Carver County Parks and Recreation",
#                        "Ramsey County Parks and Recreation","Ramsey County Parks and Recreation",
#                        "Ramsey County" , "Ramsey County Parks and Recreation",
#                        "Dakota County Parks","Dakota County Parks",
#                        "Dakota County" , "Dakota County Parks",
#                        "Minneapolis Park and Recreation Board","Minneapolis Park and Recreation Board",
#                        "Minneapolis" , "Minneapolis Park and Recreation Board",
#                        "Washington County Parks","Washington County Parks",
#                        "Washington County" , "Washington County Parks",
#                        "St. Paul Parks and Recreation","St. Paul Parks and Recreation",
#                        "St Paul Parks And Recreation" , "St. Paul Parks and Recreation",
#                        "St Paul Parks and Recreation" , "St. Paul Parks and Recreation",
#                        "St. Paul" , "St. Paul Parks and Recreation",
#                        "Scott County / Three Rivers Park District" , "Scott County Parks", #this is the Scott County Regional Trail
#                        "Scott County/Three Rivers Park District", "Scott County Parks",
#                        "Scott County Parks","Scott County Parks",
#                        "Scott County" , "Scott County Parks",
#                        "Three Rivers Park District", "Three Rivers Park District",
#                        "Three Rivers" , "Three Rivers Park District")

## Parks -----------------------------------------------------------------------
temp <- tempfile()
download.file("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional/gpkg_plan_parks_regional.zip",
  destfile = temp
)

parks_temp <- sf::read_sf(unzip(temp, "plan_parks_regional.gpkg")) %>%
  # filter(STATUS == "Existing") %>%
  left_join(namecleaner) %>%
  mutate(STATUS = recode(STATUS, "Existing" = "Park - existing",
                         "In Master Plan" = "Park - planned",
                         "Planned" = "Park - planned")) %>%
  group_by(PARKNAME, STATUS, Label, consistentagency) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = Label,
    agency = consistentagency,
    status = STATUS,
  ) %>%
  st_transform(4326) %>%
  st_as_sf()

parks <- parks_temp %>%
  filter(status == "Park - existing")

parks_planned <- parks_temp %>%
  filter(status == "Park - planned")

fs::file_delete("plan_parks_regional.gpkg")

## Trails ----------------------------------------------------------------------
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_regional_trails_exst_plan/gpkg_trans_regional_trails_exst_plan.zip",
  destfile = temp
)


trails_temp <- sf::read_sf(unzip(temp, "trans_regional_trails_exst_plan.gpkg")) %>%
  # filter(STATUS == "Existing (Open to Public)") %>%
  filter(NAME != "River Crossing",
         Agency != "Wright County") %>% #Crow River Regional Trail doesn't seem to belong to any particular Metro Agency
  mutate(STATUS = recode(STATUS, "Existing (Open to Public)" = "Trail - existing",
                         "Alternate" = "Trail - planned/closed/alt.",
                         "Existing (Not Open to Public)" = "Trail - planned/closed/alt.",
                         "Planned" = "Trail - planned/closed/alt.")) %>%
  rename(AGENCY = Agency) %>%
  left_join(namecleaner) %>%
  # mutate(consistentagency = ifelse(name == "Scott County Regional Trail" & is.na(consistentagency), 
  #                                  "Scott County Parks", consistentagency)) %>% #should confirm this is the right agency
  group_by(NAME, STATUS, Label, consistentagency) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = NAME,
    agency = consistentagency,
    status = STATUS
  ) %>%

  st_transform(4326) %>%
  st_as_sf() %>%
  sf::st_make_valid()

trails <- trails_temp %>% filter(status == "Trail - existing")

trails_planned <- trails_temp %>% filter(status == "Trail - planned/closed/alt.")

fs::file_delete("trans_regional_trails_exst_plan.gpkg")


## Trail Search ----------------------------------------------------------------------
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_regional_trails_search_cor/gpkg_trans_regional_trails_search_cor.zip",
              destfile = temp
)

trailsearch <- sf::read_sf(unzip(temp, "trans_regional_trails_search_cor.gpkg")) %>%
  left_join(namecleaner) %>%
  group_by(NAME, consistentagency) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = NAME,
    agency = consistentagency,
  ) %>%
    mutate(status = "Trail - search") %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  sf::st_make_valid()


fs::file_delete("trans_regional_trails_search_cor.gpkg")


## Park Search ----------------------------------------------------------------------
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional_search_areas/gpkg_plan_parks_regional_search_areas.zip",
              destfile = temp
)

parksearch <- sf::read_sf(unzip(temp, "plan_parks_regional_search_areas.gpkg")) %>%
group_by(NAME, Label) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = NAME,
  ) %>%
  mutate(status = "Park - search") %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  sf::st_make_valid()


fs::file_delete("plan_parks_regional_search_areas.gpkg")

## Combine ---------------------------------------------------------------------
park_trail_geog <- list(parks, parks_planned, trails, trails_planned,
                        trailsearch, parksearch)
names(park_trail_geog) <- c(
  "park",
  "park_planned",
  "trail",
  "trail_planned",
  "trail_search",
  "park_search"
)

usethis::use_data(park_trail_geog, overwrite = TRUE)

usethis::use_git_ignore(".DS_Store")
