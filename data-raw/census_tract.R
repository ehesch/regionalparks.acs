## code to prepare `census_tract` dataset goes here

date <- format(Sys.time(), "%Y%m%d")
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

temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_census_acs/xlsx_society_census_acs.zip",
  destfile = temp
)

ct <- readxl::read_xlsx(unzip(temp, "CensusACSTract.xlsx")) %>%
  janitor::clean_names() %>%
  filter(tcflag == 1)

fs::file_delete("CensusACSTract.xlsx")


## -----------------------------------------------------------------------------------------------------------------------------------------------------
ct_age <- ct %>%
  select(geoid, geoid2, poptotal, f_10_14, f_15_19, m_10_14, m_15_19, ageunder18, age18_39, age40_64, age65up) %>%
  rowwise() %>%
  mutate(
    age_10_19_percent = round(sum(f_10_14, f_15_19, m_10_14, m_15_19) / poptotal, digits = 2) * 100,
    ageunder18_percent = round(ageunder18 / poptotal, digits = 2) * 100,
    age18_39_percent = round(age18_39 / poptotal, digits = 2) * 100,
    age40_64_percent = round(age40_64 / poptotal, digits = 2) * 100,
    age65up_percent = round(age65up / poptotal, digits = 2) * 100
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
ct_race <- ct %>%
  select(geoid, geoid2, poptotal, whitenh, blacknh, asiannh, amindnh, pacificnh, othernh, multracenh) %>%
  mutate(
    whitenh_percent = round(whitenh / poptotal, digits = 2) * 100,
    blacknh_percent = round(blacknh / poptotal, digits = 2) * 100,
    asiannh_percent = round(asiannh / poptotal, digits = 2) * 100,
    amindnh_percent = round(amindnh / poptotal, digits = 2) * 100,
    pacificnh_percent = round(pacificnh / poptotal, digits = 2) * 100,
    othernh_percent = round(othernh / poptotal, digits = 2) * 100,
    multracenh_percent = round(multracenh / poptotal, digits = 2) * 100,
    poc_percent = 1 - whitenh_percent
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
ct_hisp <- ct %>%
  select(geoid, geoid2, poptotal, hisppop, nothisppop) %>%
  mutate(
    hisppop_percent = round(hisppop / poptotal, digits = 2) * 100,
    nothisppop_percent = round(nothisppop / poptotal, digits = 2) * 100
  )


## ----------------------------------------------------------------------------------------------------------------------------------------------------
ct_foreign <- ct %>%
  select(geoid, geoid2, poptotal, usborncit, forborncit, forbornnot) %>%
  mutate(
    usborncit_percent = round(usborncit / poptotal, digits = 2) * 100,
    forborn_percent = round((forborncit + forbornnot) / poptotal, digits = 2) * 100
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
ct_disability <- ct %>%
  select(geoid, geoid2, poptotal, anydis) %>%
  mutate(anydis_percent = round(anydis / poptotal, digits = 2) * 100)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
ct_inc <- ct %>%
  select(geoid, geoid2, poptotal, medianhhi)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
ct_merge <- right_join(ct_foreign, ct_disability) %>%
  right_join(ct_age) %>%
  right_join(ct_hisp) %>%
  right_join(ct_inc) %>%
  right_join(ct_race)


## ----------------------------------------------------------------------------------------------------------------------------------------------------

census_tract_spatial <- tigris::tracts(
  state = "MN",
  class = "sf"
) %>%
  st_transform(4326) %>%
  select(GEOID) %>%
  right_join(ct_merge, by = c("GEOID" = "geoid2"))

census_tract <- census_tract_spatial %>%
  select(
    GEOID,
    "usborncit_percent",
    "forborn_percent",
    "anydis_percent",
    "age_10_19_percent",
    "ageunder18_percent",
    "age18_39_percent",
    "age40_64_percent",
    "age65up_percent",
    "whitenh_percent",
    "blacknh_percent",
    "asiannh_percent",
    "amindnh_percent",
    "pacificnh_percent",
    "othernh_percent",
    "multracenh_percent",
    "hisppop_percent",
    "nothisppop_percent",
    "medianhhi",
    geometry
  )

names(census_tract) <- c(
  "GEOID",
  "Origin, US-born",
  "Origin, foreign-born",
  "Disability, any disability",
  "Age, 10-19",
  "Age, under 18",
  "Age, 18-39",
  "Age, 40-64",
  "Age, 65 and over",
  "Race, White",
  "Race, Black",
  "Race, Asian",
  "Race, American Indian",
  "Race, Pacific Islander",
  "Race, Other",
  "Race, Multiracial",
  "Ethnicity, Hispanic",
  "Ethnicity, Not Hispanic",
  "Income, Median Household Income",
  "geometry"
)

# library(ggplot2)
# ggplot() + geom_sf(data = census_tract)


county_outlines <- tigris::counties(
  state = "MN",
  class = "sf"
) %>%
  dplyr::filter(NAME %in% c(
    "Hennepin",
    "Dakota",
    "Carver",
    "Ramsey",
    "Anoka",
    "Scott",
    "Washington"
  )) %>%
  dplyr::select(NAME) %>%
  sf::st_transform(4326)

usethis::use_data(county_outlines, overwrite = TRUE)


usethis::use_data(census_tract, overwrite = TRUE)
