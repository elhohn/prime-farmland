## Author: Elliot Hohn
## Date: April 22, 2024

library(soilDB)
library(stringr)
library(sf)
library(geojsonsf)
library(tidyverse)

sf_use_s2(FALSE)

# The first step of this process was to go to gSSURGO ("gridded SSURGO") and
# download the spatial data for each of the states or terratories for this 
# analysis (Puerto Rico, US Virgin Islands, Northern Mariana Islands, American
# Samoa, Alaska, Hawaii, and Guam). The downloads are geodatabases, and I loaded
# these indvidually into QGIS and exported just the MUPOLYGONS data as a geojson
# into the mukeys folder of this repo. I didn't push these datasets up to GitHub
# because they are too big, and it's easy to recreate locally if someone wants to.

# function for generating output table
calculate_prime_farmland <- function(mukeys_sf, location) {
  mukeys <- unique(mukeys_sf$MUKEY)
  mukeys = paste0(str_pad(mukeys, width = 4, side = "left"))
  mukeys_string = toString(sQuote(mukeys, q = FALSE))
  
  qry = "SELECT mukey, muname, farmlndcl FROM mapunit WHERE mukey in (%s);"
  qry_all = sprintf(qry, mukeys_string)
  
  res <- SDA_query(qry_all)
  
  df <- mukeys_sf %>%
    merge(res, by.x = "MUKEY", by.y = "mukey", all = TRUE)
    
  df <- df %>%
    mutate(area_meters = st_area(df),
           area_acres = area_meters * 0.000247105)
  
  df$area_acres <- units::set_units(st_area(df), "acre")
  
  results <- df %>%
    st_drop_geometry() %>%
    group_by(farmlndcl) %>%
    summarise(total_acres = sum(area_acres))
  
  write_csv(results, paste0('results/', location, '.csv'))
  
  print(results)
}


## Puerto Rico and US Virgin Islands
pr_usvi <- geojson_sf("mukeys/pr_usvi.geojson") %>%
  mutate(location = 'Puerto Rico and USVI')
calculate_prime_farmland(pr_usvi, 'pr_usvi')

## Northern Mariana Islands
nmi <- geojson_sf("mukeys/nmi.geojson") %>%
  mutate(location = 'Northern Mariana Islands')
calculate_prime_farmland(nmi, 'nmi')

## Hawaii
hi <- geojson_sf("mukeys/hi.geojson") %>%
  mutate(location = 'Hawaii')
calculate_prime_farmland(hi, 'hi')

## American Samoa
as <- geojson_sf("mukeys/as.geojson") %>%
  mutate(location = 'American Samoa')
calculate_prime_farmland(as, 'as')

## Guam
guam <- geojson_sf("mukeys/guam.geojson") %>%
  mutate(location = 'Guam')
calculate_prime_farmland(guam, 'guam')

## Alaska
ak <- geojson_sf("mukeys/ak.geojson") %>%
  mutate(location = 'Alaska')
calculate_prime_farmland(ak, 'ak')

