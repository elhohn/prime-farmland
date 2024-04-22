library(aqp)
library(soilDB)
#library(terra)
library(stringr)
library(sf)
library(geojsonsf)
library(tidyverse)

sf_use_s2(FALSE)

## Puerto Rico and US Virgin Islands
pr_usvi <- geojson_sf("mukeys/pr_usvi.geojson") %>%
  mutate(location = 'Puerto Rico and USVI')
pr_usvi_mukeys <- pr_usvi$MUKEY

mukeys = paste0(str_pad(pr_usvi_mukeys, width = 4, side = "left"))
mukeys_string = toString(sQuote(mukeys, q = FALSE))

qry = "SELECT mukey, muname, farmlndcl FROM mapunit WHERE mukey in (%s);"
qry_all = sprintf(qry, mukeys_string)

res <- SDA_query(qry_all)

df <- pr_usvi %>%
  merge(res, by.x = "MUKEY", by.y = "mukey", all = TRUE) %>%
  mutate(area_meters = st_area(df),
         area_acres = area_meters * 0.000247105)

df$area_acres <- units::set_units(st_area(df), "acre")

df %>%
  st_drop_geometry() %>%
  group_by(farmlndcl) %>%
  summarise(total_acres = sum(area_acres))


## Northern Mariana Islands
nmi <- geojson_sf("mukeys/nmi.geojson") %>%
  mutate(location = 'Northern Mariana Islands')
nmi_mukeys <- nmi$MUKEY

mukeys = paste0(str_pad(nmi_mukeys, width = 4, side = "left"))
mukeys_string = toString(sQuote(mukeys, q = FALSE))

qry = "SELECT mukey, muname, farmlndcl FROM mapunit WHERE mukey in (%s);"
qry_all = sprintf(qry, mukeys_string)

res <- SDA_query(qry_all)

df <- nmi %>%
  merge(res, by.x = "MUKEY", by.y = "mukey", all = TRUE) %>%
  mutate(area_meters = st_area(df),
         area_acres = area_meters * 0.000247105)

df$area_acres <- units::set_units(st_area(df), "acre")

df %>%
  st_drop_geometry() %>%
  group_by(farmlndcl) %>%
  summarise(total_acres = sum(area_acres))


## Hawaii
hi <- geojson_sf("mukeys/hi.geojson") %>%
  mutate(location = 'Hawaii')
hi_mukeys <- hi$MUKEY

mukeys = paste0(str_pad(hi_mukeys, width = 4, side = "left"))
mukeys_string = toString(sQuote(mukeys, q = FALSE))

qry = "SELECT mukey, muname, farmlndcl FROM mapunit WHERE mukey in (%s);"
qry_all = sprintf(qry, mukeys_string)

res <- SDA_query(qry_all)

df <- hi %>%
  merge(res, by.x = "MUKEY", by.y = "mukey", all = TRUE) %>%
  mutate(area_meters = st_area(hi),
         area_acres = area_meters * 0.000247105)

df$area_acres <- units::set_units(st_area(df), "acre")

df %>%
  st_drop_geometry() %>%
  group_by(farmlndcl) %>%
  summarise(total_acres = sum(area_acres))


## American Samoa
as <- geojson_sf("mukeys/as.geojson") %>%
  mutate(location = 'American Samoa')
as_mukeys <- as$MUKEY

mukeys = paste0(str_pad(as_mukeys, width = 4, side = "left"))
mukeys_string = toString(sQuote(mukeys, q = FALSE))

qry = "SELECT mukey, muname, farmlndcl FROM mapunit WHERE mukey in (%s);"
qry_all = sprintf(qry, mukeys_string)

res <- SDA_query(qry_all)

df <- as %>%
  merge(res, by.x = "MUKEY", by.y = "mukey", all = TRUE) %>%
  mutate(area_meters = st_area(as),
         area_acres = area_meters * 0.000247105)

df$area_acres <- units::set_units(st_area(df), "acre")

df %>%
  st_drop_geometry() %>%
  group_by(farmlndcl) %>%
  summarise(total_acres = sum(area_acres))


## Guam
guam <- geojson_sf("mukeys/guam.geojson") %>%
  mutate(location = 'Guam')
guam_mukeys <- guam$MUKEY

mukeys = paste0(str_pad(guam_mukeys, width = 4, side = "left"))
mukeys_string = toString(sQuote(mukeys, q = FALSE))

qry = "SELECT mukey, muname, farmlndcl FROM mapunit WHERE mukey in (%s);"
qry_all = sprintf(qry, mukeys_string)

res <- SDA_query(qry_all)

df <- guam %>%
  merge(res, by.x = "MUKEY", by.y = "mukey", all = TRUE) %>%
  mutate(area_meters = st_area(guam),
         area_acres = area_meters * 0.000247105)

df$area_acres <- units::set_units(st_area(df), "acre")

df %>%
  st_drop_geometry() %>%
  group_by(farmlndcl) %>%
  summarise(total_acres = sum(area_acres))


## Alaska
ak <- geojson_sf("mukeys/ak.geojson") %>%
  mutate(location = 'Alaska')
ak_mukeys <- ak$MUKEY

mukeys = paste0(str_pad(ak_mukeys, width = 4, side = "left"))
mukeys_string = toString(sQuote(mukeys, q = FALSE))

qry = "SELECT mukey, muname, farmlndcl FROM mapunit WHERE mukey in (%s);"
qry_all = sprintf(qry, mukeys_string)

res <- SDA_query(qry_all)

df <- ak %>%
  merge(res, by.x = "MUKEY", by.y = "mukey", all = TRUE) %>%
  mutate(area_meters = st_area(ak),
         area_acres = area_meters * 0.000247105)

df$area_acres <- units::set_units(st_area(df), "acre")

df %>%
  st_drop_geometry() %>%
  group_by(farmlndcl) %>%
  summarise(total_acres = sum(area_acres))


