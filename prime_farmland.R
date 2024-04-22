library(aqp)
library(soilDB)
#library(terra)
library(stringr)
library(sf)
library(geojsonsf)
library(tidyverse)

pr_usvi <- geojson_sf("mukeys/pr_usvi.geojson")
pr_usvi_mukeys <- pr_usvi$MUKEY

mukeys = paste0(str_pad(pr_usvi_mukeys, width = 4, side = "left"))
mukeys_string = toString(sQuote(sites, q = FALSE))

qry = "SELECT mukey, muname, farmlndcl FROM mapunit WHERE mukey in (%s);"
qry_all = sprintf(qry, mukeys_string)

res <- SDA_query(qry_all)

final <- pr_usvi %>%
  merge(res, by.x = "MUKEY", by.y = "mukey", all = TRUE)

ggplot(final) + geom_sf(aes(fill = farmlndcl)) + guides(fill = guide_none())
