# Tanzania market survey A - preliminary analysis (02.02.22)

library(readr)
library(tidyverse)
library(sf)
library(ggplot2)

# read in tanzania market data
mrkta_1_tanzania <- read_csv("tanzania_data/mrkta_generalinfo_tanzania.csv")

# Is the market name variable unique, does it correspond to market id variable?
mrkta_1_tanzania %>% distinct(fid.see.mrkta.idtable.) # every entry has unique id
mrkta_1_tanzania %>% distinct(name.of.market) # 1 entry is a duplicate: Dutwa
mrkta_1_tanzania %>% group_by(name.of.market) %>% count() %>% view() # identify duplicate market name
dutwa_mrkt_duplicate <- mrkta_1_tanzania %>% filter(name.of.market == "Dutwa") # check differences between duplicate entries


## MAPPING MARKETS ##

# tanzania shape file
tanzania_shp <- st_read("shapefiles/Districts and TC as 2020.shp")

# plot tanzania districts
ggplot()+
  geom_sf(data=tanzania_shp)+
  coord_sf()+
  theme_bw()

# Convert mrkt_a to sf for mapping

# GPS coordinates of market are in degrees decimal minutes - DDM (i think)

mrkta_map <- mrkta_1_tanzania %>%
  select(fid.see.mrkta.idtable.,
         country.see.mrkta.lkpcountry.,
         district.region.see.mrkta.lkpregion.,
         ward.see.mrkta.lkpward.,
         village,
         name.of.market,
         type.of.market.see.mrkta.lkpmrkt.type.,
         gps.coordinates.of.the.market,                                 
         unique.row.identifier.uuid.
         ) %>% 
  mutate(lat = str_split(`gps.coordinates.of.the.market`, "\\s", simplify = TRUE)[, 1],
         long = str_split(`gps.coordinates.of.the.market`, "\\s", simplify = TRUE)[, 2])

mrkta_sf <- st_as_sf(mrkta_map, coords = c("long","lat"),  crs = 4326)

ggplot() +
  geom_sf(data = tanzania_shp) +
  geom_sf(data = mrkta_sf) +
  coord_sf()+
  theme_bw()

# all of 1 market type
ggplot() +
  geom_sf(data = tanzania_shp) +
  geom_sf(data = mrkta_sf, aes(color = `type.of.market.see.mrkta.lkpmrkt.type.`)) +
  coord_sf()+
  theme_bw()

# market == village in most cases
ggplot() +
  geom_sf(data = tanzania_shp) +
  geom_point(data = mrkta_map, aes(x = long, y = lat), fill = village) +
  coord_sf()+
  theme_bw()

# market coloured by district
ggplot() +
  geom_sf(data = tanzania_shp) +
  geom_point(data = mrkta_map, aes(x = long, y = lat), fill = `district.region.see.mrkta.lkpregion.`) +
  #geom_sf(data = mrkta_sf, aes(fill = `district.region.see.mrkta.lkpregion.`)) +
  coord_sf()+
  theme_bw()
