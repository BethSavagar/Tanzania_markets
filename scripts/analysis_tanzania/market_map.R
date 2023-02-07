# Tanzania market survey A - preliminary analysis (02.02.22)

library(readr)
library(tidyverse)
library(sf)
library(ggplot2)

# read in tanzania market data
mrkta_1_tanzania <- read_csv("tanzania_data/mrkta_generalinfo_tanzania.csv")

# LOOKUPS ##
mrkta_lkpregion <- read_csv("~/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/PPR Collaborations/Data Bank/ecoppr_data/data_raw/ecopprmarketscsvs/mrkta_lkpregion.csv")

# Is the market name variable unique, does it correspond to market id variable?
mrkta_1_tanzania %>% distinct(fid.see.mrkta.idtable.) # every entry has unique id
mrkta_1_tanzania %>% distinct(name.of.market) # 1 entry is a duplicate: Dutwa
mrkta_1_tanzania %>% group_by(name.of.market) %>% count() %>% view() # identify duplicate market name
dutwa_mrkt_duplicate <- mrkta_1_tanzania %>% filter(name.of.market == "Dutwa") # check differences between duplicate entries

# Are market name and village the same? 
mrkta_1_tanzania %>% 
  select(village, name.of.market) %>%
  filter(mrkta_1_tanzania$village != mrkta_1_tanzania$name.of.market)
# market name and village are sometimes different, sometimes typos

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

# Descriptive analysis and maps:

# markets by TYPE: 
mrkta_1_tanzania %>% 
  group_by(type.of.market.see.mrkta.lkpmrkt.type.) %>% 
  count() # all markets are type 1

# markets by REGION (colour coordinated)

mrkta_sf <- mrkta_sf %>%
  left_join(
  mrkta_lkpregion %>%
    select(Code,"region.name" = "Description"),
  by = c(`district.region.see.mrkta.lkpregion.` = "Code")
  )

ggplot() +
  geom_sf(data = tanzania_shp) +
  geom_sf(data = mrkta_sf, aes(color = as.factor(`region.name`))) +
  labs(col = "region")+
  # scale_color_brewer(palette = "Set3")+
  coord_sf()+
  theme_bw()

# markets by WARD:
mrkta_1_tanzania %>% 
  group_by(ward.see.mrkta.lkpward.) %>% 
  count() %>% filter(n>1) # most wards are unique to 1 market, 3 wards have more than 1 market.




