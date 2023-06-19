## Script for cleaning village and market names data for market survey C data
# can run this script and check market locations using maps at hte bottom, this can be used to update the lookup & correct where needed

library(readr)
library(tidyverse)
library(here)
library(knitr)


# load tanzania data for market c general info and activities surveys
mrktc_generalinfo <- read_csv(here("tanzania_data/mrktc_generalinfo_tanzania.csv"))
# >>> Resave this as CLEANED csv


## LOOKUPS ##
mrkta_lkpregion <- read_csv(here("ecopprmarketscsvs/mrkta_lkpregion.csv"))
mrkta_lkpward <- read_csv(here("ecopprmarketscsvs/mrkta_lkpward.csv"))

mrktc_lkpvilmrkt <- read_csv("scripts/clean_data_raw/mrktc_vilmrktname_lookup.csv") %>%
  select(village, 
         village.corrected = village.crossref,
         market.name, 
         market.name.corrected = market.crossref) %>% 
  distinct()

# mrktc_lkpvilname <- read_csv("scripts/clean_data_raw/mrktc_vilmrktname_lookup.csv") %>%
#   select(village, village.corrected = village.crossref) %>% distinct()
# mrktc_lkpmrktname <- read_csv("scripts/clean_data_raw/mrktc_vilmrktname_lookup.csv") %>%
#   select(market.name, market.name.corrected = market.crossref) %>% distinct()

## CLEAN 1: select relevant variables & clean market & village names.

mrktc_clean <- mrktc_generalinfo %>%
  select(
    fid = `fid.see.mrktc.idtable.`,
    country = `country.see.mrkta.lkpcountry.`,
    district.region.code = `district.region.see.mrkta.lkpregion.`,
    ward.code = `ward.see.mrkta.lkpward.`,
    village,
    market.name = `name.of.market`,
    market.type = `type.of.market.see.mrktc.lkpmrkt.type.`,
    `respondent.role`,
    other.market.visited= `13.have.you.visited.other.markets.to.sell.or.buy.sheep.and.goats.in.the.past.one.month.`,
    number.markets.visited =`number.of.other.markets.you.have.visited.to.sell.or.buy.sheep.and.goats.in.the.past.one.month`,
    trader = `researcher.is.this.person.a.trader.meaning.they.buy.and.sell.animals.as.a.business.`,
    trade.pattern = `14.what.is.your.usual.pattern.of.trading.sheep.and.or.goats.`,
    `gps.coordinates.of.the.market`,
    `unique.row.identifier.uuid.`
  ) %>%
  left_join(mrkta_lkpward %>% select(ward.code = "Code", ward.name = "Description"),
            by = c("ward.code")) %>%
  left_join(mrkta_lkpregion %>% select(Code,"district.region.name" = "Description"),
            by = c(`district.region.code` = "Code")
  ) %>%
  # clean market & village names:
  mutate(
    "market.name" = tolower(`market.name`), # lowercase
    "market.name" = gsub("[[:punct:]]", " ", `market.name`), # remove punctuation
    "market.name" = trimws(`market.name`), #trim leading and trailing spaces
    "market.name" = gsub("\\s+", ".", `market.name`)# replace 1+ spaces with "."
  )  %>% 
  mutate(
    "village" = tolower(`village`), # lowercase
    "village" = gsub("[[:punct:]]", " ", `village`), # remove punctuation
    "village" = trimws(`village`),
    "village" = gsub("\\s+", ".", `village`)
  ) 
  

mrktc_clean2 <- mrktc_clean %>% left_join(mrktc_lkpvilmrkt, by = c("village", "market.name"))
  


# Check that markets with the same name are plotted in the same places...


## MAPPING MARKETS ##

# Load Tanzania Shapefile:
library(sf)
tanzania_shp <- st_read(here("shapefiles/Districts and TC as 2020.shp"))

# Convert mrkt_a to sf for mapping

# GPS coordinates of market are in degrees decimal minutes - DDM (i think)

mrktc_map <- mrktc_clean2 %>%
  select(fid,
         country,
         district.region.code,
         district.region.name,
         ward.code,
         ward.name,
         village.corrected,
         market.name.corrected,
         gps.coordinates.of.the.market,                                 
         unique.row.identifier.uuid.
  ) %>% 
  mutate(lat = str_split(`gps.coordinates.of.the.market`, "\\s", simplify = TRUE)[, 1],
         long = str_split(`gps.coordinates.of.the.market`, "\\s", simplify = TRUE)[, 2])

mrktc_map %>% filter(is.na(lat) | is.na(long)) %>% distinct(village.corrected,market.name.corrected)
# all of the markets in this list do have gps coordinates for other entries - will need to create a market gps lookup to fill in missing values!
#for checking market & village name cleaning just remove the NAs for now

mrktc_map <- mrktc_map %>% na.omit(lat,long)
mrktc_sf <- st_as_sf(mrktc_map, coords = c("long","lat"),  crs = 4326)

mrktc_list <- mrktc_sf %>% distinct(market.name.corrected) %>% arrange(market.name.corrected)

# basic plot with "bahi" as example
ggplot() +
  geom_sf(data = tanzania_shp) +
  geom_sf(data = mrktc_sf %>%
            filter(market.name.corrected== "bahi"), aes(color = `market.name.corrected`)) +
  # scale_color_brewer(palette = "Set3")+
  coord_sf()+
  theme_bw()

seq <- 1:10
seq2 <- 11:20
seq3 <- 21:30
seq4 <- 31:40
seq5 <- 41:50
seq6 <- 51:60
seq7 <- 61:70
seq8 <- 71:80
seq9 <- 81:93


mrkts <- mrktc_list[seq9,] %>% pull()
mrktc_sf %>%
  filter(market.name.corrected %in% mrkts) %>%
  View()

ggplot() +
  geom_sf(data = tanzania_shp) +
  geom_sf(data = mrktc_sf %>%
            filter(market.name.corrected %in% mrkts), aes(color = `market.name.corrected`)) +
  # scale_color_brewer(palette = "Set3")+
  coord_sf()+
  theme_bw()




