library(sf)
library(here)
library(tidyverse)
library(RColorBrewer)

## Plot all markets from mrktA and mrktC to id consistency between data
# mrktc_clean3 = all mrktc data with cleaned names variablees post sweeps 1-5 

mrktc_clean3 <- read_csv(file = "tanzania_data/mrktc_info_clean3-1.csv")
mrktc_names_clean3 <- read_csv(file = "tanzania_data/mrktc_names_clean3-1.csv")
# Load Tanzania Shapefile:
tanzania_shp <- st_read(here("shapefiles/Districts and TC as 2020.shp"))
mrkta_gps <- read_csv(file = "tanzania_data/mrkta_gps_tanzania_CLEAN.csv")
# mrktc_gps <- read_csv(file = "tanzania_data/mrktc_gps_CLEAN.csv")

#############
## MAPDATA ##
#############

## MRKTA mapping: ##

mrkta_mapdata <- mrkta_gps %>% 
  mutate(lat = str_split(`gps.coordinates.of.the.market`, "\\s", simplify = TRUE)[, 1],
         long = str_split(`gps.coordinates.of.the.market`, "\\s", simplify = TRUE)[, 2])

# for now ignore rows which have na in lat or long:
mrkta_sf <- st_as_sf(mrkta_mapdata %>% filter(!is.na(lat), !is.na(long)), coords = c("long","lat"),  crs = 4326)


## MRKTC mapping: ##

mrktc_tidy <- mrktc_clean3 %>%
  select(
    -c(ends_with("1"), ends_with(".2"), corrected.3A, map_check.3A)
  )

mrktc_gps <- mrktc_tidy %>%
  select(
    country,
    district.region.code,
    district.region.name,
    "ward.code.3A",
    "ward.name.3A",
    "village.3A",
    "market.name.3A",
    gps.coordinates.of.the.market
  )
# write.csv(mrktc_gps, file = "tanzania_data/mrktc_gps_CLEAN.csv")

# MRKTC mapping:

mrktc_mapdata <- mrktc_gps %>% 
  mutate(lat = str_split(`gps.coordinates.of.the.market`, "\\s", simplify = TRUE)[, 1],
         long = str_split(`gps.coordinates.of.the.market`, "\\s", simplify = TRUE)[, 2]) %>% arrange(gps.coordinates.of.the.market)

# for now ignore rows which have na in lat or long:
mrktc_sf <- st_as_sf(mrktc_mapdata %>% filter(!is.na(lat), !is.na(long)), coords = c("long","lat"),  crs = 4326) 


# -----------------------------------------------------------------------------------------------------------------

###########
## MAPS ##
###########

# TANZANIA DATA
# Plot map of tanzania:

# ggrepel::geom_label_repel() for sf
tanzania_map <- ggplot() +
  geom_sf(data = tanzania_shp, aes(fill = Region_Nam)) +
  # geom_sf(data = tanzania_shp, aes(fill = Region_Nam, col = factor(District_C))) +
  # geom_sf_text(data = tanzania_shp, aes(label = NewDist20), size = 2)+
  scale_fill_manual(values=rep(brewer.pal(9,"Pastel1"),times=4))+
  scale_color_discrete(guide = "none")+
  labs(title = "Tanzania map of Region (colour) and District (boundary) ")+
  theme_bw()

tanzania_map

# ggrepel::geom_label_repel() for sf
tanzania_map_labelled <- ggplot() +
  geom_sf(data = tanzania_shp, aes(fill = Region_Nam)) +
  ggrepel::geom_text_repel(
    data = tanzania_shp,
    aes(label = NewDist20, geometry = geometry),
    size = 2,
    stat = "sf_coordinates",
    min.segment.length = 0
  )+
  # geom_sf_text(data = tanzania_shp, aes(label = NewDist20), size = 2)+
  scale_fill_manual(values=rep(brewer.pal(9,"Pastel1"),times=4))+
  theme_bw()

tanzania_map_labelled


# MARKET A DATA
# Plot location of all markets, highlighting regions of interest

# mrktA_map <- ggplot() +
#   geom_sf(data = tanzania_shp, fill = NA) +
#   geom_sf(data = mrkta_sf, aes(col = district.region.name), size = 1)+
#   scale_colour_brewer(palette = "Paired")+
#   coord_sf()+
#   theme_bw()


# Create a custom color palette with 8 colors (5 Dark2 + 3 additional)
my_palette <- c(brewer.pal(8, "Dark2"), "#FFA500", "#6A3D9A", "#33A02C")


mrktA_map <- ggplot() +
  geom_sf(data = tanzania_shp, aes(fill = Region_Nam), linewidth = 0.1) +
 #scale_fill_brewer(palette = "Pastel1")+
  scale_fill_manual(values=alpha(rep(brewer.pal(9,"Pastel1"),times=4),0.7))+
  geom_sf(data = mrkta_sf, aes(col = district.region.name), size = 1.5)+
  #scale_colour_brewer(palette = "Paired")+
  scale_colour_manual(values = my_palette)+
  coord_sf()+
  labs(title = "Market Survey A - market locations")+
  theme_bw()

mrktA_map


mrktC_map <- ggplot() +
  geom_sf(data = tanzania_shp, aes(fill = Region_Nam), linewidth = 0.1) +
  #scale_fill_brewer(palette = "Pastel1")+
  scale_fill_manual(values=alpha(rep(brewer.pal(9,"Pastel1"),times=4),0.7))+
  geom_sf(data = mrktc_sf, aes(col = district.region.name), size = 1)+
  #scale_colour_brewer(palette = "Paired")+
  scale_colour_manual(values = my_palette)+
  coord_sf()+
  labs(title = "Market Survey C - survey locations")+
  theme_bw()

mrktC_map


## District plots:
# Number of markets per district: 

districts <- mrktc_sf %>% distinct(district.region.name) %>% pull()
Districts <- str_to_title(districts)

# Create a list of map plots using a loop
map_plots <- list()
for (i in 1:length(districts)) {
  
  District_i <- Districts[i]
  district_i <- districts[i]
  
  map_plots[[i]] <- ggplot() +
    geom_sf(data = tanzania_shp %>% filter(Region_Nam == District_i), fill = NA) +
    geom_sf(data = mrkta_sf %>% filter(district.region.name == district_i), col = "black", alpha = 0.5, size = 3)+
    geom_sf(data = mrktc_sf %>% filter(district.region.name == district_i), col = "red", shape = 17, size = 1)+
    scale_colour_brewer(palette = "Paired")+
    facet_wrap(~district.region.name)+
    coord_sf()+
    theme_bw()
  
}

# Plot the maps
gridExtra::grid.arrange(grobs = map_plots, ncol = 4) 



## SPOT CHECK MISSING C surveys
 ggplot() +
    geom_sf(data = tanzania_shp %>% filter(Region_Nam %in% c("Iringa","Mbeya","Njombe")), 
            aes(fill = Region_Nam),
            linewidth = 0.1) +
   #scale_fill_brewer(palette = "Pastel1")+
   scale_fill_manual(values=alpha(rep(brewer.pal(9,"Pastel1"),times=4),0.7))+
    geom_sf(data = mrkta_sf %>% filter(district.region.name %in% c("iringa","mbeya","njombe")), col = "black", alpha = 0.5, size = 3)+
    geom_sf(data = mrktc_sf %>% filter(district.region.name %in% c("iringa","mbeya","njombe")), aes(col = district.region.name),  shape = 17, size = 1.5)+
   #scale_colour_brewer(palette = "Paired")+
   scale_colour_manual(values = my_palette)+
    coord_sf()+
    theme_bw()

 
 
 ggplot() +
   geom_sf(data = tanzania_shp, aes(fill = Region_Nam), linewidth = 0.1) +
   #scale_fill_brewer(palette = "Pastel1")+
   scale_fill_manual(values=alpha(rep(brewer.pal(9,"Pastel1"),times=4),0.7))+
   geom_sf(data = mrkta_sf, aes(col = district.region.name), alpha = 0.5, size = 3)+
   #scale_colour_brewer(palette = "Paired")+
   geom_sf(data = mrktc_sf, aes(col = district.region.name),  shape = 17, size = 1.5)+
   scale_colour_manual(values = my_palette)+
   coord_sf()+
   labs(title = "Market Survey A (circles) and C (triangles) - market locations")+
   theme_bw()
 

