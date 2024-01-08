
ward_codes <- mrktc_clean1 %>% distinct(ward.code) %>% pull()

# for each ward code plot the survey gps locations?


# Load Tanzania Shapefile:
tanzania_shp <- st_read(here("shapefiles/Districts and TC as 2020.shp"))

## Check the duplicate market locations

wards_mapdata <- mrktc_clean1 %>% 
  mutate(lat = str_split(`gps.coordinates.of.the.market`, "\\s", simplify = TRUE)[, 1],
         long = str_split(`gps.coordinates.of.the.market`, "\\s", simplify = TRUE)[, 2])

wards_mapdata %>% filter(is.na(lat) | is.na(long)) %>% View()

# for now ignore rows which have na in lat or long:

wards_sf <- st_as_sf(wards_mapdata %>% filter(!is.na(lat), !is.na(long)), coords = c("long","lat"),  crs = 4326)


i <- 1
ward_i_sf <- wards_sf %>% filter(ward.code==ward_codes[i])
ward_i_sf <- wards_sf %>% filter(ward.name=="sibwesa") %>%
  mutate(vilmrkt = paste0(village.1, ".",market.name.1))

                         
ward_i <- ggplot() +
  geom_sf(data = tanzania_shp %>% filter(Region_Nam=="Katavi"), fill = NA) +
  geom_sf(data = ward_i_sf, aes(shape = as.factor(`market.name.1`), col = as.factor(`village.1`)), size = 2) +
  labs(col = "Village Name",
       shape = "Market Name")+
  # scale_color_brewer(palette = "Set3")+
  coord_sf()+
  theme_bw()

ward_i

ward_i <- ggplot() +
  geom_sf(data = tanzania_shp %>% filter(Region_Nam=="Katavi",
                                         District_C == 2), fill = NA) +
  geom_sf(data = ward_i_sf, aes(col = as.factor(`vilmrkt`), shape = as.factor(`vilmrkt`)), alpha = 0.8) +
  # geom_sf(data = ward_i_sf %>% filter(vilmrkt %in% c("477.sibwesa","kabungu.minyagala","minyagala.sibwesa") ),aes(col = as.factor(`vilmrkt`)),col = "black", size = 2) +
  labs(col = "VilMrkt Name",
       shape ="VilMrkt Name")+
  # scale_color_brewer(palette = "Set3")+
  coord_sf()+
  theme_bw()

ward_i



ward_i_sf <- wards_sf %>% filter(district.region.name=="morogoro") %>%
  mutate(vilmrkt = paste0(village.1, ".",market.name.1))


ward_i <- ggplot() +
  geom_sf(data = tanzania_shp %>% filter(Region_Nam=="morogoro"), fill = NA) +
  geom_sf(data = ward_i_sf, aes(col = as.factor(ward.name))) +
  facet_wrap(~ward.name) +
  # geom_sf(data = ward_i_sf %>% filter(vilmrkt %in% c("477.sibwesa","kabungu.minyagala","minyagala.sibwesa") ),aes(col = as.factor(`vilmrkt`)),col = "black", size = 2) +
  labs(col = "Ward Name")+
  # scale_color_brewer(palette = "Set3")+
  coord_sf()+
  theme_bw()

ward_i


ward_i_sf <- wards_sf %>% filter(market.name.1=="itete") %>%
  mutate(vilmrkt = paste0(ward.name, ".", village.1, ".",market.name.1))


ward_i <- ggplot() +
  geom_sf(data = tanzania_shp %>% filter(), fill = NA) +
  geom_sf(data = ward_i_sf, aes(col = as.factor(vilmrkt),
                                shape = district.region.name)) +
  facet_wrap(~ward.name) +
  # geom_sf(data = ward_i_sf %>% filter(vilmrkt %in% c("477.sibwesa","kabungu.minyagala","minyagala.sibwesa") ),aes(col = as.factor(`vilmrkt`)),col = "black", size = 2) +
  labs(col = "vilmrkt",
       shape = "reg")+
  # scale_color_brewer(palette = "Set3")+
  coord_sf()+
  theme_bw()

ward_i
  
