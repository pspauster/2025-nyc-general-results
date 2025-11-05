library(tidyverse)
library(sf)

eds <- read_sf("https://data.cityofnewyork.us/resource/wwxk-38u4.geojson?$limit=100000") %>% 
  st_centroid()

city_council <- read_sf("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_City_Council_Districts_Water_Included/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson")

intersect <- st_intersection(city_council, eds)

crosswalk <- intersect %>% 
  as.data.frame() %>% 
  select(CounDist, elect_dist)

write_csv(crosswalk, "analysis/crosswalk_coundist_electdist.csv")



