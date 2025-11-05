library(tidyverse)
library(sf)
library(janitor)

eds <- read_sf("https://data.cityofnewyork.us/resource/wwxk-38u4.geojson?$limit=10000") %>% 
  mutate(ed_code = str_pad(elect_dist, 6, "left", "0"))

prez_24 <- read_csv("output/President_Vice President/ed_president_vice_president.csv")

twenty20 <- read_csv("https://vote.nyc/sites/default/files/pdf/election_results/2020/20201103General%20Election/00000100000Citywide%20President%20Vice%20President%20Citywide%20EDLevel.csv",
                     col_names = F)

twenty20_clean <- twenty20 %>% 
  mutate(ed_code = str_pad(paste0(as.character(`X12`),`X13`), 6, "left", "0")) %>% 
  filter(str_detect(`X21`, "Joseph R. Biden|Donald J. Trump")) %>% 
  separate(`X21`, sep = " \\(", into = c("candidate", "party")) %>% 
  group_by(candidate, ed_code) %>% 
  summarize(votes20 = sum(`X22`, na.rm=T)) %>% 
  pivot_wider(names_from = "candidate", values_from = "votes20") %>% 
  mutate(
    total = `Joseph R. Biden / Kamala D. Harris` + `Donald J. Trump / Michael R. Pence`,
    d_share = `Joseph R. Biden / Kamala D. Harris`/total*100,
    r_share =  `Donald J. Trump / Michael R. Pence`/total*100)

prez_24_clean <- prez_24 %>% 
  clean_names() %>% 
  filter(ed_num != "Total") %>% 
  mutate(ed_num = str_pad(str_extract(ed_num, "\\d+"), width = 3, side = "left", pad = "0"),
         ad_num = str_pad(str_extract(ad_num, "\\d+"), width = 3, side = "left", pad = "0"),
         ed_code = paste0(ad_num, ed_num)) %>% 
  group_by(ed_code, candidate, ed_num, ad_num) %>% 
  summarize(votes = sum(votes, na.rm = T)) %>% 
  filter(candidate != "WRITE-IN") %>% 
  pivot_wider(names_from = "candidate", values_from = "votes") %>% 
  mutate(
    total = `Kamala D. Harris Tim Walz` + `Donald J. Trump JD Vance`,
    d_share = `Kamala D. Harris Tim Walz`/total*100,
    r_share =  `Donald J. Trump JD Vance`/total*100)

swing <- left_join(prez_24_clean, twenty20_clean, by = "ed_code", suffix = c("", "_20")) %>% 
  mutate(two_party_swing_r = (r_share-r_share_20),
         swing_dir = if_else(two_party_swing_r >=0, "red", "blue"))
                     
eds_results <- left_join(eds, swing, by = c("ed_code"))

swing_nyc_map_data <- eds_results %>% 
  st_centroid() %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  filter(total >= 100, total_20 >100)

write_csv(swing_nyc_map_data, "analysis/swing_map.csv")

ggplot(eds_results)+
  geom_sf(mapping = aes(fill = two_party_swing_r), color = NA)+
  scale_fill_gradient2(low = "blue", mid = "beige", high = "red", name = "Swing to Trump\n(percentage point)", na.value = "lightgray")+
  labs(caption = "Note: Percentage point change in two party vote share 2020-2024")+
  theme_void()


