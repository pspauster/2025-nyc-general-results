library(tidyverse)
library(sf)
library(janitor)

ads <- read_sf("https://data.cityofnewyork.us/resource/qbgu-kv2h.geojson?$limit=10000") %>% 
  mutate(ad_num = str_pad(assem_dist, width = 3, side = "left", pad = "0"))

prez_24_ad <- read_csv("output/President_Vice President/ad_president_vice_president.csv")

twenty20 <- read_csv("https://vote.nyc/sites/default/files/pdf/election_results/2020/20201103General%20Election/00000100000Citywide%20President%20Vice%20President%20Citywide%20EDLevel.csv",
                     col_names = F)

twenty20_clean_ad <- twenty20 %>% 
  mutate(ad_code = str_pad(as.character(`X12`), width = 3, side = "left", pad = "0")) %>% 
  filter(str_detect(`X21`, "Joseph R. Biden|Donald J. Trump")) %>% 
  separate(`X21`, sep = " \\(", into = c("candidate", "party")) %>% 
  group_by(candidate, ad_code) %>% 
  summarize(votes20 = sum(`X22`, na.rm=T)) %>% 
  pivot_wider(names_from = "candidate", values_from = "votes20") %>% 
  mutate(
    total = `Joseph R. Biden / Kamala D. Harris` + `Donald J. Trump / Michael R. Pence`,
    d_share = `Joseph R. Biden / Kamala D. Harris`/total*100,
    r_share =  `Donald J. Trump / Michael R. Pence`/total*100)

prez_24_clean_ad <- prez_24_ad %>% 
  clean_names() %>% 
  filter(ad_num != "Total") %>% 
  mutate(
         ad_num = str_pad(str_extract(ad_num, "\\d+"), width = 3, side = "left", pad = "0")
         ) %>% 
  group_by(candidate, ad_num) %>% 
  summarize(votes = sum(votes)) %>% 
  filter(candidate != "WRITE-IN") %>% 
  pivot_wider(names_from = "candidate", values_from = "votes") %>% 
  mutate(
    total = `Kamala D. Harris Tim Walz` + `Donald J. Trump JD Vance`,
    d_share = `Kamala D. Harris Tim Walz`/total*100,
    r_share =  `Donald J. Trump JD Vance`/total*100)

swing_ad <- left_join(prez_24_clean_ad, twenty20_clean_ad, by = c("ad_num"="ad_code"), suffix = c("", "_20")) %>% 
  mutate(two_party_swing_r = (r_share-r_share_20),
         swing_dir = if_else(two_party_swing_r >=0, "red", "blue"))
                     
ads_results <- left_join(ads, swing_ad, by = c("ad_num"))

swing_nyc_map_data_ad <- ads_results %>% 
  st_centroid() %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  filter(total >= 100, total_20 >100)

ggplot(ads_results)+
  geom_sf(mapping = aes(fill = two_party_swing_r), color = NA)+
  scale_fill_gradient2(low = "blue", mid = "beige", high = "red", name = "Swing to Trump\n(percentage point)")+
  labs(caption = "Note: Percentage point change in two party vote share 2020-2024")+
  theme_void()

write_csv(swing_nyc_map_data_ad, "analysis/swing_map_ad.csv")

write_sf(ads, "ads_sf.geojson", append = F)

ballot1_24_ad <- read_csv("output/Proposal Number 1/ad_proposal_number_1.csv")

ballot1clean <- ballot1_24_ad %>% 
  clean_names() %>% 
  filter(ad_num != "Total") %>% 
  mutate(
    ad_num = str_pad(str_extract(ad_num, "\\d+"), width = 3, side = "left", pad = "0")
  ) %>% 
  group_by(candidate, ad_num) %>% 
  summarize(votes = sum(votes)) %>% 
  filter(candidate != "WRITE-IN") %>% 
  pivot_wider(names_from = "candidate", values_from = "votes") %>% 
  mutate(
    total = `YES` + `NO`,
    y_share = `YES`/total*100,
    n_share =  `NO`/total*100)

b1_results <- left_join(ads, ballot1clean, by = c("ad_num"))

ggplot(b1_results)+
  geom_sf(mapping = aes(fill = y_share), color = NA)+
  scale_fill_gradient2(low = "red", mid = "beige", high = "blue", midpoint = 50, name = "Vote Share for\nEqual Rights Amendment")+
  theme_void()
