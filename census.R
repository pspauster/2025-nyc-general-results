library(tidyverse)
library(tidycensus)
library(janitor)

vars <- load_variables(2020, "dhc")

census_election_districts <- get_decennial(
  year = 2020,
  variables = c("total" = "P5_001N",
                "not_hisp" = "P5_002N",
                "hispanic_latino" = "P5_010N"),
  geography = "voting district",
  state = "NY",
  county = c("Kings", "New York", "Bronx", "Queens", "Richmond")
)

census_election_districts <- get_decennial(
  year = 2020,
  variables = c("total" = "P5_001N",
                "not_hisp" = "P5_002N",
                "hispanic_latino" = "P5_010N"),
  geography = "voting district",
  state = "NY",
  county = c("Kings", "New York", "Bronx", "Queens", "Richmond")
)

acs <- load_variables(2022, "acs5")

acs_ads <- get_acs(
  year = 2022,
  variables = c("total" = "B03002_001",
                "not_hisp" = "B03002_002",
                "hispanic_latino" = "B03002_012"),
  geography = "state legislative district (lower chamber)",
  state = "NY",
  #county = c("Kings", "New York", "Bronx", "Queens", "Richmond")
)

census_ads <- get_decennial(
  year = 2020,
  variables = c("total" = "P5_001N",
                "not_hisp" = "P5_002N",
                "hispanic_latino" = "P5_010N"),
  geography = "state legislative district (lower chamber)",
  state = "NY",
  output = "wide"
  #county = c("Kings", "New York", "Bronx", "Queens", "Richmond")
) %>% 
  mutate(ad_num = str_sub(GEOID, start = 3,5),
         hisp_latino_prop = hispanic_latino/total)
  

ads_prez_24 <- read_csv("output/President_Vice President/ad_president_vice_president.csv")

ads_prez_24_clean <- ads_prez_24 %>% 
  clean_names() %>% 
  filter(ad_num != "Total") %>% 
  mutate(
         ad_num = str_pad(str_extract(ad_num, "\\d+"), width = 3, side = "left", pad = "0"),
         ) %>% 
  group_by(candidate, ad_num) %>% 
  summarize(votes = sum(votes)) %>% 
  filter(candidate != "WRITE-IN") %>% 
  pivot_wider(names_from = "candidate", values_from = "votes") %>% 
  mutate(
    total_votes = `Kamala D. Harris Tim Walz` + `Donald J. Trump JD Vance`,
    d_share = `Kamala D. Harris Tim Walz`/total_votes*100,
    r_share =  `Donald J. Trump JD Vance`/total_votes*100)

ads_census <- left_join(ads_prez_24_clean, census_ads, by = "ad_num")

ads_census %>% 
  ggplot()+
  geom_point(mapping = aes(x = hisp_latino_prop, y = r_share))
