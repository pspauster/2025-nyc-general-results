library(tidyverse)

output_dir <- file.path("output")

proposal_folders <- list.dirs(output_dir, recursive = FALSE, full.names = TRUE) %>%
  tibble(path = .) %>%
  mutate(folder = basename(path)) %>%
  filter(startsWith(folder, "Proposal Number")) %>%
  pull(path)

read_data <- function(path, dataset) {
  number <- str_extract(path, "\\d+")
  print(number)
  data <- read_csv(paste0(path, "/", dataset, "_proposal_number_", number, ".csv"))
  print(data)
  data_clean <- data %>% 
    mutate(proposal_number = number)
  print(data_clean)
}

read_data(proposal_folders[1], "borough")

clean_ballot_data <- map_df(proposal_folders, ~read_data(.x, "borough")) %>% 
  filter(boro == "Total")  %>% 
  pivot_wider(names_from = "candidate", values_from = "votes") %>% 
  mutate(percentage = YES/(YES+NO)*100,
         passed = if_else(YES>NO,TRUE, FALSE))

write_csv(clean_ballot_data, "analysis/clean_ballot_data.csv")


##################################################

xwalk <- read_csv("analysis/crosswalk_coundist_electdist.csv", col_types = cols(.default = col_character()))

ed_data <- map_df(proposal_folders, ~read_data(.x, "ed")) %>% 
  mutate(ed_clean = str_c(
    # extract just the number from AD_num
    str_extract(AD_num, "\\d+"),
    # extract the number from ED_num, pad left to 3 digits
    str_pad(str_extract(ED_num, "\\d+"), width = 3, pad = "0")
    )
  ) %>% 
  left_join(xwalk, by = c("ed_clean"="elect_dist"))

councilmembers <- read_csv("https://data.cityofnewyork.us/resource/uvw5-9znb.csv", col_types = cols(.default = col_character())) %>% 
  mutate(political_party=case_when(name == "Justin Brannan" ~ "Democrat",
                                   name == "Kristy Marmorato" ~ "Republican",
                                   T~ political_party),
         name=case_when(name == "Office of Council District 44" ~ "Simcha Felder",
                                   name == "Office of Council District 51" ~ "Frank Morano",
         T ~ name))

coun_sum <- ed_data %>% 
  group_by(proposal_number, CounDist) %>% 
  summarize(YES = sum(votes[candidate=="YES"], na.rm = T),
            NO = sum(votes[candidate=="NO"])) %>% 
  mutate(percentage = YES/(YES+NO)*100,
         passed = if_else(YES>NO,TRUE, FALSE)) %>% 
  left_join(councilmembers, by = c("CounDist"="district"))

walk(as.character(2:5),
     ~write_csv(coun_sum %>% filter(proposal_number==.x), paste0("analysis/proposal_", .x,"_results.csv"))
     )

aff_hsg <- read_csv("analysis/tabula-NYHC-Tracker-2025-Apr2025.csv") %>% 
  janitor::clean_names() %>% 
  filter(rank_2014_24 != "RANK 2014-24") %>% 
  mutate(total_2014_24 = as.numeric(str_remove_all(total_2014_24, ",")),
         x2014_23 = as.numeric(str_remove_all(x2014_23, ",")),
         average_total = if_else(x2014_23 >= 1844, "above average", "below average"))

ballot_avg <- coun_sum %>% 
  filter(proposal_number %in% c("2","3","4")) %>% 
  group_by(CounDist) %>% 
  summarize(average = mean(percentage, na.rm = T),
            total_yes = sum(YES, na.rm = T),
            total_no = sum(NO, na.rm = T),
            avg_yes = sum(YES, na.rm = T),
            avg_no = sum(NO, na.rm = T),
            )%>% 
  left_join(councilmembers, by = c("CounDist"="district")) %>% 
  left_join(aff_hsg, by = c("CounDist" = "district"))

write_csv(ballot_avg, "analysis/proposal_avg_result.csv")
write_csv(ballot_avg %>% filter(average_total=="above average"), "analysis/proposal_avg_result_hsg_above.csv")
write_csv(ballot_avg %>% filter(average_total=="below average"), "analysis/proposal_avg_result_hsg_below.csv")
write_csv(ballot_avg %>% filter(political_party=="Democrat"), "analysis/proposal_avg_result_dem.csv")
write_csv(ballot_avg %>% filter(political_party=="Republican"), "analysis/proposal_avg_result_rep.csv")

ed_data_sum <- ed_data %>% 
  filter(proposal_number %in% c("2","3","4")) %>% 
  group_by(ed_clean) %>% 
  summarize(YES = sum(votes[candidate=="YES"], na.rm = T),
            NO = sum(votes[candidate=="NO"]),
            reported = first(reported),
            ad = first(as.numeric(str_sub(ed_clean, 1,2))),
            ed = first(as.numeric(str_sub(ed_clean, 3,5)))) %>% 
  mutate(percentage = YES/(YES+NO)*100,
         passed = if_else(YES>NO,TRUE, FALSE)
)

write_csv(ed_data_sum, "analysis/proposal_avg_result_ed.csv")

#pivot_wider(names_from = "candidate", values_from = "votes") %>% 
  



