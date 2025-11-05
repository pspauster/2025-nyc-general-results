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

coun_sum <- ed_data %>% 
  group_by(proposal_number, CounDist) %>% 
  summarize(YES = sum(votes[candidate=="YES"], na.rm = T),
            NO = sum(votes[candidate=="NO"])) %>% 
  mutate(percentage = YES/(YES+NO)*100,
         passed = if_else(YES>NO,TRUE, FALSE))

write_csv(coun_sum %>% filter(proposal_number=="2"), "analysis/proposal_2_results.csv")

ballot_avg <- coun_sum %>% 
  filter(proposal_number %in% c("2","3","4")) %>% 
  group_by(CounDist) %>% 
  summarize(average = mean(percentage, na.rm = T),
            total_yes = sum(YES, na.rm = T),
            total_no = sum(NO, na.rm = T),
            avg_yes = sum(YES, na.rm = T),
            avg_no = sum(NO, na.rm = T),
            )

write_csv(ballot_avg, "analysis/proposal_avg_result.csv")

#pivot_wider(names_from = "candidate", values_from = "votes") %>% 
  



