library(rvest)
library(tidyverse)
library(purrr)

##### Get race list


#for future - create a changeable output directory for the year/election

# Set the URL
url <- "https://enr.boenyc.gov/index.html"

# Read the HTML of the page
webpage <- read_html(url)

# Extract tables
tables <- webpage %>%
  html_nodes("table")

# Assign Results
results_table <- tables %>% .[3] %>% html_table(fill = F) %>% as.data.frame()

data <- tables[3] %>%
  html_elements("tr") %>%
  map(~ .x %>%
        html_elements("td, th") %>%
        map_chr(~ as.character(.x))
  )

# Get all the titles and links from the html table
extract_links <- function(row) {
  # Extract the 'title' and 'href' attributes for each cell with an <a> tag
  titles <- map_chr(row, ~ {
    cell <- read_html(.x) %>% html_element("a")
    if (!is.null(cell)) cell %>% html_attr("title") else NA_character_
  })
  
  hrefs <- map_chr(row, ~ {
    cell <- read_html(.x) %>% html_element("a")
    if (!is.null(cell)) cell %>% html_attr("href") else NA_character_
  })
  
  # Return a data frame with the extracted titles and hrefs for each cell
  data.frame(title = titles, href = hrefs, stringsAsFactors = FALSE)
}

# Apply the function to each row
links_master <- map_dfr(data, extract_links) %>%
  filter(!is.na(href)) %>% 
  separate(title, sep = ": ", into = c("level", "race"), remove = T) %>% 
  mutate(link = paste0("https://enr.boenyc.gov/",href),
         race = str_replace(race, "/", "_"))

write_csv(links_master, "output/races_master_list.csv")


##### Define functions

read_page_results <- function(url) {
  webpage <- read_html(url)
  
  tables <- webpage %>% 
    html_nodes("table")
}

process_results_table <- function(page_tables) {
  results_table <- page_tables %>% .[3] %>% html_table(fill = F) %>% as.data.frame()
  new_colnames <- paste(results_table[1, ], results_table[2, ], sep = "_")
  if(any(c("New York", "Queens", "Kings", "Richmond", "Bronx") %in% results_table[[1]])) {
    new_colnames[1] <- "boro"
  } else if("AD" %in% str_sub(results_table[[1]],1,2)) {
    new_colnames[1] <- "AD_num"
  } else if("ED" %in% str_sub(results_table[[1]],1,2)) {
    new_colnames[1] <- "ED_num"
  }
  names(results_table) <- new_colnames
  results_table <- results_table[-c(1, 2), ] %>%
    select(-starts_with("NA_NA")) %>% 
    pivot_longer(cols = 3:ncol(.), 
                 names_to = "candidate_party", 
                 values_to = "votes") %>%
    # Separate candidate and party based on underscore "_"
    separate(candidate_party, into = c("candidate", "party"), sep = "_", extra = "merge") %>% 
    rename(reported = 2)
}

get_page_links <- function(page_tables) {
  links <- page_tables[3] %>%
    html_elements("tr") %>%
    map(~ .x %>%
          html_elements("td, th") %>%
          map_chr(~ as.character(.x))
    )
  
  map_dfr(links, extract_links) %>% 
    filter(!is.na(href)) %>% 
    mutate(link = paste0("https://enr.boenyc.gov/",href))
}

write_race_data <- function(race_url, race, sub_race) {
  print(paste0("scraping data for ", race, sub_race))
  
  time <- str_replace_all(now(), " ", "_")
  race_stub <- str_replace_all(race, " ", "_") %>% str_to_lower()
  page <- read_page_results(race_url)
  
  boro_results <- process_results_table(page) %>% 
    mutate(scrape_time = time)#modification for not party?
  if (sub_race != "") {
    write_csv(boro_results, paste0("output/", race, "/", sub_race, "/borough_", race_stub, ".csv"))
  } else {
    write_csv(boro_results, paste0("output/", race, "/borough_", race_stub, ".csv"))
  }
  print("Wrote out borough data")
  
  ad_links <- get_page_links(page)
  
  ad_link <- ad_links %>% filter(title == "Total") %>% pull(link)
  ad_page <- read_page_results(ad_link)
  ad_results <- process_results_table(ad_page) %>% 
    mutate(scrape_time = time)
  if (sub_race != "") {
    write_csv(ad_results, paste0("output/", race, "/", sub_race, "/ad_", race_stub, ".csv"))
  } else {
    write_csv(ad_results, paste0("output/", race, "/ad_", race_stub, ".csv"))
  }
  print("Wrote out AD data")
  
  ed_links <- get_page_links(ad_page)
  
  ed_results <- map2_df(ed_links$link, ed_links$title,
                        ~mutate(process_results_table(read_page_results(.x)),
                                AD_num = .y,
                                scrape_time = time))
  if (sub_race != "") {
    write_csv(ed_results, paste0("output/", race, "/", sub_race, "/ed_", race_stub, ".csv"))
  } else {
    write_csv(ed_results, paste0("output/", race, "/ed_", race_stub, ".csv"))
  }
  print("Wrote out ED data")
}

scrape_race <- function(race, race_url) {
  page <- read_page_results(race_url)
  sub_links <- get_page_links(page)
  
  if("Total" %in% sub_links$title) {
    write_race_data(race_url, race, sub_race = "")
  } else {
    
    subraces <- sub_links$title
    sublinks <- sub_links$link
    
    # Create each folder if it doesn't exist using walk
    walk(subraces, ~ if (!dir.exists(paste0("output/",race,"/", .x))) {
      dir.create(paste0("output/",race, "/", .x))
      message(paste("Created folder:", .x))
    } else {
      message(paste("Folder already exists:", .x))
    })
    
    walk2(.x = sublinks, .y = subraces, ~write_race_data(race_url = .x, race = race, sub_race = .y))
  }
}

####### Scrape away!

race_ads <- links_master %>% filter(level == "By AD")

# Create each folder if it doesn't exist using walk
walk(race_ads$race, ~ if (!dir.exists(paste0("output/", .x))) {
  dir.create(paste0("output/", .x))
  message(paste("Created folder:", .x))
} else {
  message(paste("Folder already exists:", .x))
})

walk2(race_ads$race, race_ads$link, ~scrape_race(.x,.y))







