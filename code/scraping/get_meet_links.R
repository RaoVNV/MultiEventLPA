library(rvest)
library(httr)
library(tidyverse)
library(progress)

# source("meets_one_athlete.R")
source("code/scraping/scraping_funcs/regex_list.R")
source("code/scraping/scraping_funcs/get_link_funcs.R")

years <- 2009:2021
meetLinks <- getMeetLinks(years)
meetLinks <- meetLinks %>% 
    as_tibble() %>% 
    rename(link = value)

write_csv(meetLinks, "data/raw/meetLinks.csv")
