library(rvest)
library(httr)
library(tidyverse)
library(progress)

# source("meets_one_athlete.R")
source("regex_list.R")
source("get_link_funcs.R")

years <- 2009:2021
meetLinks <- getMeetLinks(years)
meetLinks <- meetLinks %>% 
    as_tibble() %>% 
    rename(link = value)

write_csv(meetLinks, "meetLinks.csv")
