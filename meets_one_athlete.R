# there might be more events on directathletics.org

library(rvest)
library(stringr)
library(tidyverse)

source("regex_list.R")
source("get_event_funcs.R")

resOneAthlete <- function(url) {
    htmlPage <- read_html(url)
    # browser()
    athleteName <- htmlPage %>% 
        html_nodes("title") %>% 
        html_text() %>% 
        str_extract("(?<=\\| ).*") %>% 
        str_extract(".*(?= - )")
    
    tables <- htmlPage %>% 
        html_nodes("table")
    meets <- grep("\\n\\n\\t", tables, perl = TRUE)
    meetText <- tables[meets] %>% html_text
    
    return(bind_cols(name = athleteName, allMeetRes(meetText)))
}

# resOneAthlete("https://www.tfrrs.org/athletes/6422180/Notre_Dame/Yared_Nuguse")
# resOneAthlete("https://www.tfrrs.org/athletes/3761344/St_Olaf/Anton_Hesse.html")

# oneAthlete <- read_html("https://www.tfrrs.org/athletes/6997786/Georgia/Karel_Tilga")
# oneAthlete <- read_html("https://www.tfrrs.org/athletes/6559750/Texas_AM/Tyra_Gittens.html")
# check to make sure that other weird events are also downloaded properly
# oneAthlete <- read_html("https://www.tfrrs.org/athletes/6422180/Notre_Dame/Yared_Nuguse")
# oneAthlete <- read_html("https://www.tfrrs.org/athletes/6011691/Minnesota/Alec_Basten")
# oneAthlete <- read_html("https://www.tfrrs.org/athletes/3761344/St_Olaf/Anton_Hesse.html") #it's me lol
# oneAthlete <- read_html("https://www.tfrrs.org/athletes/4680867/Carthage/Dan__Hoffman.html")

# Getting lists of athletes:
# https://www.tfrrs.org/results_search.html meet results
# somehow filter by T&F if possible
# somehow filter by state/province and year
# get a link for a meet
# use a regex to see if where any multi's in that meet
# if there were multi's, go to the link for the multi's
# get athlete names and also get their sex (e.g. search "Women's")
# see if results have already been compiled for that athlete
# if a new athlete, get URL for that athlete
# use the rest of this script to get their scores from their indiviual page

# page <- read_html("https://www.tfrrs.org/results_search.html")
# tbody <- page %>% 
#     html_nodes("tbody")
# 
# meets <- grep("OTF", tbody, perl = TRUE)
# text <- tbody[meets] %>% html_text()
# 
# 
# 
# page <- read_html("https://www.tfrrs.org/results_search.html")
# 
# table <- page %>% 
#     html_node("table") %>% 
#     html_table()
# 
# View(table)
# 
# page <- read_html("https://www.tfrrs.org/results/1")
# page %>% html_text()
