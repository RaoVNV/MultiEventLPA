library(rvest)
library(httr)
library(tidyverse)

get_meets_for_year <- function(year){
    x <- POST("https://www.tfrrs.org/results_search.html", 
              body = list(
                  year = year,
                  sport = "track:xc",
                  meet_name = "",
                  state = "",
                  month = ""
              )) %>% 
        content(as = "parsed") %>% 
        html_node("table") %>% 
        html_table()
    
    return(x)
}

url <- "https://www.tfrrs.org/results_search.html"
    
htmlPage <- read_html(url)
links <- htmlPage %>% 
    html_nodes("a") %>% 
    html_attr('href')
length(links)


links <- POST("https://www.tfrrs.org/results_search.html", 
     body = list(
         meet_name = "",
         sport = "track",
         state = "",
         month = "",
         year = "2016" # year needcs to be entered as a string for some reason
    )) %>% 
    content(as = "parsed",
            type = "text/html; charset=utf-8", # can be deleted w/o changing results
            encoding = "ISO-8859-1") %>% # tbh I don't know what this means
    html_nodes("a") %>% 
    html_attr('href')
# I have a suspicion that the maximum number of links that can get returned are 1029. 


length(links)

xcMeets <- str_which(links, "xc")
tfMeets <- links[-xcMeets]


links[(length(links)-50):length(links)]


# all the lengths of the links for each individual year is 1029, which is pretty suspicious...

all_meets <- map_dfr(2009:2021,get_meets_for_year)
