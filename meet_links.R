library(rvest)
library(httr)
library(tidyverse)

source("meets_one_athlete.R")

getMultiLinks <- function(url) {
    meetPageLinks <- read_html(url) %>% 
        html_nodes("a") %>% 
        html_attr('href') %>% 
        tolower()
    
    pent <- str_which(meetPageLinks, "pentathlon")
    hep <- str_which(meetPageLinks, "heptathlon") #get men's vs. women's here?
    dec <- str_which(meetPageLinks, "decathlon")
    multi <- str_which(meetPageLinks, "multi")
    
    linksToMulti <- c(pent, hep, dec, multi)[which(c(pent, hep, dec, multi) > 0)]
    multiLinks <- meetPageLinks[linksToMulti] %>% 
        str_replace_all("//", "") %>% 
        str_replace_all("/\\n", "") %>% 
        str_replace_all("www", "https://www")
    return(multiLinks)
}

getMeetsOneYear <- function(year) {
    if(!is.character(year)) {
        year <- as.character(year)
    }
    res <- POST("https://www.tfrrs.org/results_search.html", 
              body = list(
                  meet_name = "",
                  sport = "track",
                  state = "",
                  month = "",
                  year = year # enter year as string based on website source code
              )) %>% 
        content(as = "parsed",
                type = "text/html; charset=utf-8", # can be deleted w/o changing results
                encoding = "ISO-8859-1") %>% # tbh I don't know what this means
        html_nodes("a") %>% 
        html_attr('href')
    
    return(res)
}

years <- 2009:2021
meets <- character()
for (year in years) {
    meetsOneYear <- getMeetsOneYear(year)
    meets <- c(meets, meetsOneYear)
}

links <- str_which(meets, "/www.tfrrs.org/results/\\d+/")
meetLinks <- meets[links] %>% 
    str_replace_all("//", "") %>% 
    str_replace_all("/\\n", "") %>% 
    str_replace("www", "https://www")

isThereMulti <- function(url) {
    res <- read_html(url) %>% 
        html_nodes("table") %>% 
        tolower() %>% 
        str_which("heptathlon|pentathlon|decathlon|multi")
    if(length(res) > 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

### there is a weird case where the title of the meet has the word "multi" in it and then you scrape every athlete from that meet, not just the ones who did the multi events. This code fixes that
multiLinkText <- read_html(meetLinks[60]) %>% 
    html_nodes("table") %>% 
    html_nodes("a") %>% 
    html_text() %>% 
    tolower() %>% 
    str_which("heptathlon|pentathlon|decathlon|multi")
    
eventLinks <- read_html(meetLinks[60]) %>% 
    html_nodes("table") %>% 
    html_nodes("a") %>% 
    html_attr("href")

eventLinks[multiLinkText]
###


library(progress)
small <- meetLinks[1:1000]

pb <- progress_bar$new(total = length(small))
linkList <- character()

for (i in 1:length(small)) {
    pb$tick()
    url <- small[i]
    if (isThereMulti(url) == FALSE) {
        next()
        # skip the rest of this loop and go to the next url
    }
    
    linksThisMeet <- getMultiLinks(url)
    
    for (links in linksThisMeet) {
        pageLinks <- read_html(links) %>%
            html_nodes("a")  %>%
            html_attr('href')
        
        athletes <-
            str_which(pageLinks, "/www.tfrrs.org/athletes/\\d+/")
        athleteLinks <- unique(pageLinks[athletes]) %>%
            str_replace_all("//", "") %>%
            str_replace_all("/\\n", "") %>%
            str_replace("www", "https://www")
        linkList <- c(linkList, athleteLinks)
    }
}

tail(linkList)
