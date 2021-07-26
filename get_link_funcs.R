library(rvest)
library(httr)
library(tidyverse)
library(progress)

getMeetsOneYear <- function(year) { # gets url's for a single year
    year <- as.character(year)
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

getMeetLinks <- function(x) {
    meets <- character()
    pb <- progress_bar$new(total = length(years))
    for (year in x) {
        pb$tick()
        meetsOneYear <- getMeetsOneYear(year)
        meets <- c(meets, meetsOneYear)
    }
    links <- str_which(meets, "/www.tfrrs.org/results/\\d+/")
    meetLinks <- meets[links] %>% 
        str_replace_all("//", "") %>% 
        str_replace_all("/\\n", "") %>% 
        str_replace("www", "https://www")
    return(meetLinks)
}

isThereMulti <- function(url) { # detects if the webpage references multi events
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

getMultiLinks <- function(url) { # gets url's to mutl event result pages
    multiLinkText <- read_html(url) %>% 
        html_nodes("table") %>% 
        html_nodes("a") %>% 
        html_text() %>% 
        tolower() %>% 
        str_which("heptathlon|pentathlon|decathlon|multi")
    
    eventLinks <- read_html(url) %>% 
        html_nodes("table") %>% 
        html_nodes("a") %>% 
        html_attr("href")
    
    meetPageLinks <- eventLinks[multiLinkText] %>% 
        tolower()
    
    pent <- str_which(meetPageLinks, "pentathlon")
    hep <- str_which(meetPageLinks, "heptathlon") #get men's vs. women's here?
    dec <- str_which(meetPageLinks, "decathlon")
    multi <- str_which(meetPageLinks, "multi")
    
    linksToMulti <- c(pent, hep, dec, multi)[which(c(pent, hep, dec, multi) > 0)]
    multiLinks <- meetPageLinks[linksToMulti] %>% 
        str_replace_all("//", "") %>% 
        str_replace_all("/\\n", "") %>% 
        str_replace_all("www", "https://www") %>% 
        unique()
    
    return(multiLinks)
}

mOrW <- function(url) { # determines if results page is for men's vs. women's
    h3Text <- read_html(url) %>%
        html_nodes("h3") %>%
        html_text() %>%
        tolower() %>%
        str_extract("women's|men's") %>% 
        unique() %>% 
        na.omit()
    
    if(h3Text == "women's") {
        sex = as.factor("w")
    } else {
        sex = as.factor("m")
    }
    
    return(sex)
}

getAthleteLinks <- function(x) { # input is vector of url's
    # browser()
    pb <- progress_bar$new(total = length(x))
    linkList <- tibble(link = character(), sex = factor())
    for (i in 1:length(x)) {
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
            
            athletes <- str_which(pageLinks, "/www.tfrrs.org/athletes/\\d+/")
            athleteLinks <- unique(pageLinks[athletes]) %>%
                str_replace_all("//", "") %>%
                str_replace_all("/\\n", "") %>%
                str_replace("www", "https://www")
            sex <- mOrW(links)
            res <- tibble(link = athleteLinks, sex = rep(sex, length(athleteLinks)))
            
            linkList <- bind_rows(linkList, res)
        }
    }
    return(linkList)
}

getMultiRes <- function(x) { # input is vector of athlete url's
    # browser()
    pb <- progress_bar$new(total = nrow(x))
    res <- tibble(name = character(),
                  sex = factor(),
                  meet = character(),
                  date = character(),
                  event = character(),
                  result = character(),
                  multi = character()
    )
    for(i in 1:nrow(x)) {
        pb$tick()
        temp <- resOneAthlete(x[[1]][i], x[[2]][i])
        res <- bind_rows(res, temp)
    }
    return(res)
}
