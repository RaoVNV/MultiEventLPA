library(rvest)
library(stringr)
library(tidyverse)

source("regex_list.R")
source("get_event_funcs.R")

# oneAthlete <- read_html("https://www.tfrrs.org/athletes/6997786/Georgia/Karel_Tilga")
# oneAthlete <- read_html("https://www.tfrrs.org/athletes/6559750/Texas_AM/Tyra_Gittens.html")
# check to make sure that other weird events are also downloaded properly
# oneAthlete <- read_html("https://www.tfrrs.org/athletes/6422180/Notre_Dame/Yared_Nuguse")
# oneAthlete <- read_html("https://www.tfrrs.org/athletes/6011691/Minnesota/Alec_Basten")
# oneAthlete <- read_html("https://www.tfrrs.org/athletes/3761344/St_Olaf/Anton_Hesse.html") #it's me lol
oneAthlete <- read_html("https://www.tfrrs.org/athletes/4680867/Carthage/Dan__Hoffman.html")

# at some point all off this should be combined into one function whose argument is just a url
athleteName <- oneAthlete %>% 
    html_nodes("title") %>% 
    html_text() %>% 
    str_extract("(?<=\\| ).*") %>% 
    str_extract(".*(?= - )")
athleteName

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

tables <- oneAthlete %>% 
    html_nodes("table")
meets <- grep("\\n\\n\\t", tables, perl = TRUE)
meetText <- tables[meets] %>% html_text

eventNamesRe <- paste0("(?s)(?<=", allEventNames,"\\n).*")
names(eventNamesRe) <- names(allScoresRe)

allMeetRes <- function(x) {
    browser()
    res <- tibble()
    for(meet in meetText) { # outer for loop goes through each meet
        multi <- detectMulti(meet)
        meetName <- str_extract(meet, "(?<=\\n\\n\\t).*")
        meetDate <- str_extract(meet, reDates)
        for (event in eventNamesRe) { # inner for loop goes through each event
            # now we need to search for all variations of the multi events
            if (!is.null(names(multi))) {
                multiScores <- getMultiScores(meet, multi)
                res <- bind_rows(res, multiScores)
                
                textBeforeMultiRe <- paste0("(?s).*(?=", str_to_title(names(multi)), ")")
                meet <- str_extract(meet, textBeforeMultiRe)
                names(multi) <- NULL
            }
            
            textAfterEvent <- str_extract(meet, event)
            if (!is.na(textAfterEvent)) {
                newRow <- getEventScore(textAfterEvent, eventNamesRe, event, meetName, meetDate)
                res <- bind_rows(res, newRow)
            }
        }
    }
    return(res)
}

meetRes <- allMeetRes(meetText)
meetRes %>% View

