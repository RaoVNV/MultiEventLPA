library(rvest)
library(stringr)
library(tidyverse)

source("regex_list.R")
source("get_event_funcs.R")

oneAthlete <- read_html("https://www.tfrrs.org/athletes/6997786/Georgia/Karel_Tilga")
# oneAthlete <- read_html("https://www.tfrrs.org/athletes/6559750/Texas_AM/Tyra_Gittens.html")
oneAthlete

tables <- oneAthlete %>% 
    html_nodes("table")

meets <- grep("\\n\\n\\t", tables, perl = TRUE)

meetText <- tables[meets] %>% html_text
# meetNames <- str_extract(meetText, "(?<=\\n\\n\\t).*")

reDates <- c("[a-zA-Z]{3}\\s{1,2}\\d{1,2}-\\d{1,2},\\s\\d{4}",
             "[a-zA-Z]{3}\\s{1,2}\\d{1,2},\\s\\d{4}",
             "[a-zA-Z]{3}\\s{1,2}\\d{2}\\s-\\s[a-zA-Z]{3}\\s{2}\\d{1},\\s\\d{4}")
reDates <- paste0(reDates[1], "|", reDates[2], "|", reDates[3])

# meetDates <- str_extract(meetText, reDates)
# meetsNameDate <- cbind(meetNames, meetDates)


eventNamesRe <- paste0("(?s)(?<=", allEventNames,"\\n).*")
names(eventNamesRe) <- names(allScoresRe)

allMeetRes <- function(x) {
    # browser()
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
