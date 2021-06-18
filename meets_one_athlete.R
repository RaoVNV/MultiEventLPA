library(rvest)
library(stringr)
library(tidyverse)

source("regex_list.R")

#oneAthlete <- read_html("https://www.tfrrs.org/athletes/6972495/Michigan/Ayden_Owens")
oneAthlete <- read_html("https://www.tfrrs.org/athletes/6559750/Texas_AM/Tyra_Gittens.html")
oneAthlete

tables <- oneAthlete %>% 
    html_nodes("table")

meets <- grep("\\n\\n\\t", tables, perl = TRUE)

meetRes <- tables[meets] %>% html_text
meetNames <- str_extract(meetRes, "(?<=\\n\\n\\t).*")

reDates1 <- "[a-zA-Z]{3}\\s{1,2}\\d{1,2}-\\d{1,2},\\s\\d{4}"
reDates2 <- "[a-zA-Z]{3}\\s{1,2}\\d{1,2},\\s\\d{4}"
reDates3 <- "[a-zA-Z]{3}\\s{1,2}\\d{2}\\s-\\s[a-zA-Z]{3}\\s{2}\\d{1},\\s\\d{4}"
reDates <- paste0(reDates1,"|",reDates2,"|",reDates3)

meetDates <- str_extract(meetRes, reDates)
meetsNameDate <- cbind(meetNames, meetDates)
meetsNameDate


eventNamesRe <- paste0("(?s)(?<=", allEventNames,"\\n).*")
names(eventNamesRe) <- names(allScoresRe)

textAfterMeet <- str_extract(meetRes, "(?s)(?<=\\n\\n\\t).*")
textAfterMeet[11]
# outer for loop
# for(meet in meetRes) {
#     # searching for which events the athlete competed in will be the inner forloop
# }
res <- tibble()
# inner for loop
for (event in eventNamesRe) {
    pent <- str_which(textAfterMeet[11], "Pent\\n")
    if (length(pent) > 0) {
        pentScores = character()
        for (pentEvent in pentEventNames) {
            pentEventRe <- pentNamesRe[str_which(pentNamesRe, pentEvent)]
            textAfterEvent <-
                str_extract(textAfterMeet[11], pentEventRe)
            pentScores[pentEvent] <- str_extract(textAfterEvent,
                                                 pentScoresRe[names(pentEventRe)])
        }
        pentResThisMeet <-
            tibble(
                meet = rep(meetNames[11], times = 5),
                date = rep(meetDates[11], times = 5),
                event = pentEventNames,
                bind_cols(result = pentScores),
                multi = rep("pent", times = 5)
            )
        res <- bind_rows(res, pentResThisMeet)
        textAfterMeet[11] <-
            str_extract(textAfterMeet[11], "(?s).*(?=Pent)")
    }
    
    textAfterEvent <- str_extract(textAfterMeet[11], event)
    if (!is.na(textAfterEvent)) {
        scoreRe <- names(eventNamesRe)[which(eventNamesRe == event)]
        score <-
            str_extract(textAfterEvent, allScoresRe[scoreRe])
        eventName <-
            names(eventNamesRe)[which(eventNamesRe == event)]
        newRow <- c(
            meet = meetNames[11],
            date = meetDates[11],
            event = str_replace(eventName, "re", ""),
            result = score,
            multi = "no"
        )
        res <- bind_rows(res, newRow)
    }
}

res

event
event <- eventNamesRe[14]
str_which(textAfterMeet[3], "Hep\\n")
# strategy to prevent scraping events within multis as part of multis and on their own:
# at the end of each meet, check and see if any scores for events are the same. e.g, if the same score for HJ shoes up, it's probably from the multis.
# OR search FIRST for multi-event keywords. If they appear, try and temporarily delete that text. If the multi event scores consistenly appear after everything else, then you can temporarily delete that text and pull in other scores.
