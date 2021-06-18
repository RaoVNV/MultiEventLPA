library(rvest)
library(stringr)
library(tidyverse)

source("regex_list.R")
source("get_event_funcs.R")

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
# outer for loop
# for(meet in meetRes) {
#     # searching for which events the athlete competed in will be the inner forloop
# }
res <- tibble()
# inner for loop
for (event in eventNamesRe) {
    # now we need to search for all variations of the multi events
    pent <- str_which(textAfterMeet[11], "Pent\\n")
    # look for the hep, and if found, look to see if there are complete entries for both the M and W's hep. If there are complete entries, you know if they're M or W, and use the corresponding getHepScoresM or getHepScoresW function.
    if (length(pent) > 0) {
        pentScores <- getPentScores(textAfterMeet[11])
        res <- bind_rows(res, pentScores)
        textAfterMeet[11] <- str_extract(textAfterMeet[11], "(?s).*(?=Pent)")
    }
    textAfterEvent <- str_extract(textAfterMeet[11], event)
    if (!is.na(textAfterEvent)) {
        newRow <- getEventScore(textAfterEvent[11])
        res <- bind_rows(res, newRow)
    }
}

res

