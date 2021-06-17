library(rvest)
library(stringr)

source("re_dec.R")

#oneAthlete <- read_html("https://www.tfrrs.org/athletes/6972495/Michigan/Ayden_Owens")
oneAthlete <- read_html("https://www.tfrrs.org/athletes/6997786/Georgia/Karel_Tilga")
oneAthlete

tables <- oneAthlete %>% 
    html_nodes("table")

meets <- grep("\\n\\n\\t", tables, perl = TRUE)

meetRes <- tables[meets] %>% html_text

allTextAfter <- str_extract(meetRes, "(?s)(?<=\\n\\n\\t).*")
meetNames <- str_extract(meetRes, ".+(?=\\n\\n\\s)")
meetNames <- str_extract(meetRes, "(?<=\\t).*")



reDates1 <- "[a-zA-Z]{3}\\s{1,2}\\d{1,2}-\\d{1,2},\\s\\d{4}"
reDates2 <- "[a-zA-Z]{3}\\s{1,2}\\d{1,2},\\s\\d{4}"
reDates3 <- "[a-zA-Z]{3}\\s{1,2}\\d{2}\\s-\\s[a-zA-Z]{3}\\s{2}\\d{1},\\s\\d{4}"

reDates <- paste0(reDates1,"|",reDates2,"|",reDates3)

meetDates <- str_extract(meetRes, reDates)
meetsNameDate <- cbind(meetNames, meetDates)
meetsNameDate
