library(rvest)
library(stringr)

oneAthlete <- read_html("https://www.tfrrs.org/athletes/6972495/Michigan/Ayden_Owens")
oneAthlete

tables <- oneAthlete %>% 
    html_nodes("table")
# for some reason html_nodes doesn't recognize tbody tags

dec <- grep("Dec\\n", tables, perl = TRUE)

decRes <- tables[dec] %>% html_text() # contains all scores related to the decathlon

# str_extract(decRes, "(?s)(?<=100).*") # hard coding that returns everything after the 100
decEventNames <- c("100", "LJ", "SP", "HJ", "400", "110H", "DT", "PV", "JT", "1500")
eventNameRe <- paste0("(?s)(?<=", decEventNames,").*")

# regex for each event, assuming there is some form of whitespace preceeding it
re100 <- "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})"
reLJ <- "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})"
reSP <- "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2}m)"
reHJ <- "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2}m)"
re400 <- "(?<=\\s{0,1000}+)(\\d:){0,1}(\\d{2}\\.\\d{1,2})"
re110H <- "(?<=\\s{0,1000}+)(\\d{2}\\.\\d{1,2})"
reDT <- "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2}m)"
rePV <- "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2}m)"
reJT <- "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2}m)"
re1500 <- "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})"

eventScoresRe <- c(re100, reLJ, reSP, reHJ, re400, re110H, reDT, rePV, reJT, re1500)
eventNames <- c("100", "LJ", "SP", "HJ", "400", "110H", "DT", "PV", "JT", "1500")

dec_scores = vector(mode = "list", length = length(decRes))
for(i in 1:length(eventNameRe)) {
    allTextAfter <- str_extract(decRes, eventNameRe[i])
    dec_scores[[i]] <-
        str_extract(allTextAfter, eventScoresRe[i])
    names(dec_scores)[[i]] <- eventNames[i]
}
dec_scores

