library(rvest)
library(stringr)

source("regex_list.R")

#oneAthlete <- read_html("https://www.tfrrs.org/athletes/6972495/Michigan/Ayden_Owens")
oneMAthlete <- read_html("https://www.tfrrs.org/athletes/6997786/Georgia/Karel_Tilga")
oneMAthlete

tables <- oneMAthlete %>% 
    html_nodes("table")
# for some reason html_nodes doesn't recognize tbody tags

getDecRes <- function(tables) {
    dec <- grep("Dec\\n", tables, perl = TRUE)
    decRes <- tables[dec] %>% html_text() # contains all scores related to the decathlon
    decEventNames <- c("100", "LJ", "SP", "HJ", "400", "110H", "DT", "PV", "JT", "1500")
    eventNameRe <- paste0("(?s)(?<=", decEventNames,").*")
    dec_scores = vector(mode = "list", length = length(decRes))
    for(i in 1:length(eventNameRe)) {
        allTextAfter <- str_extract(decRes, eventNameRe[i])
        dec_scores[[i]] <-
            str_extract(allTextAfter, decScoresRe[i])
        names(dec_scores)[[i]] <- eventNames[i]
    }
    return(dec_scores)
}

getHepMRes <- function(tables) {
    hep <- grep("Hep\\n", tables, perl = TRUE)
    hepRes <- tables[hep] %>% html_text() # contains all scores related to the decathlon
    hepEventMNames <- c("1000", "60", "LJ", "SP", "HJ", "60H", "PV")
    eventNameRe <- paste0("(?s)(?<=", hepEventMNames,").*")
    hep_scores = vector(mode = "list", length = length(hepRes))
    for(i in 1:length(eventNameRe)) {
        allTextAfter <- str_extract(hepRes, eventNameRe[i])
        hep_scores[[i]] <-
            str_extract(allTextAfter, hepScoresMRe[i])
        names(hep_scores)[[i]] <- hepEventMNames[i]
    }
    return(hep_scores)
}


getDecRes(tables)
getHepMRes(tables)

oneWAthlete <- read_html("https://www.tfrrs.org/athletes/6559750/Texas_AM/Tyra_Gittens.html")
oneWAthlete

tables <- oneWAthlete %>% 
    html_nodes("table")

getPentRes <- function(tables) {
    pent <- grep("Pent\\n", tables, perl = TRUE)
    pentRes <- tables[pent] %>% html_text() # contains all scores related to the decathlon
    pentEventNames <- c("1000", "60", "LJ", "SP", "HJ", "60H", "PV")
    eventNameRe <- paste0("(?s)(?<=", pentEventNames,").*")
    pent_scores = vector(mode = "list", length = length(pentRes))
    for(i in 1:length(eventNameRe)) {
        allTextAfter <- str_extract(pentRes, eventNameRe[i])
        pent_scores[[i]] <-
            str_extract(allTextAfter, pentScoresRe[i])
        names(pent_scores)[[i]] <- pentEventNames[i]
    }
    return(hep_scores)
}

