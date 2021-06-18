library(rvest)
library(stringr)

source("regex_list.R")

getDecRes <- function(tables) {
    dec <- str_which(tables, "Dec\\n")
    decRes <- tables[dec] %>% html_text() # contains all scores related to the decathlon
    decEventNames <- c("100", "LJ", "SP", "HJ", "400", "110H", "DT", "PV", "JT", "1500")
    eventNameRe <- paste0("(?s)(?<=", decEventNames,").*")
    dec_scores = vector(mode = "list", length = length(eventNameRe))
    for(i in 1:length(eventNameRe)) {
        allTextAfter <- str_extract(decRes, eventNameRe[i])
        dec_scores[[i]] <- str_extract(allTextAfter, decScoresRe[i])
        names(dec_scores)[[i]] <- decEventNames[i]
    }
    return(dec_scores)
}

getHepMRes <- function(tables) {
    hep <- str_which(tables, "Hep\\n")
    hepRes <- tables[hep] %>% html_text() # contains all scores related to the decathlon
    hepEventMNames <- c("1000", "60", "LJ", "SP", "HJ", "60H", "PV")
    eventNameRe <- paste0("(?s)(?<=", hepEventMNames,").*")
    hep_scores = vector(mode = "list", length = length(eventNameRe))
    for(i in 1:length(eventNameRe)) {
        allTextAfter <- str_extract(hepRes, eventNameRe[i])
        hep_scores[[i]] <- str_extract(allTextAfter, hepScoresMRe[i])
        names(hep_scores)[[i]] <- hepEventMNames[i]
    }
    return(hep_scores)
}

getHepWRes <- function(tables) {
    hep <- str_which(tables, "Hep\\n")
    hepRes <- tables[hep] %>% html_text() # contains all scores related to the decathlon
    hepEventWNames <- c("100H", "HJ", "SP", "200", "LJ", "JT", "800")
    eventNameRe <- paste0("(?s)(?<=", hepEventWNames,").*")
    hep_scores = vector(mode = "list", length = length(eventNameRe))
    for(i in 1:length(eventNameRe)) {
        textAfterHep <- str_extract(hepRes, "(?s)(?<=Hep).*")
        allTextAfter <- str_extract(textAfterHep, eventNameRe[i])
        hep_scores[[i]] <- str_extract(allTextAfter, hepScoresWRe[i])
        names(hep_scores)[[i]] <- hepEventWNames[i]
    }
    return(hep_scores)
}

getPentRes <- function(tables) {
    pent <- str_which(tables, "Pent\\n")
    pentRes <- tables[pent] %>% html_text() # contains all scores related to the decathlon
    pentEventNames <- c("60H", "HJ", "SP", "LJ", "800")
    eventNameRe <- paste0("(?s)(?<=", pentEventNames,").*")
    pent_scores = vector(mode = "list", length = length(eventNameRe))
    for(i in 1:length(eventNameRe)) {
        allTextAfter <- str_extract(pentRes, eventNameRe[i])
        pent_scores[[i]] <- str_extract(allTextAfter, pentScoresRe[i])
        names(pent_scores)[[i]] <- pentEventNames[i]
    }
    return(pent_scores)
}

#oneAthlete <- read_html("https://www.tfrrs.org/athletes/6972495/Michigan/Ayden_Owens")
oneMAthlete <- read_html("https://www.tfrrs.org/athletes/6997786/Georgia/Karel_Tilga")
oneMAthlete

tablesM <- oneMAthlete %>% 
    html_nodes("table")
# for some reason html_nodes doesn't recognize tbody tags

getDecRes(tablesM)
getHepMRes(tablesM)


oneWAthlete <- read_html("https://www.tfrrs.org/athletes/6559750/Texas_AM/Tyra_Gittens.html")
oneWAthlete

tablesW <- oneWAthlete %>% 
    html_nodes("table")

getPentRes(tablesW)
getHepWRes(tablesW)



