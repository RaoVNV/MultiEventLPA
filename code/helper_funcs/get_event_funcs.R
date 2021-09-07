# Functions to get multi-event results

getEventScore <- function(x, eventNamesRe, event, meetName, meetDate) {
    scoreRe <- names(eventNamesRe)[which(eventNamesRe == event)]
    score <- str_extract(x, allScoresRe[scoreRe])
    eventName <- names(eventNamesRe)[which(eventNamesRe == event)]
    newRow <- c(
        meet = meetName,
        date = meetDate,
        event = str_replace(eventName, "re", ""),
        result = score,
        multi = "no"
    )
    return(newRow)
}

vectorIsNotEmpty <- function(x) {return(!(length(x) == 0))}

detectMulti <- function(x, sex) {
    # browser()
    # if else statements required b/c there are some entry errors where a male had a pent and hep entry
    if(sex == "w") {
        pent <- str_which(x, "Pent\\n{4,}") # 4 or more required because some meet names have hep and dec in tht title
        hep <- str_which(x, "Hep\\n{4,}")
        dec <- character()
        if(length(pent) > 0 & length(hep) > 0) {
            hep <- character()
            dec <- character()
        }
    } else if( sex == "m") {
        pent <- character()
        hep <- str_which(x, "Hep\\n{4,}")
        dec <- str_which(x, "Dec\\n{4,}")
        if(length(hep) > 0 & length(dec) > 0) {
            hep <- character()
            dec <- character()
        }
    }
    
    
    
    if(any(length(c(pent, hep, dec) > 0))) {
        idx <- which(map(list(pent, hep, dec), vectorIsNotEmpty) == TRUE)
        res <- c(pent, hep, dec)[idx]
        multiEventNames <- c("pent", "hep", "dec")
        names(res) <- multiEventNames[idx]
        return(res)
    }
}

getMultiScores <- function(x, event, sex) {
    # browser()
    multiScores <- character()
    multi <- match.arg(names(event), choices = c("pent", "hep", "dec"))
    switch(multi, pent = getPentScores(x),
           hep = getHepScores(x, sex),
           dec = getDecScores(x))
}

getPentScores <- function(x) { # requires full text from each meet, not textAfterMeet
    # browser()
    pentScores = character()
    for (pentEvent in pentEventNames) {
        pentEventRe <- pentNamesRe[str_which(names(pentNamesRe), paste0(pentEvent,"$"))]
        textAfterEvent <- str_extract(x, pentEventRe)
        pentScores[pentEvent] <- str_extract(textAfterEvent,
                                             pentScoresRe[names(pentEventRe)])
    }
    
    meetNames <- str_extract(x, "(?<=\\n\\n\\t).*")
    meetDates <- str_extract(x, reDates)
    eventNames <- pentEventNames
    
    pentResThisMeet <-
        tibble(
            meet = rep(meetNames, times = length(eventNames)),
            date = rep(meetDates, times = length(eventNames)),
            event = eventNames,
            bind_cols(result = pentScores),
            multi = rep("pent", times = length(eventNames))
        )
    return(pentResThisMeet)
}

# need to write more code so that when the event is a 55, 60, 55H, or 60H that we find the exact one that it is
getHepScores <- function(x, sex) { # requires full text from each meet, not textAfterMeet
    # browser()
    hepScores = character()
    if(sex == "w") {
        eventNames <- hepEventWNames
        for (hepEventW in hepEventWNames) {
            hepEventWRe <- hepNamesWRe[str_which(hepNamesWRe, hepEventW)]
            textAfterEvent <- str_extract(x, hepEventWRe)
            hepScores[hepEventW] <- str_extract(textAfterEvent,
                                                hepScoresWRe[names(hepEventWRe)])
        }
        eventNames <- hepEventWNames
    } else {
        for (hepEventM in hepEventMNames) {
            if (hepEventM == "55" | hepEventM == "60") {
                hepEventM <- str_extract(x, "55\\n|60\\n") %>% 
                    str_replace("\\n", "")
            } else if (hepEventM == "55H" | hepEventM == "60H") {
                hepEventM <- str_extract(x, "55H\\n|60H\\n") %>% 
                    str_replace("\\n", "")
            }
            hepEventMRe <-
                hepNamesMRe[str_which(names(hepNamesMRe), paste0(hepEventM, "$"))]
            textAfterEvent <- str_extract(x, hepEventMRe)
            if(length(textAfterEvent) == 0) {
                hepScores[hepEventM] <- NA
            } else if (is.na(textAfterEvent)) {
                hepScores[hepEventM] <- NA
            } else {
                hepScores[hepEventM] <- str_extract(textAfterEvent,
                                                    hepScoresMRe[names(hepEventMRe)])
            }
        }
        eventNames <- names(hepScores)
    }
    
    meetName <- str_extract(x, "(?<=\\n\\n\\t).*")
    meetDate <- str_extract(x, reDates)
    # will need to remove the unused 55 vs. 60 and 55H vs. 60H
    hepResThisMeet <-
        tibble(
            meet = rep(meetName, times = length(eventNames)),
            date = rep(meetDate, times = length(eventNames)),
            event = eventNames,
            bind_cols(result = hepScores),
            multi = rep("hep", times = length(eventNames))
        )
    return(hepResThisMeet)
}

getDecScores <- function(x) { # requires full text from each meet, not textAfterMeet
    # browser()
    decScores = character()
    for (decEvent in decEventNames) {
        decEventRe <- decNamesRe[str_which(names(decNamesRe), paste0(decEvent,"$"))]
        textAfterEvent <- str_extract(x, decEventRe)
        decScores[decEvent] <- str_extract(textAfterEvent,
                                             decScoresRe[names(decEventRe)])
    }
    
    meetName <- str_extract(x, "(?<=\\n\\n\\t).*")
    meetDate <- str_extract(x, reDates)
    eventNames <- decEventNames
    
    decResThisMeet <-
        tibble(
            meet = rep(meetName, times = length(eventNames)),
            date = rep(meetDate, times = length(eventNames)),
            event = eventNames,
            bind_cols(result = decScores),
            multi = rep("dec", times = length(eventNames))
        )
    return(decResThisMeet)
}

allMeetRes <- function(x, sex) { # this code only gets finals, not prelims. Fix later?
    # browser()
    res <- tibble()
    for(meet in x) { # outer for loop goes through each meet
        multi <- detectMulti(meet, sex)
        meetName <- str_extract(meet, "(?<=\\n\\n\\t).*")
        meetDate <- str_extract(meet, reDates)
        for (event in eventNamesRe) { # inner for loop goes through each event
            # now we need to search for all variations of the multi events
            if (!is.null(names(multi))) {
                multiScores <- getMultiScores(meet, multi, sex)
                res <- bind_rows(res, multiScores)
                textBeforeMultiRe <- paste0("(?s).*(?=",
                                            str_to_title(names(multi)), ")")
                meet <- str_extract(meet, textBeforeMultiRe)
                names(multi) <- NULL
            }
            tempRows <- tibble()
            textAfterEvent <- str_extract(meet, event)
            while(!is.na(textAfterEvent)) {
                newRow <- getEventScore(textAfterEvent,
                                        eventNamesRe,
                                        event,
                                        meetName,
                                        meetDate)
                tempRows <- bind_rows(tempRows, newRow)
                textAfterEvent <- str_extract(textAfterEvent, event)
            }
            if(nrow(tempRows) > 2) { # there should only be at most prelims and finals for a single meet
                tempRows <- tibble()
                
                # for IAAF there may be 2 prelims, but multi athletes are unlikely to be
                # in those events
            }
            res <- bind_rows(res, tempRows)
        }
    }
    return(res)
}

resOneAthlete <- function(url, sex) {
    htmlPage <- read_html(url)
    # browser()
    athleteName <- htmlPage %>% 
        html_nodes("title") %>% 
        html_text() %>% 
        str_extract("(?<=\\| ).*") %>% 
        str_extract(".*(?= - )")
    
    tables <- htmlPage %>% 
        html_nodes("table")
    meets <- grep("\\n\\n\\t", tables, perl = TRUE)
    meetText <- tables[meets] %>% html_text
    res <- allMeetRes(meetText, sex)
    return(bind_cols(name = athleteName, sex = sex, res))
}
