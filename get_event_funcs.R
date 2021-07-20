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

detectMulti <- function(x) {
    # browser()
    pent <- str_which(x, "Pent\\n")
    hep <- str_which(x, "Hep\\n")
    dec <- str_which(x, "Dec\\n")
    
    if(any(length(c(pent, hep, dec) > 0))) {
        idx <- which(map(list(pent, hep, dec), vectorIsNotEmpty) == TRUE)
        res <- c(pent, hep, dec)[idx]
        multiEventNames <- c("pent", "hep", "dec")
        names(res) <- multiEventNames[idx]
        return(res)
    }
}

getMultiScores <- function(x, event) {
    # browser()
    multiScores <- character()
    multi <- match.arg(names(event), choices = c("pent", "hep", "dec"))
    switch(multi, pent = getPentScores(x),
           hep = getHepScores(x),
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


getHepScores <- function(x) { # requires full text from each meet, not textAfterMeet
    # browser()
    hepScores = character()
    eventNames <- hepEventMNames
    # there's a problem if the athlete ran a 55 or 55H instead of a 60 or 60H
    for (hepEventM in hepEventMNames) {
        hepEventMRe <- hepNamesMRe[str_which(names(hepNamesMRe), paste0(hepEventM,"$"))]
        textAfterEvent <- str_extract(x, hepEventMRe)
        if(is.na(textAfterEvent)) { # if true, this is a female athlete
            for (hepEventW in hepEventWNames) {
                hepEventWRe <- hepNamesWRe[str_which(hepNamesWRe, hepEventW)]
                textAfterEvent <- str_extract(x, hepEventWRe)
                hepScores[hepEventW] <- str_extract(textAfterEvent,
                                                    hepScoresWRe[names(hepEventWRe)])
            }
            eventNames <- hepEventWNames
            break
        } else {
            hepScores[hepEventM] <- str_extract(textAfterEvent,
                                                 hepScoresMRe[names(hepEventMRe)])
        }
    }
    
    meetName <- str_extract(x, "(?<=\\n\\n\\t).*")
    meetDate <- str_extract(x, reDates)
    
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

allMeetRes <- function(x) { # this code only gets finals, not prelims. Fix later?
    # browser()
    res <- tibble()
    for(meet in x) { # outer for loop goes through each meet
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
                newRow <- getEventScore(textAfterEvent,
                                        eventNamesRe,
                                        event,
                                        meetName,
                                        meetDate)
                res <- bind_rows(res, newRow)
            }
        }
    }
    return(res)
}
