# Functions to get multi-event results

getEventScore <- function(x) {
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
    return(newRow)
}


getPentScores <- function(x) {
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
    return(pentResThisMeet)
}


