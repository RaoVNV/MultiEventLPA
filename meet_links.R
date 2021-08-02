library(rvest)
library(httr)
library(tidyverse)
library(progress)

# source("meets_one_athlete.R")
source("regex_list.R")
source("get_link_funcs.R")
source("get_event_funcs.R")

# divisions helps split up the indicies of the meets. This is part of what allows
# you to save your progress as you go
divisions <- function(dividend, divisor) {
    remainder <- dividend %% divisor
    dividend %/% divisor
    parts <- c(rep(dividend %/% divisor + 1, times = remainder),
               rep(dividend %/% divisor, times = divisor - remainder))
    sections <- vector(mode = "list", length(parts))
    for(i in 1:length(sections)) {
        if(i == 1) {
            sections[[i]] <- 1:parts[i]
        } else {
            sections[[i]] <- (sections[[i-1]][length(sections[[i-1]])]+1):
                (sections[[i-1]][length(sections[[i-1]])]+parts[i])
        }
        
    }
    return(sections)
}

years <- 2009:2021
meetLinks <- getMeetLinks(years)

# athletes <- getAthleteLinks(meetLinks)


meetDivisions <- divisions(length(meetLinks), 40)
athleteLinksList <- vector(mode = "list", length = length(meetDivisions))

# for(i in 1:length(athleteLinksList)) {
#     athleteLinksList[[i]] <- getAthleteLinks(meetLinks[meetDivisions[[i]]])
# }

for(i in 1:length(athleteLinksList)) {
    athleteLinksList[[i]] <- getAthleteLinks(meetLinks[meetDivisions[[i]]])
    print(i)
}

athleteLinks <- bind_rows(athleteLinksList)
# write_csv(athleteLinks, "athleteLinks31-40.csv")

athleteDivisions <- divisions(length(athleteLinksList), 40)
resList <- vector(mode = "list", length = length(sections))

for(i in 1:length(resList)) {
    resList[[i]] <- getMultiRes(athleteLinks[sections[[i]],])
}
res <- getMultiRes(athleteLinks[sections[[1]],])



