library(rvest)
library(httr)
library(tidyverse)
library(progress)

source("code/scraping/scraping_funcs/regex_list.R")
source("code/scraping/scraping_funcs/get_event_funcs.R")
source("code/scraping/scraping_funcs/get_link_funcs.R")
# source("meets_one_athlete.R")

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

athleteLinks <- read_csv("data/athleteLinks.csv", col_types = c("cf"))
athleteLinks <- unique(athleteLinks) # this should be updated later when creatin athleteLinks

athleteDivisions <- divisions(nrow(athleteLinks), 50)
resList <- vector(mode = "list", length = length(athleteDivisions))

# this one is gonna take a while
# currently made it through 6
for(i in 1:length(resList)) {
    resList[[i]] <- getMultiRes(athleteLinks[athleteDivisions[[i]],])
    print(i)
}

athleteRes <- bind_rows(resList)

write_csv(athleteRes, "data/athlete_results.csv")
