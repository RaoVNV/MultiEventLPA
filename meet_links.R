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

# rerun now that all the errors *should* be fixed
for(i in 1:length(athleteLinksList)) {
    athleteLinksList[[i]] <- getAthleteLinks(meetLinks[meetDivisions[[i]]])
    print(i)
}

# this should be mapped in the future to improve performance
profvis({
    als <- map_dfr(small, getAthleteLinks)
})


athleteLinks <- bind_rows(athleteLinksList)
# write_csv(athleteLinks, "athleteLinks31-40.csv")

athleteLinks1_10 <- read_csv("athleteLinks1-10.csv")
athleteLinks10_30 <- read_csv("athleteLinks10-30.csv")
athleteLinks31_40 <- read_csv("athleteLinks31-40.csv")
athleteLinks <- bind_rows(athleteLinks1_10,
                          athleteLinks10_30,
                          athleteLinks31_40) %>% 
    mutate(sex = as.factor(sex))

# write_csv(athleteLinks, "athleteLinks.csv")

athleteDivisions <- divisions(nrow(athleteLinks), 50)
resList <- vector(mode = "list", length = length(athleteDivisions))

# this one is gonna take a while
# currently made it through 3
for(i in 3:length(resList)) {
    resList[[i]] <- getMultiRes(athleteLinks[athleteDivisions[[i]],])
    print(i)
}

athleteRes <- bind_rows(resList)

write_csv(athleteRes, "athlete_results.csv")

