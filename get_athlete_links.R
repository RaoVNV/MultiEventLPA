library(rvest)
library(httr)
library(tidyverse)
library(progress)

source("get_link_funcs.R")

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

meetLinks <- read_csv("meetLinks.csv")

# this is everything all at once and it'll probs crash
# athletes <- getAthleteLinks(meetLinks)

meetDivisions <- divisions(length(meetLinks), 40)
athleteLinksList <- vector(mode = "list", length = length(meetDivisions))

# rerun now that all the errors *should* be fixed
for(i in 1:length(athleteLinksList)) {
    athleteLinksList[[i]] <- getAthleteLinks(meetLinks[meetDivisions[[i]]])
    print(i)
}

athleteLinks <- bind_rows(athleteLinksList)
# this should be mapped in the future to improve performance
# profvis({
#     als <- map_dfr(small, getAthleteLinks)
# })


# write_csv(athleteLinks, "athleteLinks31-40.csv")

athleteLinks1_10 <- read_csv("athleteLinks1-10.csv")
athleteLinks10_30 <- read_csv("athleteLinks10-30.csv")
athleteLinks31_40 <- read_csv("athleteLinks31-40.csv")
athleteLinks <- bind_rows(athleteLinks1_10,
                          athleteLinks10_30,
                          athleteLinks31_40) %>%
    mutate(sex = as.factor(sex))

write_csv(athleteLinks, "athleteLinks.csv")
