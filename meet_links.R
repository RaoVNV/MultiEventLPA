library(rvest)
library(httr)
library(tidyverse)
library(progress)

# source("meets_one_athlete.R")
source("regex_list.R")
source("get_link_funcs.R")
source("get_event_funcs.R")

years <- 2009:2021
meetLinks <- getMeetLinks(years)

small <- meetLinks[1:500]

athleteLinks <- getAthleteLinks(small)

# getMultiRes(athleteLinks[78,]) debug this
