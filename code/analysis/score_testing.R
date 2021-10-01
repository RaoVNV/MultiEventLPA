library(tidyverse)
source("code/analysis/event_score.R")
athleteRes <- read_csv("data/raw/athlete_results.csv")
small <- athleteRes[1:10,]

kw2 <- small[2,]
event_score(kw2$result, kw2$event, kw2$sex)

event_score(, "HJ", "w")

scoring <- small %>% 
    select(result, event, sex) %>% 
    rename(gender = sex)

pmap_dbl(.l = scoring, .f = event_score) %>% 
    as_tibble()
