######################################################################################################
###                                                                                                ### 
###                                 T & F Multis Data Exploration                                  ###
###                                                                                                ### 
###                                 By: Corissa T. Rohloff                                         ### 
###                                 Date Created: 2021-09-01                                       ### 
###                                 Date Last Modified: 2021-09-03                                 ### 
###                                                                                                ### 
######################################################################################################

##### Clear Current Environment #####
rm(list=ls())


##### Load Packages ##### 
library(tidyverse)
library(lubridate)

source(file = "code/analysis/event_score.R") # function to calculate event points


##### Load in Data ##### 
athlete_results <- read_csv("data/raw/athlete_results.csv")

# variables
unique(athlete_results$sex)
unique(athlete_results$event)
unique(athlete_results$multi)


##### Data Cleaning ##### 
##### Setting Dates for Graphing Longitudinally ##### 
# Finding meets that go across 2 years
athlete_results %>% 
  filter(str_detect(date, "Dec 31"))

# Changing format to include year of Dec 31
athlete_results <- 
  athlete_results %>% 
  mutate(date = replace(date, str_detect(date, "Dec 31"), "Dec 31, 2013 - Jan 3, 2014")) 

# Creating Variables to have modified start and end month, day, and year
modified_dates <- 
  str_split_fixed(athlete_results$date, "[-, ]+", n = 6) %>% 
  cbind(athlete_results$name) %>% 
  as.data.frame() %>% 
  mutate(
    start_month = V1,
    start_day = V2,
    start_year = ifelse(V6 == "" & V5 == "" & V4 == "", V3,
                        ifelse(V6 == "" & V5 == "", V4,
                               ifelse(V6 == "", V5, V3))
    ),
    end_month = ifelse(V6 == "" & V5 == "" & V4 == "", NA,
                       ifelse(V6 == "" & V5 == "", V1,
                              ifelse(V6 == "", V3, V4))
    ),
    end_day = ifelse(V6 == "" & V5 == "" & V4 == "", NA,
                     ifelse(V6 == "" & V5 == "", V3,
                            ifelse(V6 == "", V4, V5))
    ),
    end_year = ifelse(V6 == "" & V5 == "" & V4 == "", NA,
                      ifelse(V6 == "" & V5 == "", V4,
                             ifelse(V6 == "", V5, V6))
    )
  ) 

# combining with original dataset and creating Date format
athlete_results_2 <- cbind(athlete_results, modified_dates %>% select(start_month:end_year)) %>% 
  mutate(
    start_date_char = paste(start_month, start_day, start_year, sep = " "),
    start_date = as.Date(start_date_char,
                         format = "%B %d %Y")) %>% 
  relocate(start_date, .after = multi) %>% 
  select(name:start_date)

##### Translating results to the appropriate unit ##### 
# speed (seconds), height (centimeters), distance (meters)

athlete_results_3 <- athlete_results_2 %>% 
  # translating M:S.MS to S.MS for running events
  mutate(
    results_modified = ifelse(str_detect(result, ":"), 
                              (as.numeric(as.POSIXct(strptime(result, format = "%M:%OS"))) - 
                                 as.numeric(as.POSIXct(strptime("0", format = "%S")))),
                              result) %>% as.numeric()
  ) %>% 
  # translating M to CM for jumping events
  rowwise() %>% 
  mutate(
    results_modified = ifelse(event %in% c("HJ", "LJ", "PV"),
                              (results_modified*100),
                              results_modified)
  )

##### Scoring Events #####
# this runs appropriately
athlete_results_3 %>% 
  head(n = 25) %>% 
  rowwise() %>% 
  mutate(
    points = event_score(result = results_modified, event = event, gender = sex)
  )

# Another function for calculating points
library(combinedevents) # this package/functions uses slightly different units
scores(marks = c(9.53, 1.59, 9.44, 4.85, "2:27.35"), gender = "female", combined_event = "pentathlon", seconds = TRUE) 
## Results match the points from Kayla Wilson in the above output (off byslight rounding)

# this takes a long time
athlete_results_4 <- athlete_results_3 %>% 
  rowwise() %>% 
  mutate(
    points = event_score(result = results_modified, event = event, gender = sex)
  )

# athlete_res_scored <- write_csv(athlete_results_4,
#                                 file = "data/processed/athlete_res_scored")

##### Write Out Files ##### 
# write_csv(athlete_results_4, file = "../Data/athlete_results_analytical.csv", row.names = F)


##### Graphing #####
# Playing around

# graphing men, dec, 100m results
athlete_results_2 %>% 
  filter(sex == "m" & multi == "dec" & event == "100") %>% 
  head(n = 1000) %>% 
  ggplot(aes(y = as.numeric(result), x = start_date, group = name)) +
  geom_line() 

# some results are very large for the 100m, times look more like 400m relay or 400m
athlete_results_2 %>% 
  filter(sex == "m" & multi == "dec" & event == "100") %>% 
  head(n = 1000) %>% 
  filter(as.numeric(result) > 40)

# filtering out large values
athlete_results_2 %>% 
  filter(sex == "m" & multi == "dec" & event == "100") %>% 
  head(n = 1000) %>% 
  filter(as.numeric(result) < 40) %>% 
  ggplot(aes(y = as.numeric(result), x = start_date, group = name)) +
  geom_line() +
  ylim(10,14)







