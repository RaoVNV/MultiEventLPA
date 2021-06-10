library(rvest)

one_athlete <- read_html("https://www.tfrrs.org/athletes/6972495/Michigan/Ayden_Owens")
one_athlete

tables <- one_athlete %>% 
    html_nodes("table")
# for some reason html_nodes doesn't recognize tbody tags

dec <- grep("Dec\\n", tables, perl = TRUE)

tables[dec] %>% html_text()

# create regex that finds the names of the events
# create regex that recognizes event scores. This will be in the following formats:
# \d\d.\d\d
# \d.\d\dm
# \d\d.\d\dm
# \d:\d\d.\d\d

# somehow we'll need to find an event name, and then find the subsequent score


