library(rvest)
library(stringr)

one_athlete <- read_html("https://www.tfrrs.org/athletes/6972495/Michigan/Ayden_Owens")
one_athlete

tables <- one_athlete %>% 
    html_nodes("table")
# for some reason html_nodes doesn't recognize tbody tags

dec <- grep("Dec\\n", tables, perl = TRUE)

dec_res <- tables[dec] %>% html_text()

dec_event_names <- c("100", "LJ", "SP", "HJ", "400", "110H", "DT", "PV", "JT", "1500")

str_extract(dec_res, dec_event_names)

grep("(?<=100)(\\n|\\s)*(?=\\d{1,2}\\.\\d{1,2})", str100, perl = TRUE, value = TRUE)

# this should get all of the newline or whitespaces between an event name and the result
# (?<=100)(\\n|\s)*(?=\d{1,2}\.\d{1,2})


str100 <- "\n\n\n100\n\n\n\n\n\n\n\n10.43\n"
idx <- which(!is.na(str_extract(str100, dec_event_names)))

# get newlines and whitespeace between the 100 and the event value
get_nl_ws <- str_extract(str100, "(?<=100)(\\n|\\s)*(?=\\d{1,2}\\.\\d{1,2})")

str_extract(str100, "(?<=\\n+)(\\d{1,2}\\.\\d{1,2})")
    
str_extract(dec_res, "(\\d{1,2}\\.\\d{1,2})")



grep("(?<=100)\\n*", dec_res[1], value = TRUE)

dec_res <- "\nDec\n\n\n\n\n\n\n8238\n\n\n\n \n                                    \n\n1st \n\n\n100\n\n\n\n\n\n\n\n10.40\n (1.5) \n\n\n\n1st\n\n\nLJ\n\n\n\n\n\n\n\n7.47m\n (1.4) \n \n\n24' 6.25\"\n\n\n\n\n3rd\n\n\nSP\n\n\n\n\n\n\n\n14.41m\n \n \n\n47' 3.5\"\n\n\n\n\n2nd\n\n\nHJ\n\n\n\n\n\n\n\n1.85m\n \n \n\n6' 0.75\"\n\n\n\n\n9th\n\n\n400\n\n\n\n\n\n\n\n46.78\n \n\n\n\n1st\n\n\n110H\n\n\n\n\n\n\n\n13.98\n (1.9) \n\n\n\n1st\n\n\nDT\n\n\n\n\n\n\n\n39.91m\n \n \n\n130' 11\"\n\n\n\n\n8th\n\n\nPV\n\n\n\n\n\n\n\n4.70m\n \n \n\n15' 5\"\n\n\n\n\n4th\n\n\nJT\n\n\n\n\n\n\n\n55.71m\n \n \n\n182' 9\"\n\n\n\n\n5th\n\n\n1500\n\n\n\n\n\n\n\n4:23.57\n \n\n\n\n1st\n\n"

str_extract(dec_res, "(?<=100)(.+)")

grep("Dec\n", tables[dec], perl = TRUE, value = TRUE)

str_extract_all(tables[dec], "\\d\\d.\\d")

# should we remove all the newwline characters?

# create regex that finds the names of the events
# create regex that recognizes event scores. This will be in the following formats:
# \d\d.\d\d
# \d.\d\dm
# \d\d.\d\dm
# \d:\d\d.\d\d

# somehow we'll need to find an event name, and then find the subsequent score


