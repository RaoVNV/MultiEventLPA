# Regular expressions for use in scripts
# I deleted all the "m"'sfrom the jumps, throws, etc. We can add those back later if it's helpful

# put relays first

reDates <- c("[a-zA-Z]{3}\\s{1,2}\\d{1,2}-\\d{1,2},\\s\\d{4}",
             "[a-zA-Z]{3}\\s{1,2}\\d{1,2},\\s\\d{4}",
             "[a-zA-Z]{3}\\s{1,2}\\d{2}\\s-\\s[a-zA-Z]{3}\\s{2}\\d{1},\\s\\d{4}")
reDates <- paste0(reDates[1], "|", reDates[2], "|", reDates[3])

decScoresRe <- c(
    re100 = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    reLJ = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    reSP = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    reHJ = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    re400 = "(?<=\\s{0,1000}+)(\\d:){0,1}(\\d{2}\\.\\d{1,2})",
    re110H = "(?<=\\s{0,1000}+)(\\d{2}\\.\\d{1,2})",
    reDT = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    rePV = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    reJT = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    re1500 = "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})"
)

decEventNames <- c("100", "LJ", "SP", "HJ", "400", "110H", "DT", "PV", "JT", "1500")
decNamesRe <- paste0("(?s)(?<=", decEventNames,"\\n).*")
names(decNamesRe) <- names(decScoresRe)

hepScoresMRe <- c(
    re1000 = "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})",
    re55 = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    re60 = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    reLJ = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    reSP = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    reHJ = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    re55H = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    re60H = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    rePV = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})"
)

hepEventMNames <- c("1000", "55", "60", "LJ", "SP", "HJ", "55H", "60H", "PV")
hepNamesMRe <- paste0("(?s)(?<=", hepEventMNames,"\\n).*")
names(hepNamesMRe) <- names(hepScoresMRe)

hepScoresWRe <- c(
    re100H = "(?<=\\s{0,1000}+)(\\d{2}\\.\\d{1,2})",
    reHJ = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    reSP = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    re200 = "(?<=\\s{0,1000}+)(\\d{2}\\.\\d{1,2})",
    reLJ = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    reJT = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    re800 = "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})"
)

hepEventWNames <- c("100H", "HJ", "SP", "200", "LJ", "JT", "800")
hepNamesWRe <- paste0("(?s)(?<=", hepEventWNames,"\\n).*")
names(hepNamesWRe) <- names(hepScoresWRe)

pentScoresRe <- c(
    re60H = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    reHJ = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    reSP = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    reLJ = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    re800 = "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})"
)

pentEventNames <- c("60H", "HJ", "SP", "LJ", "800")
pentNamesRe <- paste0("(?s)(?<=", pentEventNames,"\\n).*")
names(pentNamesRe) <- names(pentScoresRe)


# probs need to repeat names(namesRe) thing from above
# there's some issue when trying to add the 600 and 4x200

indoorEventNames <- c("60", "200", "400", "600", "800", "Mile", "3000", "5000", "60H", "4x200",
                      "4x400", "DMR", "HJ", "PV", "LJ", "TJ", "SP", "WT", "Pent", "Hep")
outdoorEventNames <- c("100", "200", "400", "800", "1500", "5000", "10000",
                       "100H", "110H", "400H","3000S", "4x100", "4x400",
                       "HJ", "PV", "LJ", "TJ", "SP", "DT", "JT", "Hep", "Dec")
indoorOnlyEvents <- !(indoorEventNames %in% outdoorEventNames)
indoorEventNames[indoorOnlyEvents]
allEventNames <- c(outdoorEventNames, indoorEventNames[indoorOnlyEvents])

indoorScoresRe <- c(
    re60 = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    re200 = "(?<=\\s{0,1000}+)(\\d{2}\\.\\d{1,2})",
    re400 = "(?<=\\s{0,1000}+)(\\d:){0,1}(\\d{2}\\.\\d{1,2})",
    re600 = "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})",
    re800 = "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})",
    reMile = "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})",
    re3000 = "(?<=\\s{0,1000}+)((\\d:){1}|(\\d{2}:))(\\d{2}\\.\\d{1,2})",
    re5000 = "(?<=\\s{0,1000}+)(\\d{2}:)(\\d{2}\\.\\d{1,2})",
    re60H = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    re4x200 = "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})",
    re4x400 = "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})",
    reDMR = "(?<=\\s{0,1000}+)(\\d{1,2}:)(\\d{2}\\.\\d{1,2})",
    reHJ = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    rePV = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    reLJ = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    reTJ = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    reSP = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    reWT =  "(?<=\\s{0,1000}+)(\\d{2}\\.\\d{1,2})",
    rePent = "(?<=\\s{0,1000}+)(\\d{1,4})",
    reHep = "(?<=\\s{0,1000}+)(\\d{1,4})"
)

outdoorScoresRe <- c(
    re100 = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    re200 = "(?<=\\s{0,1000}+)(\\d{2}\\.\\d{1,2})",
    re400 = "(?<=\\s{0,1000}+)(\\d:){0,1}(\\d{2}\\.\\d{1,2})",
    re800 = "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})",
    re1500 = "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})",
    re5000 = "(?<=\\s{0,1000}+)(\\d{2}:)(\\d{2}\\.\\d{1,2})",
    re10000 = "(?<=\\s{0,1000}+)(\\d{2}:)(\\d{2}\\.\\d{1,2})",
    re100H = "(?<=\\s{0,1000}+)(\\d{2}\\.\\d{1,2})",
    re110H = "(?<=\\s{0,1000}+)(\\d{2}\\.\\d{1,2})",
    re400H = "(?<=\\s{0,1000}+)(\\d:){0,1}(\\d{2}\\.\\d{1,2})",
    re3000S = "(?<=\\s{0,1000}+)((\\d:){1}|(\\d{2}:))(\\d{2}\\.\\d{1,2})",
    re4x100 = "(?<=\\s{0,1000}+)(\\d:){0,1}(\\d{2}\\.\\d{1,2})",
    re4x400 = "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})",
    reHJ = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    rePV = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    reLJ = "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})",
    reTJ = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    reSP = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    reDT = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    reJT = "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})",
    reHep = "(?<=\\s{0,1000}+)(\\d{1,4})",
    reDec = "(?<=\\s{0,1000}+)(\\d{2,4})"
)

indoorOnlyScoresRe <- !(names(indoorScoresRe) %in% names(outdoorScoresRe))
indoorScoresRe[indoorOnlyScoresRe]
allScoresRe <- c(outdoorScoresRe, indoorScoresRe[indoorOnlyScoresRe])
eventNamesRe <- paste0("(?s)(?<=\\n", allEventNames,"\\n).*")
names(eventNamesRe) <- names(allScoresRe)
