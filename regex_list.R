# Regular expressions for use in scripts

re100 <- "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})"
reLJ <- "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2})"
reSP <- "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2}m)"
reHJ <- "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2}m)"
re400 <- "(?<=\\s{0,1000}+)(\\d:){0,1}(\\d{2}\\.\\d{1,2})"
re110H <- "(?<=\\s{0,1000}+)(\\d{2}\\.\\d{1,2})"
reDT <- "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2}m)"
rePV <- "(?<=\\s{0,1000}+)(\\d{1}\\.\\d{1,2}m)"
reJT <- "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2}m)"
re1500 <- "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})"

re1000 <- "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})"
re60 <- "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})"
re60H <- "(?<=\\s{0,1000}+)(\\d{1,2}\\.\\d{1,2})"

re200 <- "(?<=\\s{0,1000}+)(\\d{2}\\.\\d{1,2})"
re800 <- "(?<=\\s{0,1000}+)(\\d:){1}(\\d{2}\\.\\d{1,2})"
re100H <- "(?<=\\s{0,1000}+)(\\d{2}\\.\\d{1,2})"


decEventNames <- c("100", "LJ", "SP", "HJ", "400", "110H", "df", "PV", "JT", "1500")
decScoresRe <- c(re100, reLJ, reSP, reHJ, re400, re110H, reDT, rePV, reJT, re1500)

hepEventMNames <- c("1000", "60", "LJ", "SP", "HJ", "60H", "PV")
hepScoresMRe <- c(re1000, re60, reLJ, reSP, reHJ, re60H, rePV)

hepEventWNames <- c("100H", "HJ", "SP", "200", "LJ", "JT", "800")
hepScoresWRe <- c(re100H, reHJ, reSP, re200, reLJ, reJT, re800)

pentEventNames <- c("60H", "HJ", "SP", "LJ", "800")
pentScoresRe <- c(re60H, reHJ, reSP, reLJ, re800)

