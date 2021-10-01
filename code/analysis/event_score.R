######################################################################################################
###                                                                                                ### 
###                                 event_score() function                                         ###
###                                                                                                ### 
###                                 By: Corissa T. Rohloff                                         ### 
###                                 Date Created: 2021-09-02                                       ### 
###                                 Date Last Modified: 2021-09-03                                 ### 
###                                                                                                ### 
######################################################################################################

##### Load Packages ##### 
library(tidyverse)

##### Function Description ##### 
# Used to calculate the number of points earned for a T&F multi event.
## Scoring methods are based on IAAF rules (2001).

event_score <- function(result, event, gender) {
  # browser()
  result <- as.numeric(result)
  event <- as.character(event) # force this to all lowercase?
  gender <- as.character(gender)
  ### Parameters of function
  # result (numeric): speed (in seconds), height (in centimeters), distance (in meters)
  # event (character): event name, accepts: ""60", "60H", "100", "100H", "110H", "200", "400", "800", "1000", "1500", "HJ", "PV", "LJ", "SP", "DT", "JT"
  # gender (character): "m" or "w" 
  
  ##### Should all of these "static" parameters be put into the global environment
  ##### instead of getting loaded every single time the function is called?
  
  ## parameter values from IAAF Scoring Tables for Combined Events.pdf (in GitHub)
  mens_parameters <- 
    data.frame(
      events = c("100", "200", "400", "1500", "110H", "HJ", "PV", "LJ", "SP", "DT", "JT", "60", "1000", "60H"),
      a = c(25.4347, 5.8425, 1.53775, 0.03768, 5.74352, 0.8465, 0.2797, 0.14354, 51.39, 12.91, 10.14, 58.0150, 0.08713, 20.5173), 
      b = c(18, 38, 82, 480, 28.5, 75, 100, 220, 1.5, 4, 7, 11.5, 305.5, 15.5),
      c = c(1.81, 1.81, 1.81, 1.85, 1.92, 1.42, 1.35, 1.4, 1.05, 1.1, 1.08, 1.81, 1.85, 1.92)
    )
  womens_parameters <- 
    data.frame(
      events = c("200", "800", "100H", "HJ", "LJ", "SP", "JT", "100", "400", "1500", "PV", "DT", "60H"),
      a = c(4.99087, 0.11193, 9.23076, 1.84523, 0.188807, 56.0211, 15.9803, 17.8570, 1.34285, 0.02883, 0.44125, 12.3311, 20.0479), 
      b = c(42.5, 254, 26.7, 75, 210, 1.5, 3.8, 21, 91.7, 535, 100, 3, 17),
      c = c(1.81, 1.88, 1.835, 1.348, 1.41, 1.05, 1.04, 1.81, 1.81, 1.88, 1.35, 1.1, 1.835)
    )

  field_events <- c("DT", "HJ", "JT", "LJ", "PV", "SP")
  track_events <- c("60", "60H", "100", "200", "400", "1500", "110H", "1000", "800", "100H")
  
  jumps <- c("HJ", "LJ", "PV")
  throws <- c("DT", "JT", "SP")
  
  score_parameters <- list(mens_parameters, womens_parameters)
  names(score_parameters) <- c("m", "w")
  
  ## pulling scoring parameters based on gender and event
  a <- score_parameters[[gender]] %>% filter(events == event) %>% pull(a)
  b <- score_parameters[[gender]] %>% filter(events == event) %>% pull(b)
  c <- score_parameters[[gender]] %>% filter(events == event) %>% pull(c)
  
  ## Used to determine event type, which effects the equation used to calculate points
  type <- ifelse(event %in% field_events, "field", # jumps and throws
                 ifelse(event %in% track_events, "track", "no event type"))
  
  
  
  # if the gender and event combination does not appear in the IAAF table, 
  ## the above code will produce numeric(0).
  ### This if else statement will recognize this issue and will not calculate a score.
  if (length(a) == 0 | length(b) == 0 | length(c) == 0) {
    return(NA)
  }
  
  # Calculating event points
  if (type == "track") {
    score <- floor(a*(b - result)^c)
    return(score) # track events (faster time results in more points)
  } else if (type == "field") {
    if (event %in% jumps) {
      result <- result * 100 # jumps are converted to centimeters
    }
    score <- floor(a*(result - b)^c)
    return(score) # field events (longer distance/height results in more points)
  } else {
    return("Error")
  }
}


##### Session Info ##### 
# Last Modified Under:
# sessionInfo()
# R version 4.1.1 (2021-08-10)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Big Sur 11.5.2
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] lubridate_1.7.10 forcats_0.5.1    stringr_1.4.0    dplyr_1.0.7      purrr_0.3.4      readr_2.0.1     
# [7] tidyr_1.1.3      tibble_3.1.3     ggplot2_3.3.5    tidyverse_1.3.1 
# 
# loaded via a namespace (and not attached):
# [1] tidyselect_1.1.1  haven_2.4.3       colorspace_2.0-2  vctrs_0.3.8       generics_0.1.0   
# [6] utf8_1.2.2        rlang_0.4.11      pillar_1.6.2      glue_1.4.2        withr_2.4.2      
# [11] DBI_1.1.1         bit64_4.0.5       dbplyr_2.1.1      modelr_0.1.8      readxl_1.3.1     
# [16] lifecycle_1.0.0   munsell_0.5.0     gtable_0.3.0      cellranger_1.1.0  rvest_1.0.1      
# [21] labeling_0.4.2    tzdb_0.1.2        parallel_4.1.1    fansi_0.5.0       broom_0.7.9      
# [26] Rcpp_1.0.7        scales_1.1.1      backports_1.2.1   vroom_1.5.4       jsonlite_1.7.2   
# [31] farver_2.1.0      fs_1.5.0          bit_4.0.4         hms_1.1.0         digest_0.6.27    
# [36] stringi_1.7.3     grid_4.1.1        cli_3.0.1         tools_4.1.1       magrittr_2.0.1   
# [41] crayon_1.4.1      pkgconfig_2.0.3   ellipsis_0.3.2    data.table_1.14.0 xml2_1.3.2       
# [46] reprex_2.0.1      assertthat_0.2.1  httr_1.4.2        rstudioapi_0.13   R6_2.5.1         
# [51] compiler_4.1.1  
