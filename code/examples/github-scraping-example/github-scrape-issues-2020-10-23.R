##################################################
### Load libraries
##################################################

library(tidyverse)
library(purrr)
library(rvest)



##################################################
### Create the Git Session
##################################################

#Enter your user info in the Console

gituser = "zief0002"
userpw = "Shedster1234!"
  
# Authenticate the session. You will have to be signed in to github via a web browser.  
session = html_session("https://github.com/login")

login = session %>% 
  html_node("form") %>% 
  html_form() %>% 
  set_values(login = gituser, password = userpw)

github = session %>% 
  submit_form(login, submit = "commit") %>% 
  read_html()
  


##################################################
### Create a roster of project users and URLs
##################################################

# Import from projectroster.csv file, and add URL
roster = read_csv(file = "/Volumes/GoogleDrive/Shared drives/DSC-WAV/Evaluation/githubscraping/projectroster.csv") %>%
  filter(year == 2020) %>%
  mutate(
    url2 = paste0(url, "/issues")
  )


    


##################################################
### FOR loop to grab commits
##################################################

# Create empty list to store results
n_repos = length(unique(roster$repo))
github_data = vector("list", n_repos)
issue_urls = unique(roster$url2)

# Loop
for(i in 1:n_repos){
  
  # Scrape commit dates
  issues = session %>%
    jump_to(issue_urls[i]) %>% 
    read_html() %>%
    html_nodes("div.js-navigation-container") %>%
    html_nodes("a") %>%
    html_text()
    
  issues = issues[str_detect(issues, pattern = "\\n", negate = TRUE)]
  issues = issues[str_detect(issues, pattern = "\\w")]  
  
  date = session %>%
    jump_to(issue_urls[i]) %>% 
    read_html() %>%
    html_nodes("relative-time") %>%
    html_text()
  
  
  
  
  
  # Create data frame of commits and dates
  my_issues = data.frame(
    issue = issues[seq(1, length(issues), 2)],
    user = issues[seq(1, length(issues), 2) + 1],
    date = date
  ) %>%
    mutate(
      repo = unique(roster$repo)[i]
    ) 
  
  # Output data frame
  github_data[[i]] = my_issues
}

open_issues = do.call(rbind, github_data)



  
# BELLS AND WHISLTES
  
  
### examples for a specific commit -> 
commit.title.grab <- function(url, session) {
  session %>%
    jump_to(commitlist[7,3]) %>% ### change back to function input
    read_html() %>%
    html_nodes("p.commit-title") %>% 
    html_text() 
}

note.grab <- function(url, session) {
  session %>%
    jump_to(commitlist[7,3]) %>% ### change back to function input
    read_html() %>%
    html_nodes("pre") %>%
    html_text() 
}

date.grab <- function(url, session) {
  session %>%
    jump_to(commitlist[7,3]) %>%
    read_html() %>%
    html_nodes("relative-time") %>% html_text() %>%
    parse_date(format="%b %d, %Y")
}

additions.grab <- function(url, session) {
  session %>%
    jump_to(commitlist[7,3]) %>%
    read_html() %>%
    html_nodes(xpath="/html/body/div[4]/div/main/div[2]/div/div[2]/div[2]/strong[1]") %>%
    html_text() %>%
    parse_number()
}

deletions.grab <- function(url, session) {
  session %>%
    jump_to(commitlist[7,3]) %>%
    read_html() %>%
    html_nodes(xpath="/html/body/div[4]/div/main/div[2]/div/div[2]/div[2]/strong[2]") %>%
    html_text() %>%
    parse_number()
}



##################################################
### Plot
##################################################

final_data = final_data %>%
  mutate(date = as.Date(date, format = "%b %d, %Y"))

commit_counts = final_data %>%
  group_by(date, user) %>%
  summarize(N = n())

p1 = final_data %>%
  filter(date > "2020-08-23") %>%
ggplot(data = , aes(x = date, fill = user)) +
  geom_dotplot(pch = 21, color = "black", method = "histodot", dotsize = 1) + 
  geom_vline(xintercept = as.Date("2020-09-18", "2020-10-16")) +
  #scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d\n(%a)") +
  scale_x_date(
    name = "",
    breaks = as.Date(c("2020-08-24", "2020-09-18", "2020-10-16", "2020-11-06", "2020-11-20")), #"1 week", 
    labels = c("Aug\n24", "Sept\n18", "Oct\n16", "Nov\n06", "Nov\n20"),
    limits = as.Date(c("2020-08-24", "2020-11-20"))
    #date_minor_breaks = "1 week"
    ) +
  scale_y_continuous(name = " ", breaks = NULL) +
  ggsci::scale_fill_ucscgb(name = "") +
  theme_bw() +
  facet_grid(user ~ repo) +
  guides(fill = FALSE) +
  theme(
    #axis.text.x = element_text(angle = 60, hjust = 1),
    panel.grid.minor.x = element_blank()
  )


ggsave(p1, filename = "~/Desktop/valley-bikes.png", width = 20, height = 13)

, date_labels = "%W"

