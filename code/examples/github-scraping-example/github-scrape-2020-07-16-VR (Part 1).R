library(tidyverse)
library(purrr)
library(rvest)

## Create the Git Session
  #Enter your user info in the Console
  gituser <- "Enter your Github Username: "
  userpw <- "Enter your Github Password: "
  
  gituser="RaoVNV"
  userpw="Vman2008!!"
  
  ### Turns out you actually have to be signed in to git via a webbrowser. If not, git wont authenticate.
  
  session <- html_session("https://github.com/login")
  login <- session %>% html_node("form") %>% html_form() %>% set_values(login = gituser, password = userpw)
  github <- session %>% submit_form(login, submit = "commit") %>% read_html()
  

## Create "roster", a dataframe with project, user, and URLs
  # one link per person per project

  # pull in the projectroster.csv file
  # file path: Shared drives\DSC-WAV\Evaluation\githubscraping
    csv.roster=read.csv(file="G:\\Shared drives\\DSC-WAV\\Evaluation\\githubscraping\\projectroster.csv")
  # rename vars because for some stupid reason R adds "i..." to my varnames.
    names(csv.roster) = c("project", "username")

  # create links for each person's commits for their project
    roster = csv.roster %>%
      mutate(url = 
        paste(
          "https://github.com/DSC-WAV/proj-", 
          project, 
          "/commits?author=", 
          username, 
        sep=""))

# Scrape everyone's commmits
## Create a data frame with one url per commit per person per project
  
    
# FANCY VERSION WHICH I CANT GET TO WORK:
  # function to grab the url for each individual commit
  commitlink.grab <- function(url, session=session) {
    session %>%
      jump_to(url) %>% read_html() %>%
      html_nodes("ol") %>% 
        html_nodes("li") %>% 
          html_nodes("a.sha.btn.btn-outline.BtnGroup-item") %>%
          # note, i started clicking inspect, then right click, then copy, then full xpath. That helped. 
            html_attr("href")
  }
  ### I want to apply this function to everyone, and have that full long list with one row per commit. 
  ### Then I can scrape attributes one by one from the commit link itself.
  ### I can't get the map_bdl to work...
    commitlist <- roster %>%
      mutate(commitlinks = map_dbl(url, commitlink.grab, session = session))
    
    
    
# BRUTE FORCE VERSION THAT I GOT TO WORK:
  ### ^ couldn't get the above line to work. Here's my hack -> 
  commitlist=data.frame();
  for (i in 1:dim(roster)[1]){
    commitlink.grab = {} #create a new array to store commit links for each person
    page.to.be.scraped=roster$url[i] #start with the link to the page for commits per person
    first.page.ind=1 #indicator to make sure we're only scraping the "older" links on subsequent pages
      # on the first page of commits, the "newer" button will be depricated. the html object is for both buttons. therefore the a[1] will give the link for the "older" page.
      # if there is a third page, then on the second page, both the "newer" and "older" buttons will be active, thus we want the second link, a[2].
      # if there is no a[2], it means there are not any 'older' pages, and the loop can stop. 
    
    while(length(page.to.be.scraped)==1){
    commit.page.scrape<-  session %>%
      jump_to(page.to.be.scraped) %>%
      read_html() %>%
      html_nodes("ol") %>% 
        html_nodes("li") %>% 
          html_nodes("a.sha.btn.btn-outline.BtnGroup-item") %>%
            html_attr("href")
    commitlink.grab=c(commitlink.grab,commit.page.scrape)
    
    next.page.link <- session %>%
      jump_to(page.to.be.scraped) %>%
      read_html() %>%
      html_nodes(xpath=paste("/html/body/div[4]/div/main/div[2]/div/div[3]/div/a[",2-first.page.ind,"]",sep="")) %>%
      html_attr("href")
    
    page.to.be.scraped=next.page.link
    first.page.ind=0
    }
    
    commitlist=rbind(commitlist, 
      merge(
        roster[i,1:2], 
        paste("https://github.com", commitlink.grab, sep="")
      )
    )
  }
  names(commitlist)[3]="commitlink"
  ### creates a data frame called "commitlist", one record per commit per person per project.
  ### contains project, username, and link to the commit itself.

  #counts by person
  number.of.commits.per.person <- commitlist %>% group_by(project, username) %>% count()
  view(number.of.commits.per.person) 
  
  
  ### For the rest of the code, I have functions that work when you specify the link to grab the following info: 
  ### title, notes, date, additions, deletions. 
  ### They need to be integrated so that they add columns to the commitlist dataframe

  
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




