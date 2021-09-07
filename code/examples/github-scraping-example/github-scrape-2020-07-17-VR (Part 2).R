head(commitlist)

c.title=c.notes=c.date=c.add=c.del={}

for (i in 1:dim(commitlist)[1]){
  commit.html <- session %>% jump_to(commitlist[i,3]) %>% read_html() 

  title.x <- commit.html %>% html_nodes("p.commit-title") %>% html_text() %>% gsub("^\\s+|\\s+$", "", .)
  if(length(title.x)>0){c.title[i]=title.x}else{c.title[i]=""}
  
  notes.x <- commit.html %>% html_nodes("pre") %>% html_text() %>% gsub("^\\s+|\\s+$", "", .)
  if(length(notes.x)>0){c.notes[i]=notes.x}else{c.notes[i]=""}
     
  date.x  <- commit.html %>% html_nodes("relative-time") %>% html_text() # Note, date format is "%b %d, %Y"
  if(length(date.x)>0){c.date[i]=date.x}else{c.date[i]=""}
  
  add.x   <- commit.html %>% html_nodes(xpath="/html/body/div[4]/div/main/div[2]/div/div[2]/div[2]/strong[1]") %>% html_text() %>% parse_number()
  if(length(add.x)>0){c.add[i]=add.x}else{c.add[i]=""}
    
  del.x   <- commit.html %>% html_nodes(xpath="/html/body/div[4]/div/main/div[2]/div/div[2]/div[2]/strong[2]") %>% html_text() %>% parse_number()
  if(length(del.x)>0){c.del[i]=del.x}else{c.del[i]=""}
}



commit.info = cbind(commitlist,c.title, c.notes, c.date, c.add, c.del)
head(commit.info)


write.csv(commit.info,"G:\\Shared drives\\DSC-WAV\\Evaluation\\githubscraping\\DSCWAVcommitinfo.csv", row.names = FALSE)
