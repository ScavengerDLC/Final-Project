library(tidyverse)
library(lubridate)
library(httr)
library(rvest)
library(jsonlite)

######################
# Set your Path Here #
######################

path <- "C:/Users/zades/Documents/GitHub/Final-Project/"

### The purpose of this R file is to get NYTimes article text from articles on 
### Presidential candidates from 1968 to 2024. 


######################
# Step One: Get URLS #
######################
### Create List of Presidential Candidates Since 1968
### with other important information about the campaign
### String formatting is for NYT API
### Today we separate the Bushes by middle name, but in this instance
### it makese sense to just call Bush Sr. "George Bush" because at the time 
### Bush Jr. was not getting articles written about him at this point

candidate <- c("Richard+Nixon", "Hubert+Humphrey", 
              "Richard+Nixon", "George+McGovern", 
              "Jimmy+Carter", "Gerald+Ford",
              "Ronald+Reagan", "Jimmy+Carter",
              "Ronald+Reagan", "Walter+Mondale",
              "George+Bush", "Michael+Dukakis",
              "Bill+Clinton", "George+Bush",
              "Bill+Clinton", "Bob+Dole",
              "George+W.+Bush", "Al+Gore", 
              "George+W.+Bush", "John+Kerry",
              "Barack+Obama", "John+McCain",
              "Barack+Obama", "Mitt+Romney",
              "Donald+Trump", "Hillary+Clinton",
              "Joseph+Biden", "Donald+Trump",
              "Donald+Trump", "Kamala+Harris")


incumbant_party <- c(FALSE, TRUE,
                     TRUE, FALSE,
                     FALSE, TRUE,
                     FALSE, TRUE,
                     TRUE, FALSE,
                     TRUE, FALSE,
                     FALSE, TRUE,
                     TRUE,FALSE,
                     FALSE, TRUE,
                     TRUE,FALSE,
                     FALSE, TRUE,
                     TRUE, FALSE,
                     FALSE, TRUE,
                     FALSE, TRUE,
                     FALSE, TRUE)

party <- c("r", "d", 
           "r", "d", 
           "d", "r", 
           "r", "d", 
           "r", "d", 
           "r", "d", 
           "d", "r", 
           "d", "r",
           "r", "d",
           "r", "d",
           "d", "r",
           "d", "r",
           "r", "d",
           "d", "r",
           "r","d")

winner <- rep(c(TRUE, FALSE), 15)

### Create List of year
year <- c(rep(1968, 2), rep(1972, 2), rep(1976, 2), rep(1980, 2), rep(1984, 2), 
           rep(1988, 2), rep(1992, 2), rep(1996, 2), rep(2000, 2), rep(2004, 2),
           rep(2008, 2), rep(2012, 2), rep(2016, 2), rep(2020, 2), rep(2024, 2))

### Create List Of Election Days in Year
election_day <- c(rep(ymd("1968-11-05"), 2), rep(ymd("1972-11-07"), 2), rep(ymd("1976-11-02"), 2), 
                  rep(ymd("1980-11-04"), 2), rep(ymd("1984-11-06"), 2), rep(ymd("1988-11-08"), 2), 
                  rep(ymd("1992-11-03"), 2), rep(ymd("1996-11-05"), 2), rep(ymd("2000-11-07"), 2), 
                  rep(ymd("2004-11-02"), 2), rep(ymd("2008-11-04"), 2), rep(ymd("2012-11-06"), 2), 
                  rep(ymd("2016-11-08"), 2), rep(ymd("2020-11-03"), 2), rep(ymd("2024-11-05"), 2))

### It is days like these that make me thankful for Mr. Brown teaching me how to type 
### properly in high school.

### Combine These Lists into a dataframe
presidential_candidates <- data.frame(candidate = candidate, party = party, incumbant_party = incumbant_party, winner = winner, year = year, election_day = election_day)

### Create Column of Dates that are 107 days before election day (when Harris officially started running)
presidential_candidates <- presidential_candidates |> mutate(
  start_date_2024_adjustment = election_day - days(107)
)

### Write this dataframe onto the disk, will be used in other R scripts for the project
write.csv(presidential_candidates, paste0(path, "presidential_candidates.csv"))


article_urls <- data.frame()

#############################
# SET YOUR NYT API KEY HERE #
#############################

### If you do not have one: you can make one for free here: https://developer.nytimes.com/get-started
### This key will work, but you should make your own because there is a non 0 chance 
### that my key won't have any uses when you attempt to run this
key <- "yKfZ0OFrJ1Yv7EnALtHTS6OTNnFgFFqR"



#######################################################
# STOP READ THIS COMMENT BEFORE RUNNING ANY MORE CODE #
#######################################################

### Get Article URLs for Each Candidate from the Start Date (107 days before 
### election day) and End Date (election day)

### Some notes on this for loop, it'll take A VERY LONG TIME to run
### Each President is searched for 10 times, each time going back a page
### to get a total of 100 articles in the time frame mentioned. 
### With a total of 3000 article URLs being pulled and 13 seconds separating each
### API request to not over step the limits put up by the NYTimes.
### If you just want to run it for a bit, I made sure to implement print()
### statements at the end of every URL request, so you'll know if it is progressing
### Additionally, check article_urls data frame once you get a few results done
### because it'll basically just look like that but for every candidate in the 
### presidential_candidates dataframe

### The For Loop to get Article URLs and publication dates from NYT API
for (i in 1:30) {
  ### Set Candidate Name
  name <- presidential_candidates$candidate[[i]]
  
  ### Set Start Date 0 is for formatting date as YYYYMMDD the issue with this 
  ### set of dates is the month number
  start_date <- paste0(year(presidential_candidates$start_date_2024_adjustment[[i]]), "0",
                       month(presidential_candidates$start_date_2024_adjustment[[i]]),
                       day(presidential_candidates$start_date_2024_adjustment[[i]]))
    
  ### Set End Date 0 is for formatting date as YYYYMMDD The issue with this 
  ### set of dates is the Day number
  end_date <- paste0(year(presidential_candidates$election_day[[i]]),
                     month(presidential_candidates$election_day[[i]]), "0",
                     day(presidential_candidates$election_day[[i]]))
  ### Set Year
  article_year <- presidential_candidates$year[[i]]
  
  ### Before 1984, all articles are classified as "archive" and not to their
  ### specific "news_desk" type. 
  if(article_year <= 1980) {
    nyt_url <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",name,
                      "&begin_date=",start_date,"&end_date=",end_date, "&sort=newest",
                      "&facet_filter=true&api-key=", key, sep="")
    
    ### Get First 100 Articles Sorted by Newest
    for (g in 1:10) {
      ### Get Results
      nytSearchResults <- fromJSON(paste0(nyt_url, "&page=", g), flatten = TRUE) %>% data.frame()
      
      ### Print Message Saying it was successful
      print(paste0("Retrieved Results from Page ", g, 
                   " On Articles Covering ", name, " in ", article_year))
      
      ### Select desired rows and add presidential information
      nytSearchResults <- nytSearchResults |> 
        rename(web_url = response.docs.web_url,
               pub_date = response.docs.pub_date) |>
        select(c(web_url, pub_date)) |>
        mutate(
          candidate = name,
          year = article_year
        )
      
      ### Rbind with article_urls dataframe
      article_urls <- rbind(article_urls, nytSearchResults)
      
      ### Sleep for 13 Seconds to allow for Maximum pulling of information
      ### I know that it is 12 seconds for the fastest possible, but I don't 
      ### want to risk it breaking while I am asleep. 
      Sys.sleep(13)
    }
  }
  
  ### 1984 allows us to select for specifically opinion pieces
  if (article_year >= 1984) {
    ### Set API Search
    ### I used their tool to help generate the correct URL for me to use
    ### https://developer.nytimes.com/docs/articlesearch-product/1/routes/articlesearch.json/get
    ### URL Filters Articles Based On Source, Section, and Document Type
    ### Section selects for Opinion and document type selects for articles
    ### rather than videos, slideshows, or other forms of interactive media.
    nyt_url <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",name,
                      "&begin_date=",start_date,"&end_date=",end_date, "&sort=newest",
                      "&fq=source%3A(%22The%20New%20York%20Times%22)%20AND%20document_type%3A(%22article%22)%20AND%20section_name%3A(%22Opinion%22)",
                      "&facet_filter=true&api-key=", key, sep="")
    
    ### Get First 100 Articles Sorted by Relevance
    for (h in 1:10) {
      ### Get Results
      nytSearchResults <- fromJSON(paste0(nyt_url, "&page=", h), flatten = TRUE) %>% data.frame()
      
      ### Print Message Saying it was successful
      print(paste0("Retrieved Results from Page ", h, 
                   " On Articles Covering ", name, " in ", article_year))
      
      ### Select desired rows and add presidential information
      nytSearchResults <- nytSearchResults |> 
        rename(web_url = response.docs.web_url,
               pub_date = response.docs.pub_date) |>
        select(c(web_url, pub_date)) |>
        mutate(
          candidate = name,
          year = article_year
        )
        
      ### Rbind with article_urls dataframe
      article_urls <- rbind(article_urls, nytSearchResults)
      
      ### Sleep for 13 Seconds to allow for Maximum pulling of information
      ### I know that it is 12 seconds for the fastest possible, but I don't 
      ### want to risk it breaking while I am asleep. 
      Sys.sleep(13)
    }
  }
}

### Save Article Urls to Disk for Safe Keeping

write.csv(article_urls, paste0(path, "article_info/article_urls.csv"))

###############################
# Step 2 Getting Article Text #
###############################
### Read CSV that is stored locally 
article_urls <- read.csv(paste0(path, "article_info/article_urls.csv"))

### Get rid of artifact observation number column
article_urls <- article_urls |> select(!X)


### I had some issues accessing full NYT articles because R wasn't logged in to the
### NYTs I tried to look around for code that would solve my problem 
### I tried 3 different solutions and none of them worked
### The first one gave me a 403 when I tried to use the code
### https://optimumsportsperformance.com/blog/web-scraping-webpages-that-require-login-info/
### These two wouldn't load any webpage
### https://stackoverflow.com/questions/43580133/how-to-use-rselenium-to-login-to-a-website-on-windows-machine
### https://www.geeksforgeeks.org/web-scraping-using-rselenium/
### I can atleast grab the first few sentences of a large number of articles
### on each candidate

### Create Dataframe
all_articles_text <- data.frame(matrix(NA, nrow = 0, ncol = 4))
names(all_articles_text) <- c("candidate", "year", "date", "text")

### A note on this for loop: 
### I wanted to be nice to the NYTimes servers and not access 3000 articles very
### quickly, especially because I don't want to get blocked by them! So I set up
### a Sys.sleep() to pause the for loop. This introduces the probability 
### that you will get a network error. The code works perfectly fine, however
### you will occasionally get a 504 error. This is just a server hiccup on the 
### end of the NYTimes, I'm not quite sure how to force my code to run through
### the error or run the last iteration again on an error. 
### When gathering the text of the 3000 articles the first time, I simply ran the 
### code and whenver there was a 504 error, I would adjust the lowerbound
### of the for loop.It is very random when you'll get a 504 loop. I generated 
### 1500 articles in one go one time and just under 100 another. 


for (k in 1:3000) {
  ### Set URL
  url <- article_urls$web_url[[k]]
  
  ### Get HTML
  article_webpage <- read_html(url)
  
  ### Set Article Publication Date
  
  article_date <- article_urls$pub_date[[k]]
  
  ### Set Candidate
  candidate_from_result <- article_urls$candidate[[k]]
  
  ### Set Election Cycle Year
  election_cycle <- article_urls$year[[k]]
  
  
  ### Get Article Text  
  article_text <- article_webpage |>
    html_elements("article") |>
    html_elements("p") |>
    html_text()
  
  ### Set information into Dataframe
  df <- data.frame(candidate = candidate_from_result, year = election_cycle, date = article_date, 
                   text = article_text)
  
  ### Filter Text for Common extra statements like "Advertisement" , "supported by" , 
  ### subscription error, or "view Full Article in Timesmachine"
  df <- df |> filter(str_detect(text, "Advertisement") == FALSE & str_detect(text, "Supported by") == FALSE & 
                       str_detect(text, "We are having trouble retrieving the article content.") == FALSE &
                       str_detect(text, "Please enable JavaScript in your browser settings.") == FALSE &  
                       str_detect(text, "Thank you for your patience while we verify access.") == FALSE &  
                       str_detect(text, "Thank you for your patience while we verify access. If you are in Reader mode please exit and log into your Times account, or subscribe for all of The Times.") == FALSE &  
                       str_detect(text, "Already a subscriber?") == FALSE &  
                       str_detect(text, "Want all of The Times?") == FALSE) |>
    mutate(text = str_remove(text, "View Full Article in Timesmachine »"))
  
  ### Combine Text to one cell
  
  df <- df |> mutate(text = paste(text, sep=" ", collapse = " ")) |> distinct()
  
  ### Bind Text to All Aritcle Text Dataframe
  all_articles_text <- rbind(all_articles_text, df)
  
  ### Sleep for 13 Seconds to be nice to the NYT servers.
  print(paste0("Article ", k, " covering ", candidate_from_result, " text extracted!"))
  Sys.sleep(13)
}

### Write Article Text to CSV

write.csv(all_articles_text, file = paste0(path, "article_info/article_text.csv"))
