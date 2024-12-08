library(tidyverse)
library(udpipe)
library(ggplot2)
library(tidytext)
library(igraph)
library(tibbletime)


######################
# Set your Path Here #
######################

path <- "C:/Users/zades/Documents/GitHub/Final-Project/"

##############
# Read Files #
##############

article_text <- read.csv(paste0(path, "article_info/article_text.csv"))

presidential_candidates <- read.csv(paste0(path, "presidential_candidates.csv"))

#####################
# Clean Up the Data #
#####################

### Remove Identical Articles and select necessary variables
### I saved candidate into the original data frame so I can save the 
### progress when getting text, in case of network errors.  
article_text <- article_text |> select(year, date, text) |> distinct(text, .keep_all = TRUE)

### Make dates date-times
article_text <- article_text |>
  mutate(
    date = str_remove_all(date, "T.*"),
    date = ymd(date)
  )


##################################################
# For Loop That Gets Words Relating to Candidate #
##################################################

### Create Empty All Words Dataframe
### Columns are Date, word, and Candidate
all_words <- data.frame()

### Will take quite some time to run
for (i in seq(1968, 2024, 4)) {
  ### Get Text Data Frame for Particular Year
  ### I am doing this at the year level to break it in to smaller chunks to process
  ### rather than the entire thing at once
  ### I will save each year to it's own folder for other uses not related to the 
  ### immediate project 
  year_filtered_articles <- article_text |>  filter(year == i) |> select(text, date) |> rename(doc_id = date)
  
  ### Break up article text into format that R can analyze
  ### Paragraphs identify individual articles 
  ### At this point, we group all articles by day to do sentiment analysis on how
  ### all articles feel about the president
  year_filtered_articles <- udpipe(year_filtered_articles, "english")
  
  ### Get Candidate Names
  names <- presidential_candidates |> filter(year == i) |> select(candidate)
  
  ### Seperate First and Last Name
  names <- names |> 
    mutate(
      first_name = str_remove(candidate, "\\+.*"),
      last_name = str_remove(candidate, "^[^+]*\\+")) |> 
    select(!candidate)
  
  ### Get dataframe of only sentence identifying information, lemma, and token_id being selected
  year_limited_head <- year_filtered_articles |> select(doc_id, paragraph_id, 
                                                        sentence_id, head_token_id, 
                                                        lemma)
  
  year_limited <- year_filtered_articles |> select(doc_id, paragraph_id, 
                                                   sentence_id, token_id, lemma)
  
  ### If Statement checks for Harris, this is because she would not be referred 
  ### to as Mr.
  ### Second If statement corrects candidate 2 from Mr. to Ms. to adjust for 
  ### Harris
  if (names[2,2] != "Harris") {
    ### Sentiment of Candidate 1
    ### Use last name as identifier
    candidate_1 <- year_filtered_articles |> filter(lemma == names[1,2]) 
    
    candidate_1_children <- candidate_1|>
      inner_join(year_limited_head, 
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "token_id" == "head_token_id")) |>
      select(doc_id, lemma.y) |>
      rename(word = lemma.y)
    
    ### Extra rows here are kept for next step
    candidate_1_parent <- candidate_1 |>
      inner_join(year_limited,
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "head_token_id" == "token_id")) |>
      select(doc_id, lemma.y) |>
      rename(word = lemma.y)
    
    ### A lot of the names in parent are just the first name of the candidate 
    ### so we'll have to do another round to get more words
    ### Filter out First Names that aren't the candidates (family members)
    ### but keep every other word
    ### Other proper nouns that we'll want to keep might include Administration and
    ### President. Also, Mr To keep the wanted proper nouns, I'm going to convert them to
    ### lowercase and then filter out all remaining proper nouns 
    
    candidate_1_parent <- candidate_1_parent |> 
      mutate(
        word = ifelse(word == names[1,1] | word == "Administration" | word == "President"| word == "Mr.",
                      tolower(word), word)
      ) |>
      filter(str_detect(word, "[[:upper:]]") != TRUE) |>
      mutate(
        word = ifelse(word == tolower(names[1,1]) | word == "administration" | word == "president"| word == "mr.",
                      str_to_title(word), word)
      )
    
    ### Get Parents of Parents (Children were retrieved in previous step)
    ### I know this isn't the best way to do it, but I need a working prototype
    ### This code is simialr to the previous step, but it filters the article
    ### list with all the variables, so we can get the head token id for 
    ### the parent
    ### .y identifes parent information
    candidate_1_again <- candidate_1 |>
      inner_join(year_filtered_articles,
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "head_token_id" == "token_id")) |>
      rename(word = lemma.y)|> 
      mutate(
        word = ifelse(word == names[1,1] | word == "Administration" | word == "President"| word == "Mr.",
                      tolower(word), word)
      ) |>
      filter(str_detect(word, "[[:upper:]]") != TRUE) |>
      mutate(
        word = ifelse(word == tolower(names[1,1]) | word == "administration" | word == "president"| word == "mr.",
                      str_to_title(word), word)
      ) 
    
    ### Get Parent of Parents Dataframe
    candidate_1_grandparent <- candidate_1_again |>
      inner_join(year_limited, 
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "head_token_id.y" == "token_id")) |>
      select(doc_id, lemma) |>
      rename(word = lemma) 
    
    ### Get Children of Children
    candidate_1_again <- candidate_1 |>
      inner_join(year_filtered_articles,
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "token_id" == "head_token_id")) |>
      rename(word = lemma.y)
    
    candidate_1_grandchildren <- candidate_1_again |>
      inner_join(year_limited_head, 
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "token_id.y" == "head_token_id")) |>
      select(doc_id, lemma) |>
      rename(word = lemma)
    
    ### Rbind Words together
    candidate_1_words <- rbind(candidate_1_children, candidate_1_parent, 
                               candidate_1_grandparent, candidate_1_grandchildren)
    
    candidate_1_words <- candidate_1_words |> mutate(candidate = paste0(names[1,1], " ", names[1,2]))
    
    ### Remove Unneeded Dataframes to clean up my Environment
    rm(candidate_1, candidate_1_children, candidate_1_again, candidate_1_grandparent, candidate_1_parent, candidate_1_grandchildren)
    
    print(paste0("Words on ", names[1,1], " ", names[1,2], " gathered!"))
    
    ### words of Candidate 2 
    ### Use last name as identifier
    candidate_2 <- year_filtered_articles |> filter(lemma == names[2,2]) 
    
    candidate_2_children <- candidate_2|>
      inner_join(year_limited_head, 
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "token_id" == "head_token_id")) |>
      select(doc_id, lemma.y) |>
      rename(word = lemma.y)
    
    ### Extra rows here are kept for next step
    candidate_2_parent <- candidate_2 |>
      inner_join(year_limited,
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "head_token_id" == "token_id")) |>
      select(doc_id, lemma.y) |>
      rename(word = lemma.y)
    
    ### A lot of the names in parent are just the first name of the candidate 
    ### so we'll have to do another round to get more words
    ### Filter out First Names that aren't the candidates (family members)
    ### but keep every other word
    ### Other proper nouns that we'll want to keep might include Administration and
    ### President. Also, Mr To keep the wanted proper nouns, I'm going to convert them to
    ### lowercase and then filter out all remaining proper nouns 
    
    candidate_2_parent <- candidate_2_parent |> 
      mutate(
        word = ifelse(word == names[2,1] | word == "Administration" | word == "President"| word == "Mr.",
                      tolower(word), word)
      ) |>
      filter(str_detect(word, "[[:upper:]]") != TRUE) |>
      mutate(
        word = ifelse(word == tolower(names[2,1]) | word == "administration" | word == "president"| word == "mr.",
                      str_to_title(word), word)
      )
    
    ### Get Parents of First name (Children were retrieved in previous step)
    ### I know this isn't the best way to do it, but I need a working prototype
    ### This code is simialr to the previous step, but it filters the article
    ### list with all the variables, so we can get the head token id for 
    ### the parent
    ### .y identifes parent information
    candidate_2_again <- candidate_2 |>
      inner_join(year_filtered_articles,
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "head_token_id" == "token_id")) |>
      rename(word = lemma.y)|> 
      mutate(
        word = ifelse(word == names[2,1] | word == "Administration" | word == "President"| word == "Mr.",
                      tolower(word), word)
      ) |>
      filter(str_detect(word, "[[:upper:]]") != TRUE) |>
      mutate(
        word = ifelse(word == tolower(names[2,1]) | word == "administration" | word == "president"| word == "mr.",
                      str_to_title(word), word)
      )
    
    ### Get Parent of Parents
    candidate_2_grandparent <- candidate_2_again |>
      inner_join(year_limited, 
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "head_token_id.y" == "token_id")) |>
      select(doc_id, lemma) |>
      rename(word = lemma) 
    
    ### Get Children of Children
    candidate_2_again <- candidate_2 |>
      inner_join(year_filtered_articles,
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "token_id" == "head_token_id")) |>
      rename(word = lemma.y)
    
    candidate_2_grandchildren <- candidate_2_again |>
      inner_join(year_limited_head, 
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "token_id.y" == "head_token_id")) |>
      select(doc_id, lemma) |>
      rename(word = lemma)
    
    ### Rbind Words together
    candidate_2_words <- rbind(candidate_2_children, candidate_2_parent, 
                               candidate_2_grandparent, candidate_2_grandchildren)
    
    candidate_2_words <- candidate_2_words |> mutate(candidate = paste0(names[2,1], " ", names[2,2]))
    
    ### Remove Unneeded Dataframes to clean up my Environment
    rm(candidate_2, candidate_2_children, candidate_2_again, candidate_2_grandparent, candidate_2_parent, candidate_2_grandchildren)
    
    print(paste0("Words on ", names[2,1], " ", names[2,2], " gathered!"))
    
    ### Bind words gathered in previous steps to all words 
    all_words <- rbind(candidate_1_words, candidate_2_words, all_words)
  }
  
  
  if (names[2,2] == "Harris") {
    ### Sentiment of Candidate 1
    ### Use last name as identifier
    candidate_1 <- year_filtered_articles |> filter(lemma == names[1,2]) 
    
    candidate_1_children <- candidate_1|>
      inner_join(year_limited_head, 
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "token_id" == "head_token_id")) |>
      select(doc_id, lemma.y) |>
      rename(word = lemma.y)
    
    ### Extra rows here are kept for next step
    candidate_1_parent <- candidate_1 |>
      inner_join(year_limited,
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "head_token_id" == "token_id")) |>
      select(doc_id, lemma.y) |>
      rename(word = lemma.y)
    
    ### A lot of the names in parent are just the first name of the candidate 
    ### so we'll have to do another round to get more words
    ### Filter out First Names that aren't the candidates (family members)
    ### but keep every other word
    ### Other proper nouns that we'll want to keep might include Administration and
    ### President. Also, Mr To keep the wanted proper nouns, I'm going to convert them to
    ### lowercase and then filter out all remaining proper nouns 
    
    candidate_1_parent <- candidate_1_parent |> 
      mutate(
        word = ifelse(word == names[1,1] | word == "Administration" | word == "President"| word == "Mr.",
                      tolower(word), word)
      ) |>
      filter(str_detect(word, "[[:upper:]]") != TRUE) |>
      mutate(
        word = ifelse(word == tolower(names[1,1]) | word == "administration" | word == "president"| word == "mr.",
                      str_to_title(word), word)
      )
    
    ### Get Parents of Parents (Children were retrieved in previous step)
    ### I know this isn't the best way to do it, but I need a working prototype
    ### This code is simialr to the previous step, but it filters the article
    ### list with all the variables, so we can get the head token id for 
    ### the parent
    ### .y identifes parent information
    candidate_1_again <- candidate_1 |>
      inner_join(year_filtered_articles,
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "head_token_id" == "token_id")) |>
      rename(word = lemma.y)|> 
      mutate(
        word = ifelse(word == names[1,1] | word == "Administration" | word == "President"| word == "Mr.",
                      tolower(word), word)
      ) |>
      filter(str_detect(word, "[[:upper:]]") != TRUE) |>
      mutate(
        word = ifelse(word == tolower(names[1,1]) | word == "administration" | word == "president"| word == "mr.",
                      str_to_title(word), word)
      ) 
    
    ### Get Parent of Parents Dataframe
    candidate_1_grandparent <- candidate_1_again |>
      inner_join(year_limited, 
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "head_token_id.y" == "token_id")) |>
      select(doc_id, lemma) |>
      rename(word = lemma) 
    
    ### Get Children of Children
    candidate_1_again <- candidate_1 |>
      inner_join(year_filtered_articles,
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "token_id" == "head_token_id")) |>
      rename(word = lemma.y)
    
    candidate_1_grandchildren <- candidate_1_again |>
      inner_join(year_limited_head, 
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "token_id.y" == "head_token_id")) |>
      select(doc_id, lemma) |>
      rename(word = lemma)
    
    ### Rbind Words together
    candidate_1_words <- rbind(candidate_1_children, candidate_1_parent, 
                               candidate_1_grandparent, candidate_1_grandchildren)
    
    candidate_1_words <- candidate_1_words |> mutate(candidate = paste0(names[1,1], " ", names[1,2]))
    
    ### Remove Unneeded Dataframes to clean up my Environment
    rm(candidate_1, candidate_1_children, candidate_1_again, candidate_1_grandparent, candidate_1_parent, candidate_1_grandchildren)
    
    print(paste0("Words on ", names[1,1], " ", names[1,2], " gathered!"))
    
    ### words of Candidate 2 
    ### Use last name as identifier
    candidate_2 <- year_filtered_articles |> filter(lemma == names[2,2]) 
    
    candidate_2_children <- candidate_2|>
      inner_join(year_limited_head, 
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "token_id" == "head_token_id")) |>
      select(doc_id, lemma.y) |>
      rename(word = lemma.y)
    
    ### Extra rows here are kept for next step
    candidate_2_parent <- candidate_2 |>
      inner_join(year_limited,
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "head_token_id" == "token_id")) |>
      select(doc_id, lemma.y) |>
      rename(word = lemma.y)
    
    ### A lot of the names in parent are just the first name of the candidate 
    ### so we'll have to do another round to get more words
    ### Filter out First Names that aren't the candidates (family members)
    ### but keep every other word
    ### Other proper nouns that we'll want to keep might include Administration and
    ### President. Also, Mr To keep the wanted proper nouns, I'm going to convert them to
    ### lowercase and then filter out all remaining proper nouns 
    
    candidate_2_parent <- candidate_2_parent |> 
      mutate(
        word = ifelse(word == names[2,1] | word == "Administration" | word == "President"| word == "Mr.",
                      tolower(word), word)
      ) |>
      filter(str_detect(word, "[[:upper:]]") != TRUE) |>
      mutate(
        word = ifelse(word == tolower(names[2,1]) | word == "administration" | word == "president"| word == "mr.",
                      str_to_title(word), word)
      )
    
    ### Get Parents of First name (Children were retrieved in previous step)
    ### I know this isn't the best way to do it, but I need a working prototype
    ### This code is simialr to the previous step, but it filters the article
    ### list with all the variables, so we can get the head token id for 
    ### the parent
    ### .y identifes parent information
    candidate_2_again <- candidate_2 |>
      inner_join(year_filtered_articles,
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "head_token_id" == "token_id")) |>
      rename(word = lemma.y)|> 
      mutate(
        word = ifelse(word == names[2,1] | word == "Administration" | word == "President"| word == "Mr.",
                      tolower(word), word)
      ) |>
      filter(str_detect(word, "[[:upper:]]") != TRUE) |>
      mutate(
        word = ifelse(word == tolower(names[2,1]) | word == "administration" | word == "president"| word == "mr.",
                      str_to_title(word), word)
      )
    
    ### Get Parent of Parents
    candidate_2_grandparent <- candidate_2_again |>
      inner_join(year_limited, 
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "head_token_id.y" == "token_id")) |>
      select(doc_id, lemma) |>
      rename(word = lemma) 
    
    ### Get Children of Children
    candidate_2_again <- candidate_2 |>
      inner_join(year_filtered_articles,
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "token_id" == "head_token_id")) |>
      rename(word = lemma.y)
    
    candidate_2_grandchildren <- candidate_2_again |>
      inner_join(year_limited_head, 
                 join_by("doc_id" == "doc_id",
                         "paragraph_id" == "paragraph_id",
                         "sentence_id" == "sentence_id",
                         "token_id.y" == "head_token_id")) |>
      select(doc_id, lemma) |>
      rename(word = lemma)
    
    ### Rbind Words together
    candidate_2_words <- rbind(candidate_2_children, candidate_2_parent, 
                               candidate_2_grandparent, candidate_2_grandchildren)
    
    candidate_2_words <- candidate_2_words |> mutate(candidate = paste0(names[2,1], " ", names[2,2]))
    
    ### Remove Unneeded Dataframes to clean up my Environment
    rm(candidate_2, candidate_2_children, candidate_2_again, candidate_2_grandparent, candidate_2_parent, candidate_2_grandchildren)
    
    print(paste0("Words on ", names[2,1], " ", names[2,2], " gathered!"))
    
    ### Bind words gathered in previous steps to all words 
    all_words <- rbind(candidate_1_words, candidate_2_words, all_words)
  }
}

### Save All_words to disk
write.csv(all_words, paste0(path, "all_words_candidates.csv"))

all_words <- read.csv(paste0(path, "all_words_candidates.csv"))

### Make sure date is date_time (if you read in csv rather than generated for the first time)
all_words <- all_words |> 
  mutate(
    doc_id = ymd(doc_id),
    year = year(doc_id)
  )


### Get Sentiment of Words
sentiment_afinn <- get_sentiments("afinn") |> rename(afinn = value)
sentiment_bing <- get_sentiments("bing") |> mutate(bing_sentiment = ifelse(sentiment == "positive", 1, -1)) |> select(!sentiment)
sentiment_loughran <- get_sentiments("loughran") |> mutate(loughran_sentiment = ifelse(sentiment == "positive", 1, -1)) |> select(!sentiment)

all_words <- all_words |>
  left_join(sentiment_afinn, by = c("word" = "word")) |>
  left_join(sentiment_bing, by = c("word"= "word")) |>
  left_join(sentiment_loughran, by = c("word" = "word"))

### Find what option gives the lowest % of entries as NA
mean(is.na(all_words$afinn))
mean(is.na(all_words$bing_sentiment))
mean(is.na(all_words$loughran_sentiment))

### Bing has the most data, will also take afinn because of its wider scale
words_bing <- all_words |>
  select(candidate, year, doc_id, word, bing_sentiment) |>
  rename(date = doc_id) |>
  filter(!is.na(bing_sentiment))

words_afinn <- all_words |>
  select(candidate, year, doc_id, word, afinn) |>
  rename(date = doc_id) |>
  filter(!is.na(afinn))



### Get Average Sentiment for each candidate
### And daily average

words_bing <- words_bing |>
  group_by(candidate, year) |>
  mutate(average_bing = mean(bing_sentiment)) |>
  ungroup()

words_bing <- words_bing |>
  group_by(candidate, date) |>
  mutate(daily_bing = mean(bing_sentiment)) |>
  ungroup() |>
  select(candidate, date, year, average_bing, daily_bing) |> distinct()


words_afinn <- words_afinn |>
  group_by(candidate, year) |>
  mutate(average_afinn = mean(afinn)) |>
  ungroup()

words_afinn <- words_afinn |>
  group_by(candidate, date) |>
  mutate(daily_afinn = mean(afinn)) |>
  ungroup() |>
  select(candidate,date, year, average_afinn, daily_afinn) |> distinct()


### Replace spaces in candidate names with + for easy joining in analysis.r

words_bing <- words_bing |> mutate(candidate = str_replace_all(candidate, " ", "\\+"))

words_afinn <- words_afinn |> mutate(candidate = str_replace_all(candidate, " ", "\\+"))


### Export tables as CSVs

write.csv(words_bing, paste0(path, "sentiment_bing.csv"))

write.csv(words_afinn, paste0(path, "sentiment_afinn.csv"))
