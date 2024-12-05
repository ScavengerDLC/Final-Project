library(tidyverse)

######################
# Set your Path Here #
######################

path <- "C:/Users/zades/Documents/GitHub/Final-Project/"

#####################
# Load Polling CSVs #
#####################

### Historical Polling Averages from 1968-2016
polling_historical <- read.csv(paste0(path, "Data/pres_pollaverages_1968-2016.csv"))

### Polling Average 2020
polling_2020 <- read.csv(paste0(path, "Data/presidential_poll_averages_2020.csv"))

### Polling Average 2024
polling_2024 <- read.csv(paste0(path, "Data/presidential_general_averages.csv"))

######################################################################
# Get dataframes into one set of all polling averages #
######################################################################

### Fix 2024 Dataframe by removing 2020 data and select correct columns(does not 
### include pct_estimate from 2020 dataframe) and remove third parties plus Joe Biden data
polling_2024 <- polling_2024 |>
  filter(cycle == 2024) |>
  filter(candidate == "Harris" | candidate == "Trump") |>
  select(c(candidate, date, state, cycle, pct_estimate)) |>
  rename(year = cycle)

### Make Candidates Names in NYTs format and make date a datetime
polling_2024 <- polling_2024 |>
  mutate(candidate = ifelse(candidate == "Trump", "Donald+Trump", "Kamala+Harris")) |>
  mutate(date = ymd(date))

### Fix 2020 Dataframe by selecting correct columns and fixing column names
### also make date stored as a datetime rather than "character" and 
### remove Third Parties
polling_2020 <- polling_2020 |>
  select(!pct_trend_adjusted) |>
  rename(year = cycle,
         candidate = candidate_name,
         date = modeldate) |>
  mutate(date = mdy(date)) |>
  filter(candidate == "Joseph R. Biden Jr." | candidate == "Donald Trump")

### Correct names to be in NYT search style
polling_2020 <- polling_2020 |>
  mutate(candidate = ifelse(candidate == "Joseph R. Biden Jr.", "Joseph+Biden", "Donald+Trump"))

### Fix column order for rbind
polling_2020 <- polling_2020 |>
  relocate(year, .before = pct_estimate) |>
  relocate(state, .before = year) |>
  relocate(date, .before = state) |>
  relocate(candidate, .before = date)

### Fix Historical Dataframe by selecting correct columns and fixing column names
polling_historical <- polling_historical |>
  rename(year = cycle,
         candidate = candidate_name,
         date = modeldate) |>
  select(c(candidate, date, state, year, pct_estimate)) |>
  mutate(date = mdy(date)) 

### Fix Minor Name issues like Hillary having her middle name
polling_historical <- polling_historical |>
  mutate(
    candidate = ifelse(candidate == "Hillary Rodham Clinton", "Hillary+Clinton",
                ifelse(candidate == "Michael S. Dukakis", "Michael+Dukakis",
                ifelse(candidate == "Walter F. Mondale", "Walter+Mondale",
                ifelse(candidate == "Gerald R. Ford", "Gerald+Ford",
                ifelse(candidate == "George S. McGovern", "George+McGovern",
                ifelse(candidate == "Richard M. Nixon", "Richard+Nixon",
                ifelse(candidate == "Hubert Humphrey, Jr.", "Hubert+Humphrey", candidate)))))))
  )

### Turn Other names into NYTimes name format
polling_historical <- polling_historical |>
  mutate(
    candidate = str_replace_all(candidate, " ", "+")
  )

### Rbind columns Each row in each dataframe is a unique measurement of pct_estimate
### for each state in a given year for each candidate per election cycle. 

polling_data <- rbind(polling_2024, polling_2020, polling_historical)

### Read Presidential_candidates csv
presidential_candidates <- read.csv(paste0(path, "presidential_candidates.csv"))
### Join Dataframes with presidential_candidates
polling_data <- polling_data |>
  left_join(presidential_candidates, 
            join_by(candidate, year))

### Remove Index Column from csv
polling_data <- polling_data |> select(!X)

### Remove Third Parties (they never stood a chance...)
polling_data <- polling_data |> filter(party == "d" | party == "r")

### Create "Valid" variable that is a true/false testing if the date is before 
### the cut off
polling_data <- polling_data |>
  mutate(
    valid = ifelse(date >= start_date_2024_adjustment, TRUE, FALSE)
  )


### Filter for Valid Days and remove valid variable
polling_data <- polling_data |>
  filter(valid == TRUE) |>
  select(!valid)

### Create margin_estimate variable
margin_data <- polling_data |>
  pivot_wider(id_cols = c(state, date), names_from = party, values_from = pct_estimate)

margin_data <- margin_data |>
  mutate(dem_margin_estimate = d - r) |>
  select(state, date, dem_margin_estimate)

polling_data <- polling_data |>
  left_join(margin_data, by = c("state", "date"))

### Export Polling Data as csv
write.csv(polling_data, paste0(path, "polling_data.csv"))

####################
# Load Results CSV #
####################

election_results_historical <- read.csv(paste0(path, "/Data/election_results_1968_2020.csv"))

election_results_2024 <- read.csv(paste0(path, "Data/election_results_2024.csv"))

#########################################################################################
# Get Dataframes in a structure where each observation is a state/candidate/year/result #
#########################################################################################

############################################
# Historical Data Wrangling from here down #
############################################
election_results_historical <- election_results_historical |>
  pivot_longer(cols = c(Alabama..All.Parties..Votes:Wyoming..Other..Votes), names_to = "id", values_to = "raw_votes")

### Change .. to _ in id stringe, will help identify state names. States with
### two names will only have . seperating them while all other words have ..

election_results_historical <- election_results_historical |>
  mutate(id = str_replace_all(id, "\\.\\.", "_")) 


### Extract State and Parties from id column
election_results_historical <- election_results_historical |>
  mutate(party = str_extract(id, "_.*")) |>
  mutate(state = str_remove(id, party))

### Remove Leading Underscore in Party Column and remove _Votes
election_results_historical <- election_results_historical |>
  mutate(party = str_remove(party, "_")) |>
  mutate(party = str_remove(party, "_Votes"))

### Remove ID column
election_results_historical <- election_results_historical |>
  select(c(!id))

### Pivot Wider to get into format similar other dataframe
election_results_historical <- election_results_historical |>
  pivot_wider(id_cols = c(Time, state), names_from = party, values_from = raw_votes)

### Make National Dataframe
national_total_historical <- data.frame(matrix(nrow=0, ncol = 6))

names(national_total_historical) <- names(election_results_historical)

for (h in 1:14) {
  year_selected <- 1968 + 4*(h-1)
  
  year_data <- election_results_historical |> filter(Time == year_selected)
  
  national_total_year <- data.frame(matrix(NA, nrow=1, ncol = 6))
  
  names(national_total_year) <- names(election_results_historical)
  
  national_total_year <- national_total_year |>
    mutate(
      Time = year_selected,
      state = "National",
      Democratic = sum(year_data$Democratic),
      Republican = sum(year_data$Republican),
      Other = sum(year_data$Other),
      All.Parties = sum(year_data$All.Parties)
    )
  
  national_total_historical <- rbind(national_total_historical, national_total_year)
  
}

election_results_historical <- rbind(election_results_historical, national_total_historical)

### Pivot longer to get back into state/year/party/total format

election_results_historical <- election_results_historical |> rename(total = All.Parties) |>
  pivot_longer(
  cols = c(total:Other),
  names_to = "party",
  values_to = "raw_votes"
)


### Get Percentages from Raw votes
### Data is currently structured in such a way where you can use a sequence to call
### all the values you need for every state to calculate the percentages 
### Each State/Year is in a 4 row chunk with each row being Total, Democratic, Republican, Other

upper_limit_historical <- nrow(election_results_historical)/4

### Create vote_share data frame 
### Originally i was going to have vote share be a new variable but 
### that didn't want to work so I'm doing a work around where I extract the 
### values that I need. the code is basically the same as when I was trying
### to add a new variable but the very end is changed, a comment will make it 
### clear when that is. 

vote_share_historical <- data.frame()

for (i in 1:upper_limit_historical){
  ### Calculate the row numbers we are working with
  row_total <- 4*(i-1) + 1
  
  row_dem <- 4*(i-1) + 2
  
  row_rep <- 4*(i-1) + 3
  
  row_other <- 4*(i-1) + 4
   
  #### Get Raw Vote Totals as Values
  total_votes <- election_results_historical[row_total,4]
  
  democratic_votes <- election_results_historical[row_dem, 4]
  
  republican_votes <- election_results_historical[row_rep, 4]
  
  other_votes <- election_results_historical[row_other, 4]
  
  ### Get Percentages
  dem_vote_share <- democratic_votes / total_votes
  
  rep_vote_share <- republican_votes / total_votes
  
  other_vote_share <- other_votes / total_votes
  
  dem_margin <- dem_vote_share - rep_vote_share
  
  ### Get state and year
  
  current_state <- election_results_historical[row_total, 2]
  cycle <- election_results_historical[row_total, 1]
  
  ### I tried to make a new variable here but that didn't work so I'm making
  ### a whole new dataframe. I know not the best practice but I don't have 
  ### another option
  
  df <- data.frame(year = cycle, state = current_state, dem_vote_share = dem_vote_share, 
                   rep_vote_share = rep_vote_share, other_vote_share = other_vote_share,
                   dem_margin = dem_margin)
  
  names(df) <- c("year", "state", "d", "r", "o", "dem_margin")
  
  vote_share_historical <- rbind(vote_share_historical, df)
}

vote_share_historical <- vote_share_historical |>
  pivot_longer(cols = c(d, r, o), names_to = "party", values_to = "vote_share")

### Historical Data Wrangling, Done!

######################################
# 2024 Data Wrangling from here down #
######################################

### Create National Totals
national_total_2024 <- data.frame(matrix(NA, nrow=1, ncol = 5))

names(national_total_2024) <- c("state", "harris", "trump", "other", "total")

national_total_2024 <- national_total_2024 |>
  mutate(
    state = "National",
    harris = sum(election_results_2024$harris),
    trump = sum(election_results_2024$trump),
    other = sum(election_results_2024$other),
    total = sum(election_results_2024$total)
  )

election_results_2024 <- rbind(election_results_2024, national_total_2024)
election_results_2024 <- election_results_2024 |>
  pivot_longer(cols = c(harris:total), names_to = "party", values_to = "raw_votes")

### Same For Loop used to format historical dataset to vote shares

### Set upper limit
upper_limit_2024 <- nrow(election_results_2024) / 4

### Create dataframe for vote shares

vote_share_2024 <- data.frame()

for (i in 1:upper_limit_2024){
  ### Calculate the row numbers we are working with
  row_total <- 4*(i-1) + 4
  
  row_dem <- 4*(i-1) + 1
  
  row_rep <- 4*(i-1) + 2
  
  row_other <- 4*(i-1) + 3
  
  #### Get Raw Vote Totals as Values
  total_votes <- election_results_2024[row_total,3]
  
  democratic_votes <- election_results_2024[row_dem, 3]
  
  republican_votes <- election_results_2024[row_rep, 3]
  
  other_votes <- election_results_2024[row_other, 3]
  
  ### Get Percentages
  dem_vote_share <- democratic_votes / total_votes
  
  rep_vote_share <- republican_votes / total_votes
  
  other_vote_share <- other_votes / total_votes
  
  dem_margin <- dem_vote_share - rep_vote_share
  
  ### Get state and year
  
  current_state <- election_results_2024[row_total, 1]
  cycle <- 2024
  
  ### I tried to make a new variable here but that didn't work so I'm making
  ### a whole new dataframe. I know not the best practice but I don't have 
  ### another option
  
  df <- data.frame(year = cycle, state = current_state, dem_vote_share = dem_vote_share, 
                   rep_vote_share = rep_vote_share, other_vote_share = other_vote_share,
                  dem_margin = dem_margin)
  
  names(df) <- c("year", "state", "d", "r", "o", "dem_margin")
  
  vote_share_2024 <- rbind(vote_share_2024, df)
}

vote_share_2024 <- vote_share_2024 |>
  pivot_longer(cols = c(d, r, o), names_to = "party", values_to = "vote_share")

##########################################
# Combine 2024 data with historical data #
##########################################
vote_share_data <- rbind(vote_share_2024, vote_share_historical) 

### Relocate Margin Variable to Better Spot
vote_share_data <- vote_share_data |> relocate(dem_margin, .after = vote_share)

### Get rid of . in state names
### these guys are going to kill me
stupid_guys <- vote_share_data |> filter(str_detect(state, "\\.") & !str_detect(state, "Washington"))

dc <- vote_share_data |> filter(str_detect(state, "\\.") & str_detect(state, "Washington"))

### Get rid of the unworthy
vote_share_data <- vote_share_data |> filter(!str_detect(state, "\\."))

stupid_guys <- stupid_guys |> mutate(state = str_replace_all(state, "\\.", " "))

dc <- dc |> mutate(state = "District of Columbia")

### Bring back in the cleaned up states

vote_share_data <- rbind(vote_share_data, stupid_guys, dc)

### Export CSV
write.csv(vote_share_data, paste0(path, "vote_share_data.csv"))
