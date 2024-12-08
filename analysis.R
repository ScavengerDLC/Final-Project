library(tidyverse)
library(lubridate)
library(ggplot2)
library(arrow)
library(dplyr)

################################################################################
################################################################################
## A Note to Me: Don't forget to load in the text analysis csv and have it    ##
## follow the path that the other dataframes are taking in terms of what      ##
## is done to them                                                            ##
################################################################################
################################################################################

#################
# Set Your Path #
#################

path <- "C:/Users/zades/Documents/GitHub/Final-Project/"

#############
# Read CSVs #
#############

polling_data <- read.csv(paste0(path, "polling_data.csv"))

election_data <- read.csv(paste0(path, "vote_share_data.csv"))

sentiment_afinn <- read.csv(paste0(path, "sentiment_afinn.csv"))

sentiment_bing <- read.csv(paste0(path, "sentiment_bing.csv"))


########################
# Remove CSV Index Row #
########################

polling_data <- polling_data |> select(!X)

election_data <- election_data |> select(!X)

sentiment_afinn <- sentiment_afinn |> select(!X)

sentiment_bing <- sentiment_bing |> select(!X)

###################
# Join Dataframes #
###################
model_dataset <- polling_data |> left_join(election_data, by = c("party", "year", "state"))

model_dataset <- model_dataset |> left_join(sentiment_afinn, by = c("candidate", "date", "year")) |>
  left_join(sentiment_bing, by = c("candidate", "date", "year"))

###################################################
# Create Difference between prediction and result #
# Also, set vote_share And dem_margin to be on    #
# 100point scale                                  #
###################################################
model_dataset <- model_dataset |> relocate(pct_estimate, .before = vote_share) |>
  mutate(
    vote_share = vote_share*100,
    dem_margin = dem_margin*100,
    polling_difference = abs(vote_share - pct_estimate)
  )

### Create Difference in margins
model_dataset <- model_dataset |> 
  mutate(margin_error = abs(dem_margin_estimate - dem_margin))


### Make Dates Date Times
model_dataset <- model_dataset |>
  mutate(
    date = ymd(date)
  ) |>
  select(!election_day & !start_date_2024_adjustment)

### Set True False to 1 and 0 respectively
model_dataset <- model_dataset |>
  mutate(incumbant_party = ifelse(incumbant_party == TRUE, 1, 0),
         winner = ifelse(winner == TRUE, 1, 0))

####################################################
# Regression Vote Share ~ pct_estimate + controls #
####################################################

### Republicans
republican <- model_dataset |> filter(party == "r")

republican_vote_margin <- lm(vote_share ~ pct_estimate + date + year + incumbant_party + winner, data = republican)

summary(republican_vote_margin)
### Democrats
democrat <- model_dataset |> filter(party == "d")

democrat_vote_margin <- lm(vote_share ~ pct_estimate + date + year + incumbant_party + winner, data = democrat)

summary(democrat_vote_margin)

##########################################################
# Regression Vote Share ~ pct_estimate Minimal controls #
##########################################################

### Republicans

republican_vote_margin_minimal_controls <- lm(vote_share ~ pct_estimate + year + incumbant_party, data = republican)

summary(republican_vote_margin_minimal_controls)

### Democrats

democrat_vote_margin_minimal_controls <- lm(vote_share ~pct_estimate + year+ incumbant_party, data = democrat)


summary(democrat_vote_margin_minimal_controls)

### Together
vote_share_minimal_controls <-lm(vote_share ~ pct_estimate + year + incumbant_party, data = model_dataset)

summary(vote_share_minimal_controls)

#####################################################
# Regression Vote Share ~ pct_estimate no controls #
#####################################################
### Republicans
republican_vote_margin_no_controls <- lm(vote_share ~ pct_estimate, data = republican)

summary(republican_vote_margin_no_controls)

### Democrats
democrat_vote_margin_no_controls <- lm(vote_share ~ pct_estimate, data = democrat)

summary(democrat_vote_margin_no_controls)

#####################################################
# Regression polling_difference ~ party + controls  #
#####################################################
### Change Party to be Dummy Variable, 1 for democrat, 0 for republican
model_dataset <- model_dataset |> mutate(dummy_party = ifelse(party == "d", 1, 0))

party_error <- lm(polling_difference ~ dummy_party + date + year + incumbant_party + winner, data = model_dataset)

summary(party_error)

##########################################################
# Regression polling_difference ~ party minimal controls #
##########################################################
party_error_minimal_controls <- lm(polling_difference ~ dummy_party + year + incumbant_party, data = model_dataset)

summary(party_error_minimal_controls)

#####################################################
# Regression polling_difference ~ party no controls #
#####################################################
party_error_no_controls <- lm(polling_difference ~ dummy_party, data = model_dataset)

summary(party_error_no_controls)

### I decide to go with minimal controls to avoid overfitting
### control for year to adjust for polling methods changing and 
### incumbancy party to control for incumbancy effects
### Also including states might be rising N too high with too little 
### real variation (alot of earlier dates didn't have as many polls being done)
##########################################################
# Filter model dataset to only national level and redo   #
# previous regressions                                   #
##########################################################
national_data <- model_dataset |>
  filter(state == "National")

##########################################################
# Regression Vote Share ~ pct_estimate Minimal controls #
##########################################################

### Republicans
republican_national <- national_data |> filter(party == "r")

republican_vote_margin_minimal_controls_national <- lm(vote_share ~ pct_estimate + year + incumbant_party, data = republican_national)

summary(republican_vote_margin_minimal_controls_national)

### Democrats
democrat_national <- national_data |> filter(party == "d")
democrat_vote_margin_minimal_controls_national <- lm(vote_share ~ pct_estimate + year + incumbant_party, data = democrat_national)

summary(democrat_vote_margin_minimal_controls_national)

### Together

vote_margin_minimal_controls_national <- lm(vote_share ~ pct_estimate + year + incumbant_party, data = national_data)

summary(vote_margin_minimal_controls_national)


##########################################################
# Regression polling_difference ~ party minimal controls #
##########################################################
party_error_minimal_controls_national <- lm(polling_difference ~ dummy_party + year + incumbant_party, data = national_data)

summary(party_error_minimal_controls_national)


##########################################################
# Filter model dataset to only national 2024 and redo    #
# previous regressions                                   #
##########################################################
national_data_2024 <- model_dataset |>
  filter(year == 2024 & state == "National")

##########################################################
# Regression Vote Share ~ pct_estimate Minimal controls #
##########################################################

### Republicans
republican_national_2024 <- national_data_2024 |> filter(party == "r")

republican_vote_margin_minimal_controls_national_2024 <- lm(vote_share ~ pct_estimate + date, data = republican_national_2024)

summary(republican_vote_margin_minimal_controls_national_2024)

### Democrats
democrats_national_2024 <- national_data_2024 |> filter(party == "d")

democrat_vote_margin_minimal_controls_national_2024 <- lm(vote_share ~ pct_estimate + year+ incumbant_party, data = democrats_national_2024)

summary(democrat_vote_margin_minimal_controls_national_2024)

### Together

vote_share_national_2024 <- lm(vote_share ~ pct_estimate + date, data = national_data_2024)

summary(vote_share_national_2024)

##########################################################
# Regression polling_difference ~ party minimal controls #
##########################################################
party_error_minimal_controls_national <- lm(polling_difference ~ dummy_party + year + incumbant_party, data = national_data)

summary(party_error_minimal_controls_national)

###############################
# Voting Regressions End Here #
###############################

####################################
# Sentiment Regressions Begin Here #
####################################

########################################
# Regression voting_share ~ daily_bing #
########################################
### To save time, I'll use the "minimal controls" set up that I've been using 
### for other regressions

### By Party: All Data

# Republicans
republican_daily_bing <- lm(vote_share ~ daily_bing + year + incumbant_party, data = republican)
summary(republican_daily_bing)

# Democrats
democrat_daily_bing <- lm(vote_share ~ daily_bing + year + incumbant_party, data = democrat)
summary(democrat_daily_bing)

### By Party: National Only

# Republicans
national_republican_daily_bing <- lm(vote_share ~ daily_bing + year + incumbant_party, data = republican_national)
summary(national_republican_daily_bing)
# Democrats
national_democrat_daily_bing <- lm(vote_share ~ daily_bing + year + incumbant_party, data = democrat_national)
summary(national_democrat_daily_bing)

### No Party Separation: All Data

daily_bing_regression <- lm(vote_share ~ daily_bing + year + incumbant_party, data = model_dataset)
summary(daily_bing_regression)

### No Party Separation: National Only
daily_bing_national_regression <- lm(vote_share ~ daily_bing + year + incumbant_party, data = national_data)
summary(daily_bing_national_regression)

### No trend here exept for republicans. Might mean that they are more unbiased covering 
### republicans, while they say a similar thing about dems no matter what. However, it could
### also just be finding a positive relationship by chance. 
### Other sentiment regressions will shed more light

##########################################
# Regression voting_share ~ average_bing #
##########################################

### By Party: All Data

# Republicans
republican_average_bing <- lm(vote_share ~ average_bing + year + incumbant_party, data = republican)
summary(republican_average_bing)

# Democrats
democrat_average_bing <- lm(vote_share ~ average_bing + year + incumbant_party, data = democrat)
summary(democrat_average_bing)

### By Party: National Only

# Republicans
national_republican_average_bing <- lm(vote_share ~ average_bing + year + incumbant_party, data = republican_national)
summary(national_republican_average_bing)
# Democrats
national_democrat_average_bing <- lm(vote_share ~ average_bing + year + incumbant_party, data = democrat_national)
summary(national_democrat_average_bing)

### No Party Separation: All Data

average_bing_regression <- lm(vote_share ~ average_bing + year + incumbant_party, data = model_dataset)
summary(average_bing_regression)

### No Party Separation: National Only
average_bing_national_regression <- lm(vote_share ~ average_bing + year + incumbant_party, data = national_data)
summary(average_bing_national_regression)

### Stronger evidence for party difference in how NYTimes covers candidates vs results 
### but still not visible in national numbers only 

#########################################
# Regression voting_share ~ daily_afinn #
#########################################

### By Party: All Data

# Republicans
republican_daily_afinn <- lm(vote_share ~ daily_afinn + year + incumbant_party, data = republican)
summary(republican_daily_afinn)

# Democrats
democrat_daily_afinn <- lm(vote_share ~ daily_afinn + year + incumbant_party, data = democrat)
summary(democrat_daily_afinn)

### By Party: National Only

# Republicans
national_republican_daily_afinn <- lm(vote_share ~ daily_afinn + year + incumbant_party, data = republican_national)
summary(national_republican_daily_afinn)
# Democrats
national_democrat_daily_afinn <- lm(vote_share ~ daily_afinn + year + incumbant_party, data = democrat_national)
summary(national_democrat_daily_afinn)

### No Party Separation: All Data

daily_afinn_regression <- lm(vote_share ~ daily_afinn + year + incumbant_party, data = model_dataset)
summary(daily_afinn_regression)

### No Party Separation: National Only
daily_afinn_national_regression <- lm(vote_share ~ daily_afinn + year + incumbant_party, data = national_data)
summary(daily_afinn_national_regression)



###########################################
# Regression voting_share ~ average_afinn #
###########################################

### By Party: All Data

# Republicans
republican_average_afinn <- lm(vote_share ~ average_afinn + year + incumbant_party, data = republican)
summary(republican_average_afinn)

# Democrats
democrat_average_afinn <- lm(vote_share ~ average_afinn + year + incumbant_party, data = democrat)
summary(democrat_average_afinn)

### By Party: National Only

# Republicans
national_republican_average_afinn <- lm(vote_share ~ average_afinn + year + incumbant_party, data = republican_national)
summary(national_republican_average_afinn)
# Democrats
national_democrat_average_afinn <- lm(vote_share ~ average_afinn + year + incumbant_party, data = democrat_national)
summary(national_democrat_average_afinn)

### No Party Separation: All Data

average_afinn_regression <- lm(vote_share ~ average_afinn + year + incumbant_party, data = model_dataset)
summary(average_afinn_regression)

### No Party Separation: National Only
average_afinn_national_regression <- lm(vote_share ~ average_afinn + year + incumbant_party, data = national_data)
summary(average_afinn_national_regression)


##############
# GRAPH TIME #
##############

### Vote share vs Average Afinn Score
national_data |>
  ggplot(aes(x = average_afinn, y = vote_share)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Average AFINN Score with National Vote Share",
                               subtitle = "1968-2024") +
  xlab("Average AFINN Score of mentions by NYTimes") +
  ylab("Actual Percent of Vote") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey85"),
    legend.position = "none"
  )

ggsave(paste0(path, "average_afinn_vs_vote_share.png"))
### Vote Share Vs Average Bing
national_data |>
  ggplot(aes(x = average_bing, y = vote_share)) +
  geom_point() +
  geom_smooth(method = lm)

### pct_estimate vs vote_share All
model_dataset |>
  ggplot(aes(x = pct_estimate, y = vote_share)) +
  geom_point() +
  geom_smooth(aes(color = "one"), method = "lm", formula = y ~ x) +
  geom_line(aes(x=50, color = "two"), size = 1) +
  geom_line(aes(y=50, color = "two"), size = 1) +
  scale_color_manual(values = c("#6c008c", "#0229d6")) +
  labs(title = "Vote Estimate from Polls Compared to Actual Vote Results",
       subtitle = "1968-2024") +
  xlab("Estimated Percent of Vote") +
  ylab("Actual Percent of Vote") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey85"),
    legend.position = "none"
  )

ggsave(paste0(path, "vote_estimate_vs_vote_share.png"))

### pct_estimate vs vote_share National
national_data |>
  ggplot(aes(x = pct_estimate, y = vote_share)) +
  geom_point() +
  geom_smooth(aes(color = "one"), method = lm, formula = y ~ x) +
  geom_line(aes(x=50, color = "two"), size = 1) +
  geom_line(aes(y=50, color = "two"), size = 1) +
  scale_color_manual(values = c("#6c008c", "#0229d6")) +
  labs(title = "National Vote Estimate from Polls Compared to Actual National Vote Results",
       subtitle = "1968-2024") +
  xlab("Estimated Percent of Vote") +
  ylab("Actual Percent of Vote") +
  xlim(c(20, 70)) +
  ylim(c(35, 65)) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey85"),
    legend.position = "none"
  )
ggsave(paste0(path, "national_vote_estimate_vs_vote_share.png"))

### pct_estimate vs vote_share National 2024

national_data_2024 |> 
  ggplot(aes(x = pct_estimate, y = vote_share)) +
  geom_point() +
  geom_smooth(aes(color = "one"), method = lm, formula = y ~ x) +
  geom_line(aes(x=50, color = "two"), size = 1) +
  geom_line(aes(y=50, color = "two"), size = 1) +
  scale_color_manual(values = c("#6c008c", "#0229d6")) +
  labs(title = "National Vote Estimate from Polls Compared to Actual National Vote Results",
       subtitle = "2024") +
  xlab("Estimated Percent of Vote") +
  ylab("Actual Percent of Vote") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey85"),
    legend.position = "none"
  )

ggsave(paste0(path, "national_2024_vote_estimate_vs_vote_share.png"))

### With Yaxis adjustment

national_data_2024 |> 
  ggplot(aes(x = pct_estimate, y = vote_share)) +
  geom_point() +
  geom_smooth(aes(color = "one"), method = lm, formula = y ~ x) +
  geom_line(aes(x=50, color = "two"), size = 1) +
  geom_line(aes(y=50, color = "two"), size = 1) +
  scale_color_manual(values = c("#6c008c", "#0229d6")) +
  labs(title = "National Vote Estimate from Polls Compared to Actual National Vote Results 2024") +
  xlab("Estimated Percent of Vote") +
  ylab("Actual Percent of Vote") +
  xlim(c(20, 70)) +
  ylim(c(35, 65)) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey85"),
    legend.position = "none"
  )



### Post 2016

national_data_post_2016 <- national_data |> filter(year >= 2016)

national_data_post_2016 |> 
  ggplot(aes(x = pct_estimate, y = vote_share)) +
  geom_point() +
  geom_smooth(aes(color = "one"), method = lm, formula = y ~ x) +
  geom_line(aes(x=50, color = "two"), size = 1) +
  geom_line(aes(y=50, color = "two"), size = 1) +
  scale_color_manual(values = c("#6c008c", "#0229d6")) +
  labs(title = "National Vote Estimate from Polls Compared to Actual National Vote Results",
       subtitle = "2016-2024") +
  xlab("Estimated Percent of Vote") +
  ylab("Actual Percent of Vote") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey85"),
    legend.position = "none"
  )

ggsave(paste0(path, "national_post_2016_vote_estimate_vs_vote_share.png"))
### Histogram of Margin Errors

model_dataset |>
  filter(state == "National") |>
  ggplot() +
  geom_histogram(aes(x = margin_error), binwidth = 1) +
  labs(title = "N Days where Candidate Lead is within X Percent of Actual Victory Margin",
       subtitle = "National Numbers Only, 1968-2024",
       caption = "One Obervation is One day of National Polling Averages") +
  xlab("Candidate Lead Error") +
  ylab("Number of Days") +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26)) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey85"),
    legend.position = "none"
  )

ggsave(paste0(path, "historical_margin_of_error_histogram.png"))

national_data_post_2016 |>
  ggplot() +
  geom_histogram(aes(x = margin_error), binwidth = 1) +
  labs(title = "N Days where Candidate Lead is within X Percent of Actual Victory Margin",
       subtitle = "National Numbers Only, 2016-2024",
       caption = "One Obervation is One day of National Polling Averages") +
  xlab("Candidate Lead Error") +
  ylab("Number of Days") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey85"),
    legend.position = "none"
  )

ggsave(paste0(path, "post_2016_margin_of_error_histogram.png"))


national_data |>
  filter(year ==2024) |>
  group_by(party) |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = pct_estimate, color = party)) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "538 Election Estimate",
       subtitle = "National Numbers, 2024") +
  xlab("Date") +
  ylab("Estimated Candidate Support") +
  theme(
    panel.grid.major = element_line(color = "grey60"),
    panel.grid.minor = element_line(color = "grey75")
  )

ggsave(paste0(path, "538_estimate.png"))

national_data |>
  filter(year ==2024, !is.na(daily_bing)) |>
  group_by(party) |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = daily_bing, color = party)) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "NYTimes Average Bing Sentiment for Each Candidate",
       subtitle = "2024") +
  xlab("Date") +
  ylab("Bing Sentiment") +
  theme(
    panel.grid.major = element_line(color = "grey60"),
    panel.grid.minor = element_line(color = "grey75")
  )

ggsave(paste0(path, "NYT_Bing.png"))

### calculate Odds of Looking at 538 when the estimate is outside of margin of error
more_6_moe <- model_dataset |> filter(state == "National") |> filter(margin_error > 6) |> nrow()

grave_error_prob <- more_6_moe / model_dataset |> filter(state == "National") |> nrow()

### Write Necessary Dataframes to Disk
write.csv(model_dataset, paste0(path, "model_dataset.csv"))

write.csv(national_data, paste0(path, "national_data.csv"))
