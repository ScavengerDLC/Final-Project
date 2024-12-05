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


########################
# Remove CSV Index Row #
########################

polling_data <- polling_data |> select(!X)

election_data <- election_data |> select(!X)

###################
# Join Dataframes #
###################
model_dataset <- polling_data |> left_join(election_data, by = c("party", "year", "state"))

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



##############
# GRAPH TIME #
##############

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

### Histogram of Margin Errors

model_dataset |>
  filter(state == "National") |>
  ggplot() +
  geom_histogram(aes(x = margin_error), binwidth = 1) +
  labs(title = "N Days where Candidate Lead is within X Percent of Actual Victory Margin",
       subtitle = "National Numbers Only, 1968-2024",
       caption = "One Obervation is One day of National Polling Averages") +
  xlab("Candidate Lead") +
  ylab("Number of Days") +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26)) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey85"),
    legend.position = "none"
  )

national_data_post_2016 |>
  ggplot() +
  geom_histogram(aes(x = margin_error), binwidth = 1) +
  labs(title = "N Days where Candidate Lead is within X Percent of Actual Victory Margin",
       subtitle = "National Numbers Only, 2016-2024",
       caption = "One Obervation is One day of National Polling Averages") +
  xlab("Candidate Lead") +
  ylab("Number of Days") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey85"),
    legend.position = "none"
  )

more_6_moe <- model_dataset |> filter(state == "National") |> filter(margin_error > 6) |> nrow()

grave_error_prob <- more_6_moe / model_dataset |> filter(state == "National") |> nrow()

###################################################################
# Save Full Model Dataset with all Variables for faster shiny app #
# Grouped by Year                                                 #
###################################################################

### This is the only way I know how to do this right now
### Write csv
write.csv(model_dataset, paste0(path, "model_dataset.csv"))
### read csv as parquet
model_parquet <- open_dataset(
  sources = "model_dataset.csv",
  col_types = schema(facility_id = string()),
  format = "csv"
)
### group by year

model_parquet <- model_parquet |> group_by(model_parquet$year)


### write as partitioned parquet
