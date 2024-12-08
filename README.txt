This Project takes 538 polling data from 1968-2024, election data from the same time period and New York Times Articles from the same time period and 
compares NYTimes article sentiment's ability to predict elections to polls'. 

Made on R Version 4.4.1

Downloaded CSV Explanation:
All CSVs NOT found in article_info and Data are generated through relatively quick code. 

CSVs found in /article_info are gathered through get_nyt_times_articles.R (more on this later) 

CSVs found in /Data were downloaded from various webpages.
	election_results_1968_2020.csv came from this link:  https://doi.org/10.6068/DP19394176D0E19
		- Some values were missing. Obtained from this website that claimed to get info from same source https://uselectionatlas.org/RESULTS/state.php?year=1980&fips=2&f=0&off=0&elect=0 
	election_results_2024.csv came from this link: https://apnews.com/projects/election-results-2024/?office=P
		- Manual entry. I couldn't web scrape it because smaller vote totals from third parties were hidden behind button that needed to be clicked by a mouse :(
		- Copy and paste from inspect element, no typing. 

	pres_pollaverages_1968-2016.csv, presidential_general_averages.csv, presidential_poll_averages_2020.csv came from the 538 GitHub page: https://github.com/fivethirtyeight/data/tree/master/polls



Explanation of R-Script Files and Order to Run in. Run in the order they are explained here CSVs are explained as they are generated

get_nyt_articles.R: 

	What needs to be changed on your end: Path at start, NYTimes API key on line 114 (see Summary of Function for more info)

	Summary of Function: 

	This File gets article text and URLs of 100 most recent articles written when given the search term "First_Name+Last_Name" where each name is a presidential candidate
	Note on method of getting articles: Pre 1984, articles are not stored with their section so all articles are just listed as "archive" section and not World News, Sports, Opinion, ect. 

	When getting URLs, you should make a NYTimes api key here: https://developer.nytimes.com/get-started
	There is a non-zero chance that the API key that is in the code won't have any uses up. A large comment is made when you need to change it, but if you can't find it look to line 114.

	The For loop immediately after the key input gets article URLs, it will take approximately an hour and a half to run if you generate your own URLs, luckily I have a CSV in /article_data so you don't need to do that.
	
	There are two conditions in the forloop that adjusts based on if the article is before 1984. This is because the search query is different for archive years. 

	Sys.sleep is at the end of the loop to comply with NYTimes terms and conditions. Max 5 queries per minute and 500 per day. 

	The second step is to extract the text. This is done through scraping the webpages of the URLs gathered. This is also spaced out in time to comply with NYTimes terms and conditions. Due to the large number of articles being 
	scraped, there is a high likelihood that you will run into a network error (504 or similar error). This isn't anything wrong with my code and is just the website crying occasionally. The best we can do is emotionally support the 
	page, tell it that it is ok and try again. When doing this the first time I adjusted the lower bound of the forloop to make up for any errors. 
	
	Also, generates presidential_candidates.csv, a valuable csv with some information where each row is a candidate with information like year that they ran, party, if they were the incumbent, and what their "Harris adjustment" was. 
	The "Harris adjustment" refers to the fact that we are putting the start of the campaign at 107 days before the election (when Harris officially started running) this is done to keep the units of analysis the same. 

	CSVs Generated: 
	/article_info/article_urls.csv - list of URLs with date published, candidate it is associated with and the URL
	/article_info/article_text.csv - List of text with similar information as URLs, but no URL
	presidential_candidates.csv - list of candidates that ran for president in the 2 parties between 1968 and 2024 with various indemnifying information about the campaign such as year and if they were the incumbent party. 
	
	

poll_result_wrangling.R: 

	What needs to be changed on your end: Path

	Summary of Function:
	This R File cleans the data from the Election Results and Polling Data

	Combines CSVs into vote_share_data and polling_data

	Vote_share_data is a table with election data where each observation is at the State, Candidate, Year level

	Polling_data is a table with polling data where each observation is at the State, candidate, Date level 

	CSVs Generated: 
	vote_share_data.csv: vote_share_data dataframe turned into a csv
	polling_data.csv: polling_data dataframe turned into a csv

sentiment.R:

	What needs to be changed on your end: Path

	Summary of Function:
	This R File extracts the sentiment of NYTimes authors when speaking about each candidate

	Takes article_text.csv and extracts sentences from articles and looks for candidate's names. it then looks at the word tokens and finds parent and children tokens but grandparent and grandchildren tokens. This is because despite 	the many articles being taken, the complex structure of the sentences required us to go deeper into the connections to start finding non-name or connecting words. I did the analysis like this, rather than a more blunt approach 	because newspaper articles on the election will often mention both campaigns, and I wanted to represent this in my analysis by looking deeper into the structure of the sentence.

	It then takes those word lists and attaches Bing sentiment and Afinn sentiment. These two were chosen because they were the most complete (~10% of words gathered using the above method)

	CSVs Generated:
	all_words_candidates.csv: A csv of all words gathered in the first step, this is before filtering out NA's and junk words. I saved this here because it took a long time to generate and if it got broken I didn't want to run the 	code over and over again.
	
	sentiment_afinn.csv: a csv of average afinn sentiment for each candidate over the course of the campaign and for each candidate on any particular day

	sentiment_bing.csv: a csv of average bing sentiment for each candidate over the course of the campaign and for each candidate on any particular day

analysis.r:
	This R file combines all generated CSVs into a masterdata frame and performs various regressions and makes various graphs. 
	
	CSVs Generated:
	model_dataset.csv: CSV with all information at the State, Candidate, Date Level
	national_data.csv: Subset of model_dataset.csv that are only national numbers

	PNGs Generated:
	Quite a few. They follow a naming convention. 
	Prefixs:
	national_xxx means national graph. national_2016_xxx means national post 2016 graph. national_2024_xxx means national 2024 graph. historical_xxx means all data.  

	Type of Graph:
	x_vs_y means scatterplot of x vs y. xxxx_histogram means histogram of X.  
	 
	
final-project-app/App.R
	A shiny app that takes the model_dataset.csv and generates 3 interactive graphs in 2 different tabs. The first is a historical polling average trend graph for a user input year range. It shows a scatter plot of Vote Estimate 	from Polls vs Actual Results. The second tab is a time series that compares the polling averages to NYTimes Bing Daily Average Sentiment for a user input year.  



There are simply too many that aren't saved to the disk tables to explain. They follow naming conventions. Any table is a derivative of one of the tables explained above in some way that is described by the name. For example. republican_national is a filter of the model_dataset.csv function that searches for only republican national data 


Thank you for looking over my project! It can be quite chaotic at places, however it all comes together and works. 