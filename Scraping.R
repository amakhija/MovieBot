#' 
#' The Best Mov(ie) Bot
#' By Anushka, Stanton, Vance
#' 11 July 2016
#' 
#' DESCRIPTION: Source code for a twitter bot that analyzes tweets of new movies
#' for sentiment and tweets out the movie with the highest percentage
#' of positive senitment. Sentiment analysis is done with Sentiment140.
#'
library(rvest)
library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(tm)
library(twitteR)
library(jsonlite)
library(plyr)
library(sentiment)
devtools::install_git("https://github.com/okugami79/sentiment140")

# Set up API
consumer_key = "xxxxxxxxxxxxx"
consumer_secret = "xxxxxxxxxxxx"
access_token = "xxxxxxxxxxxx"
access_secret = "xxxxxxxxxxxx"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Scrape top 10 movies from Fandango and clean the data
fandango <- read_html("http://www.fandango.com/boxoffice") %>% html_nodes("table") %>% .[[1]] %>% html_table(trim = TRUE)
names(fandango) <- str_replace_all(names(fandango), "[/[:space:]]", "")
tops <- head(fandango[fandango$WeeksReleased < 4, 2], n = 7L)
a <- str_replace_all(tops, "[/[:space:]]", "")
a <- str_replace_all(a, "\\:.*", "")
a <- str_replace_all(a, "[0-9]+", "")

# Turn into list and clean up tweets
x = list()

for(movie in a) {
  x <- c(x, searchTwitter(movie, n = 5, lang = "en"))
}

twitter.text = sapply(x, function(t) t$getText())
twitter.text= gsub("[^[:print:]]", "", twitter.text)
twitter.text = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", twitter.text)
twitter.text = gsub("[^[:space:]]*…$", "", twitter.text)

# Check sentiment of tweets
twitter.sentiment <- sentiment(twitter.text)

# Add in Names of Movies
twitter.sentiment$moviename <- rep(a, each = 5)

# Group the tweets by the number of positive tweets for each movie
x2 <- numeric()
for(movie in a) {
  x2 <- c(x2, nrow(subset(subset(twitter.sentiment, moviename == movie), polarity == "positive")))
}

# Store it in a data frame
finalcount <- data.frame(Movie.Name = tops,
                    sentiment.count = x2)

# Order the data frame in terms of sentiment count
finalcount <- finalcount[order(finalcount$sentiment.count, decreasing = TRUE),]

# Get the names for the top three movies and store them in a vector
topthree <- as.character(head(finalcount, n = 3L)$Movie.Name)

# List of phrases to tweet out:
tweets <- c("Results are in! This week's top movies:",
            "Wondering what movie to see this week? Pick between:",
            "Tell us if we're right! Are these the week's best movies?",
            "Don't miss out on this week's best movies:",
            "Tired of work and need a movie break? Check out:",
            "Are these the best movies of the week? Go watch and let us know:",
            "#replytweet Are these the week's best films?")


# Tweet out the most positive movie
string <- as.String(c(sample(tweets, 1), paste("1.", topthree[1]), paste("2.", topthree[2]), paste("3.", topthree[3])))
# string
tweet(string)
