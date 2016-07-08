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

# Set up API
consumer_key = “xxxxxxxxxxxxx"
consumer_secret = “xxxxxxxxxxxx"
access_token = “xxxxxxxxxxxxxx"
access_secret = “xxxxxxxxxxxxxxx"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Scrape top 10 movies from Fandango and clean the data
imdb <- read_html("http://www.fandango.com/boxoffice") %>% html_nodes("table") %>% .[[1]] %>% html_table(trim = TRUE)
names(imdb) <- str_replace_all(names(imdb), "[/[:space:]]", "")
topten <- head(imdb[imdb$WeeksReleased < 4, 2], n = 10L)
a <- str_replace_all(topten, "[/[:space:]]", "")
a <- str_replace_all(a, "\\:.*", "")
a <- str_replace_all(a, "[0-9]+", "")

# Add hashtag to the top ten movies and turn into list
str_c("#", a, sep = "")
x = list()

# Search Twitter for movies and clean data
for(movie in a) {
  x <- c(x, searchTwitter(movie, n = 5, lang = "en"))
}
twitter.text = sapply(x, function(t) t$getText())
twitter.text= gsub("[^[:print:]]", "", twitter.text)
twitter.text = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", twitter.text)
twitter.text = gsub("[^[:space:]]*…$", "", twitter.text)

# Group Tweets by movie

# Check sentiment of tweets
sentiment(twitter.text)

# List how many positive sentiment tweets there are per movie

# Compare the movies for positive sentiment

# Tweet out the most positive movie
