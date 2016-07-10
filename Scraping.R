library(rvest)
library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(tm)
library(twitteR)
library(jsonlite)
library(plyr)
devtools::install_git("https://github.com/okugami79/sentiment140")

# Set up API
consumer_key = "IPMipNDF3FRq7TTPAYbwBEJrM"
consumer_secret = "teFpixr4CvaEgzA1C1o8WxohK3AjEpjqxsSfx5InHhUeideLRC"
access_token = "750634029016346624-rvNoVvVtdljMQLkWictiS8EhGH6Yp9o"
access_secret = "Kgc2sDP2c1oDDowxxdlmSmbv7VZGJV2lw2I1G8rPwlR85"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Scrape top 10 movies from Fandango and clean the data
imdb <- read_html("http://www.fandango.com/boxoffice") %>% html_nodes("table") %>% .[[1]] %>% html_table(trim = TRUE)
names(imdb) <- str_replace_all(names(imdb), "[/[:space:]]", "")
tops <- head(imdb[imdb$WeeksReleased < 4, 2], n = 7L)
a <- str_replace_all(tops, "[/[:space:]]", "")
a <- str_replace_all(a, "\\:.*", "")
a <- str_replace_all(a, "[0-9]+", "")

# Add hashtag to the top ten movies and turn into list
# str_c("#", a, sep = "")
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

# Tweet out the most positive movie
string <- cat("The top three movies for this week are:\n1. ", topthree[1], "\n2. ", topthree[2], "\n3. ", topthree[3])
print(string)
