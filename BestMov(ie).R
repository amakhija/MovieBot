#' 
#' Title: The Best Mov(ie) Bot
#' Authors: Anushka Makhija, Stanton Leavitt, Vance Matthews
#' Date: 11 July 2016
#' 
#' DESCRIPTION: Source code for a twitter bot that analyzes tweets of new movies
#' for sentiment and tweets out the movie with the highest percentage
#' of positive senitment. Sentiment analysis is done with Sentiment140.

library(rvest)
library(stringr)
library(tm)
library(twitteR)
devtools::install_git("https://github.com/okugami79/sentiment140")
library(sentiment)

# Set up API - insert API access code here
consumer_key = "*************************"
consumer_secret = "**************************************************"
access_token = "**************************************************"
access_secret = "*********************************************"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Scrape the top 10 movies from Fandango and clean the data
fandango <- read_html("http://www.fandango.com/boxoffice") %>% html_nodes("table") %>% .[[1]] %>% html_table(trim = TRUE)
names(fandango) <- str_replace_all(names(fandango), "[/[:space:]]", "")
tops <- head(fandango[fandango$WeeksReleased < 4, 2], n = 7L)
a <- str_replace_all(tops, "[/[:space:]]", "")
a <- str_replace_all(a, "\\:.*", "")
a <- str_replace_all(a, "[0-9]+", "")

# Turn top ten movies into list & count reps of each film.
x = list()
names <- character()
reps <- numeric()

for(movie in a) {
  y <- searchTwitter(movie, n = 300, lang = "en")
  x <- c(x, y)
  names <- c(names, rep(movie, length(y)))
  reps <- c(reps, length(y))
}

# Create new data frame for movie tweet count (in case API cannot return enough tweets)
newdf <- data.frame(moviename = a,
                    moviecount = reps)

twitter.text = sapply(x, function(t) t$getText())
twitter.text= gsub("[^[:print:]]", "", twitter.text)
twitter.text = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", twitter.text)
twitter.text = gsub("[^[:space:]]*…$", "", twitter.text)

# Check sentiment of tweets
twitter.sentiment <- sentiment(twitter.text)
twitter.sentiment$moviename <- names

# Vector of the number of positive tweets per movie
x2 <- numeric()
for(movie in a) {
  x2 <- c(x2, nrow(subset(subset(twitter.sentiment, moviename == movie), polarity == "positive")))
}
# Vector of the number of total tweets per movie
x3 <- numeric()
for(movie in a) {
  x3 <- c(x3, nrow(subset(twitter.sentiment, moviename == movie)))
}

# Calculate percentage of positive tweets compared to total tweets
proportion <- x2/x3

# Store proportion in a data frame
finalcount <- data.frame(Movie.Name = tops,
                         prop.count = proportion)

# Order the data frame in terms of sentiment count
finalcount <- finalcount[order(finalcount$prop.count, decreasing = TRUE),]

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


# Tweet out the top 3 most positive movies
string <- as.String(c(sample(tweets, 1), paste("1.", topthree[1]), paste("2.", topthree[2]), paste("3.", topthree[3])))
tweet(string)
