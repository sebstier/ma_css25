#' course: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "Research ethics in CSS and web data collection"
#' author: "Sebastian Stier"
#' class: 2
#' institute: University of Mannheim & GESIS
#' date: "2025-02-26"



# YouTube API ----
library(tuber)
# If you want to use this, do your API verification here: 
#https://developers.google.com/youtube/v3/getting-started

#client_id <- "YOUR-CLIENT-ID"
#client_secret <- "YOUR-CLIENT-SECRET"

yt_oauth(
  app_id = client_id,
  app_secret = client_secret
)

#check out the functions in 
#tuber::
get_stats(video_id = "IXDR2-WWY5Y")

get_video_details(video_id = "IXDR2-WWY5Y")

df_yt <- get_comment_threads(c(video_id = "IXDR2-WWY5Y"), max_results = 20)


# Data visualization using gapminder data ----
library(ggplot2) # ggplot2 is part of the tidyverse and should already be loaded
library(gapminder)

# Create a scatter plot of lifeExp and gdpPercap


# Save the plot


# Create a bar chart showing the GDP/Capita of European countries in the year 2007


# Calculate the (worldwide) average GDP per capita per year and plot this as a bar chart
# Sum the total world population per year. Plot the results in a bar chart for the years 1992-2007


# Trump Twitter Archive ----

# Download the Trump Twitter archive and save the file in the folder "data"
# https://drive.google.com/file/d/1xRKHaP-QwACMydlDnyFPEaFdtskJuBa6/view
library(tidyverse)
list.files("data")
df_trump <- read_csv("data/tweets_01-08-2021.csv")
glimpse(df_trump)
df_trump <- read_csv("data/tweets_01-08-2021.csv",
                     col_types = "ccllcddTl")
# df_trump <- read.csv("data/tweets_01-08-2021.csv", 
#                      colClasses = c("id" = "character")) 
summary(df_trump)
glimpse(df_trump)

# Different data formats
#read_csv
# write_rds(df_trump, "data/df_trump.rds")
# df_trump %>% 
#   write_rds("data/df_trump.rds")
list.files("data")
df_trump <- read_rds("data/df_trump.rds")


# Use group_by() and summarize() to summarize the number of tweets per day
# These two commands are mostly used in combination:
# "group_by" groups columns by a grouping variable
# "summarize" consolidates the mentioned column based on the grouping variable
# into a single row
df_trump %>% 
  # Let's just create a copy of "date"
  # mutate(day = date)
  # Let's create a sum index of favorites and retweets
  # mutate(sum_favs_rets = favorites+retweets)
  mutate(day = as.Date(date)) %>% 
  group_by(day) %>% 
  summarise(sum_tweets = n(),
            sum_favorites = sum(favorites),
            mean_favorites = mean(favorites),
            median_favorites = median(favorites)) %>% 
  arrange(desc(sum_favorites))

# TODO continue here next time

# Some basic text operations ----

# Let's explore the function ?str_detect
# Some tests
test_vec <- c("fakenews", "fake", "FAKE", "FakE", "FAKENEWS", "gesetz", "wahl", "bundestagswahl")
tolower(test_vec)
toupper(test_vec)
str_detect(test_vec, "fake")

# Calculate the occurrence of the words "crazy" or "fake" across devices


# Visualizing the Trump tweets dataset ----

# Use mutate() to create a variable indicating that the tweet was sent via 
#iPhone or Android or another device


# Calculate the share of tweets per device that contain either "crazy" or "fake"


# Create a subset of the data that contains the tweets with either "crazy" or "fake"



# Add the variables to the data frame

# Create a time series plot of the daily share of "crazy" and "fake" over time




