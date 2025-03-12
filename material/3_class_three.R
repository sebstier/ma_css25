#' class: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "Designed Digital Data"
#' author: "Sebastian Stier"
#' lesson: 3
#' institute: University of Mannheim & GESIS
#' date: "2025-03-12"

library(tidyverse)

# Exercise 1: Explore a toy web tracking and survey dataset ----

# Load the browsing data
filename <- "data/toy_browsing.rda"
download.file(url = "https://osf.io/download/52pqe/", destfile = filename)
load(filename)

# Load the survey data
filename <- "data/toy_survey.rda"
download.file(url = "https://osf.io/download/jyfru/", destfile = filename)
load(filename)
rm(filename)

# Load the data
# different ways of storing data
# save() load() #rda
# write_rds() read_rds() #rds from the tidyverse
# write_csv read_csv() #csv from the tidyverse 
list.files("data")
load("data/toy_browsing.rda")
load("data/toy_survey.rda")

# Create object df_wt for further analysis
df_wt <- toy_browsing %>% 
  as_tibble()
table(df_wt$wave)

# START OF HOMEWORK

# Explore the dataset: what is the number of rows, columns, unique persons, 
# what is the covered date range?

# Calculate the mean and median number of website visits per wave and device


# How many of the visits happened on mobile vs. desktop for each wave? 
# What is the share of mobile vs. desktop per wave?


# Plot a time series of the number of visits per day

# END OF HOMEWORK ----

# Exercise 2: Domain augmentation of the web tracking data ----

# What are the top ten visited domains in the data?
## Install the R package adaR: https://gesistsa.github.io/adaR/
library(adaR)
## Apply the relevant function from the package to extract domains from URLs
glimpse(df_wt)
df_wt <- df_wt %>% 
  mutate(domain = adaR::ada_get_domain(url))

# Rank the domains according to their appearance
df_wt %>% 
  group_by(domain) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#TODO: We continue here in the next class


# Inspect whether there are NAs in domain; what can explain the NAs?


# Summarize the number of total visits, Google and Facebook visits per person

  
# Merge the survey data with the number of total visits, Google visits and Facebook visits 
# per panelist_id


# Plot the relation of Facebook visits and age with a point diagram


# Exercise 3: Analysis of news website visits ----

# Merge the news domain information with the web browsing data
## Load U.S. news domain list
news_list <- read.csv("https://raw.githubusercontent.com/ercexpo/us-news-domains/main/us-news-domains-v2.0.0.csv")

# First, check whether there are duplicates in the news data 

# de-duplicate a vector
unique(c("sebastian", "sebastian", "felix"))

# remove the duplicates
news_list <- news_list %>% 
  filter(!duplicated(domain))
nrow(news_list)


# Finally, join the web tracking data with the news lists
news_list$news <- 1

table(df_wt$news, useNA = "a")

# Let's create a dummy for news websites

# Identify the web tracking visits whose URL contains "trump"
## hint: ?str_detect


# Some more explorations of our new variables: where outside of news websites does trump occur?


# most popular trump domains

