#Packages required:
library(twitteR)
library(ggplot2)
library(httr)
library(rjson)
library(tm)
library(gridExtra)
library(lubridate)

# Load any tweets from local machine including one that
# I've already sent you
tweets <- read.csv(file.choose())
#Put the tweets downloaded into a data.frame
#You can skip line below if they are already in a data frame
tweets <- twListToDF(tweets)

#Remove punctuation, numbers, html-links and unecessary spaces:
textScrubber <- function(dataframe) {
  
  dataframe$text <-  gsub("-", " ", dataframe$text)
  dataframe$text <-  gsub("&", " ", dataframe$text)
  dataframe$text <-  gsub("[[:punct:]]", " ", dataframe$text)
  dataframe$text <-  gsub("[[:digit:]]", "", dataframe$text)
  dataframe$text <-  gsub("http\\w+", "", dataframe$text)
  dataframe$text <-  gsub("\n", " ", dataframe$text)
  dataframe$text <-  gsub("[ \t]{2,}", "", dataframe$text)
  dataframe$text <-  gsub("^\\s+|\\s+$", "", dataframe$text)
  dataframe$text <-  tolower(dataframe$text)
  
  return(dataframe)
}

#Pass the text scrubber function
tweets <- textScrubber(tweets)

# For now we are going to skip the entire lot for the device function

device <- function(dataframe) {
  tot_all <- length(dataframe$statusSource)
  
  Web <- 100*(length(grep("Twitter Web Client", dataframe$statusSource))/tot_all)
  Windows <- 100*(length(grep("Twitter For Windows Phone", dataframe$statusSource))/tot_all)
  TweetDeck <- 100*(length(grep("TweetDeck", dataframe$statusSource))/tot_all)
  iPhone <- 100*(length(grep("Twitter for iPhone", dataframe$statusSource))/tot_all)
  iPad <- 100*(length(grep("Twitter for iPad", dataframe$statusSource))/tot_all)
  Blackberry <- 100*(length(grep("Twitter for BlackBerry", dataframe$statusSource))/tot_all)
  Tweetbot <- 100*(length(grep("Tweetbot for i", dataframe$statusSource))/tot_all)
  Hootsuite <- 100*(length(grep("Hootsuite", dataframe$statusSource))/tot_all)
  Android <- 100*(length(grep("Twitter for Android", dataframe$statusSource))/tot_all)
  Ads <- 100*(length(grep("Twitter Ads", dataframe$statusSource))/tot_all)
  M5 <- 100*(length(grep("Mobile Web (M5)", dataframe$statusSource))/tot_all)
  Mac <- 100*(length(grep("Twitter for Mac", dataframe$statusSource))/tot_all)
  Facebook <- 100*(length(grep("Facebook", dataframe$statusSource))/tot_all)
  Instagram <- 100*(length(grep("Instagram", dataframe$statusSource))/tot_all)
  IFTT <- 100*(length(grep("IFTT", dataframe$statusSource))/tot_all)
  Buffer <- 100*(length(grep("Buffer", dataframe$statusSource))/tot_all)
  CoSchedule <- 100*(length(grep("CoSchedule", dataframe$statusSource))/tot_all)
  GainApp <- 100*(length(grep("Gain App", dataframe$statusSource))/tot_all)
  MobileWeb <- 100*(length(grep("Mobile Web", dataframe$statusSource))/tot_all)
  iOS <- 100*(length(grep("iOS", dataframe$statusSource))/tot_all)
  OSX <- 100*(length(grep("OS X", dataframe$statusSource))/tot_all)
  Echofon <- 100*(length(grep("Echofon", dataframe$statusSource))/tot_all)
  Fireside <- 100*(length(grep("Fireside Publishing", dataframe$statusSource))/tot_all)
  Google <- 100*(length(grep("Google", dataframe$statusSource))/tot_all)
  MailChimp <- 100*(length(grep("MailChimp", dataframe$statusSource))/tot_all)
  TwitterForWebsites <- 100*(length(grep("Twitter for Websites", dataframe$statusSource))/tot_all)
  
  percentages <- data.frame(Web, Windows, TweetDeck, iPhone, iPad, Blackberry, Tweetbot,
                            Hootsuite, Android, Ads, M5, Mac, Facebook, Instagram,
                            IFTT, Buffer, CoSchedule, GainApp, MobileWeb, iOS, OSX,
                            Echofon, Fireside, Google, MailChimp, TwitterForWebsites)
  return(percentages)
}

# Pass the device function
Device <- sort(device(tweets), decreasing = T)

#Sort most popular devices
Device[ ,1:5]
