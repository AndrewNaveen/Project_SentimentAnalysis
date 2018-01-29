install.packages("twitteR")
install.packages("ROAuth")
install.packages("streamR")
getwd()
setwd("/Users/andrewnaveenkumarsekar/Documents/Data_Sets/R_Datasets")
library(twitteR)
library(ROAuth)
library(devtools)
library(dplyr)
library(streamR)

consumer_key <- "Add key"
consumer_secret <- ""
access_token <- ""
access_secret <- ""

TwitterAuth<-setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#SearchTwitter - Searches for the provided string on Twitter
search.twitter<-searchTwitter("Google pixel", n=320,  lang='en')
search.twitter.df<-twListToDF(search.twitter)

#UserTimeLine - Extracts posts by a particular user

user.timeline<-userTimeline("Google", n=3200, includeRts = FALSE)
user.timeline.df<-twListToDF(user.timeline)




