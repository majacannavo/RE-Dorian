# search geographic twitter data for Hurricane Dorian
# by Joseph Holler, 2019,2021
# This code requires a twitter developer API token!
# See https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html

# install packages for twitter querying and initialize the library
packages = c("rtweet","here","dplyr","rehydratoR")
setdiff(packages, rownames(installed.packages()))
install.packages(setdiff(packages, rownames(installed.packages())),
                 quietly=TRUE)

library(rtweet)
library(here)
library(dplyr)
library(rehydratoR)

install.packages("tidyverse")
library(tidyverse)

############# SEARCH TWITTER API #############

# reference for search_tweets function:
# https://rtweet.info/reference/search_tweets.html
# don't add any spaces in between variable name and value for your search
# e.g. n=1000 is better than n = 1000
# the first parameter in quotes is the search string
# n=10000 asks for 10,000 tweets
# if you want more than 18,000 tweets, change retryonratelimit to TRUE and
# wait 15 minutes for every batch of 18,000
# include_rts=FALSE excludes retweets.
# token refers to the twitter token you defined above for access to your twitter
# developer account
# geocode is equal to a string with three parts: longitude, latitude, and
# distance with the units mi for miles or km for kilometers

# set up twitter API information with your own information for
# app, consumer_key, and consumer_secret
# this should launch a web browser and ask you to log in to twitter
# for authentication of access_token and access_secret
twitter_token = create_token(
  app = "",                     #enter your app name in quotes
  consumer_key = "",  		      #enter your consumer key in quotes
  consumer_secret = "",         #enter your consumer secret in quotes
  access_token = "",
  access_secret = ""
)

# get tweets for vaccine, searched on May 5, 2021
# this code will no longer work! It is here for reference.
vaccinations_orig_retry3 = search_tweets("(covid AND vaccine) OR (covid and vaccines) OR (covid and vaccinations)",
                       n=200000, include_rts=FALSE,
                       token=twitter_token,
                       geocode="39,-99,1500mi",
                       retryonratelimit=TRUE)

# write results of the original twitter search
write.table(vaccinations_orig_retry3$status_id,
            here("data","raw","public","vaccineids.txt"),
            append=FALSE, quote=FALSE, row.names = FALSE, col.names = FALSE)

# get tweets without any text filter for the same geographic region in May,
# searched on May 4, 2021
# this code will no longer work! It is here for reference.
# the query searches for all verified or unverified tweets, i.e. everything
may_orig = search_tweets("-filter:verified OR filter:verified",
                         n=200000, include_rts=FALSE,
                         token=twitter_token,
                         geocode="39,-99,1500mi",
                         retryonratelimit=TRUE)

# write results of the control twitter search
write.table(may_orig$status_id,
            here("data","raw","public","mayids.txt"),
            append=FALSE, quote=FALSE, row.names = FALSE, col.names = FALSE)

############# LOAD SEARCH TWEET RESULTS  #############

### REVAMP THESE INSTRUCTIONS

# load tweet status id's for vaccine search results
vaccineids =
  data.frame(read.table(here("data","raw","public","vaccineids.txt"),
                        numerals = 'no.loss'))

# load status id's for May general twitter search
mayids =
  data.frame(read.table(here("data","raw","public","mayids.txt"),
                        numerals = 'no.loss'))

# rehydrate vaccine tweets
vaccine_raw = rehydratoR("", "",
                         "",
                         "", vaccineids,
                base_path = NULL, group_start = 1)

# rehydrate may tweets
may_raw = rehydratoR("", "",
                     "",
                     "", mayids,
                        base_path = NULL, group_start = 1)


############# FILTER DORIAN FOR CREATING PRECISE GEOMETRIES #############

# reference for lat_lng function: https://rtweet.info/reference/lat_lng.html
# adds a lat and long field to the data frame, picked out of the fields
# that you indicate in the c() list
# sample function: lat_lng(x, coords = c("coords_coords", "bbox_coords"))

# list and count unique place types
# NA results included based on profile locations, not geotagging / geocoding.
# If you have these, it indicates that you exhausted the more precise tweets
# in your search parameters and are including locations based on user profiles
count(vaccine_raw, place_type)

# convert GPS coordinates into lat and lng columns
# do not use geo_coords! Lat/Lng will be inverted
vaccine = lat_lng(vaccine_raw, coords=c("coords_coords"))
may = lat_lng(may_raw, coords=c("coords_coords"))

# select any tweets with lat and lng columns (from GPS) or
# designated place types of your choosing
vaccine = subset(vaccine,
                place_type == 'city'| place_type == 'neighborhood'|
                  place_type == 'poi' | !is.na(lat))

may = subset(may,
                  place_type == 'city'| place_type == 'neighborhood'|
                    place_type == 'poi' | !is.na(lat))

# convert bounding boxes into centroids for lat and lng columns
vaccine = lat_lng(vaccine,coords=c("bbox_coords"))
may = lat_lng(may,coords=c("bbox_coords"))

# re-check counts of place types
count(vaccine, place_type)

############# SAVE FILTERED TWEET IDS TO DATA/DERIVED/PUBLIC #############

write.table(may$status_id,
            here("data","vaccine", "derived","public","mayids_filtered.txt"),
            append=FALSE, quote=FALSE, row.names = FALSE, col.names = FALSE)

write.table(vaccine$status_id,
            here("data","vaccine", "derived","public","vaccineids_filtered.txt"),
            append=FALSE, quote=FALSE, row.names = FALSE, col.names = FALSE)

############# SAVE TWEETs TO DATA/DERIVED/PRIVATE #############

saveRDS(vaccine, here("data","vaccine", "derived","private","vaccine.RDS"))
saveRDS(may, here("data","vaccine", "derived","private","may.RDS"))
