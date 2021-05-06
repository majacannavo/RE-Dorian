# Analyze Hurricane Dorian Twitter data, by Joseph Holler, 2019
# following tutorial at:
# https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
# also get advice from the rtweet page: https://rtweet.info/

#install packages for twitter, census, data management, and mapping
packages = c("rtweet","tidycensus","tidytext","maps","RPostgres","igraph","tm",
             "ggplot2","RColorBrewer","rccmisc","ggraph","here")
setdiff(packages, rownames(installed.packages()))
install.packages(setdiff(packages,
                         rownames(installed.packages())), quietly=TRUE)

#initialize the libraries. this must be done each time you load the project
library(rtweet)
library(igraph)
library(dplyr)
library(tidytext)
library(tm)
library(tidyr)
library(ggraph)
library(tidycensus)
library(ggplot2)
library(RPostgres)
library(RColorBrewer)
library(DBI)
library(rccmisc)
library(here)

############# TEMPORAL ANALYSIS #############

#create temporal data frame & graph it

dorianByHour <- ts_data(dorian3, by="hours")
ts_plot(dorian3, by="hours")


############# NETWORK ANALYSIS #############

#create network data frame. Other options for 'edges' in the network include mention, retweet, and reply
dorianNetwork <- network_graph(dorian3, c("quote"))

plot.igraph(dorianNetwork)
#Please, this is incredibly ugly... if you finish early return to this function and see if we can modify its parameters to improve aesthetics

############# TEXT / CONTEXTUAL ANALYSIS #############

# remove urls, fancy formatting, etc. in other words, clean the text content
dorianText = dorian %>% select(text) %>% plain_tweets()

# parse out words from tweet text
dorianWords = dorianText %>% unnest_tokens(word, text)

# how many words do you have including the stop words?
count(dorianWords)

# create list of stop words (useless words not worth analyzing)
data("stop_words")

# add "t.co" twitter links to the list of stop words
# also add the twitter search terms to the list
stop_words = stop_words %>%
  add_row(word="t.co",lexicon = "SMART") %>%
  add_row(word="hurricane",lexicon = "Search") %>%
  add_row(word="dorian",lexicon = "Search") %>%
  add_row(word="sharpiegate",lexicon = "Search")

#delete stop words from dorianWords with an anti_join
dorianWords =  dorianWords %>% anti_join(stop_words)

# how many words after removing the stop words?
count(dorianWords)

# graph frequencies of words
dorianWords %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

# separate words and count frequency of word pair occurrence in tweets
dorianWordPairs = dorianText %>%
  mutate(text = removeWords(tolower(text), stop_words$word)) %>%
  unnest_tokens(paired_words, text, token = "ngrams", n = 2) %>%
  separate(paired_words, c("word1", "word2"),sep=" ") %>%
  count(word1, word2, sort=TRUE)

# graph a word cloud with space indicating association.
# you may change the filter to filter more or less than pairs with 30 instances
dorianWordPairs %>%
  filter(n >= 25 & !is.na(word1) & !is.na(word2)) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network of Tweets during Hurricane Dorian",
       x = "", y = "") +
  theme_void()


############# SPATIAL ANALYSIS #############

#first, sign up for a Census API here:
# https://api.census.gov/data/key_signup.html
#replace the key text 'yourkey' with your own key!
counties <- get_estimates("county",
                          product="population",
                          output="wide",
                          geometry=TRUE, keep_geo_vars=TRUE,
                          key="yourkey")

# select only the states you want, with FIPS state codes
# look up fips codes here:
# https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
counties = filter(counties,
                  STATEFP %in% c('54', '51', '50', '47', '45', '44', '42', '39',
                                 '37','36', '34', '33', '29', '28', '25', '24',
                                 '23', '22', '21', '18', '17','13', '12', '11',
                                 '10', '09', '05', '01') )

# save counties to Derived/Public folder
saveRDS(counties, here("data","derived","public","counties.RDS"))

# optionally, load counties from derived/public/counties.RDS
counties = readRDS(here("data","derived","public","counties.RDS"))

# map results with GGPlot
# note: cut_interval is an equal interval classification function, while
# cut_number is a quantile / equal count function
# you can change the colors, titles, and transparency of points
ggplot() +
  geom_sf(data=counties, aes(fill=cut_number(DENSITY,5)), color="grey")+
  scale_fill_brewer(palette="GnBu")+
  guides(fill=guide_legend(title="Population Density"))+
  geom_point(data = dorian, aes(x=lng,y=lat),
             colour = 'purple', alpha = .2) +
  labs(title = "Tweet Locations During Hurricane Dorian")+
  theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


############### UPLOAD RESULTS TO POSTGIS DATABASE ###############

#Connectign to Postgres
#Create a con database connection with the dbConnect function.
#Change the user and password to your own!
con <- dbConnect(RPostgres::Postgres(), dbname='dsm', host='artemis', user='', password='')

#list the database tables, to check if the database is working
dbListTables(con)

#create a simple table for uploading
doriansql <- select(dorian,c("user_id","status_id","text","lat","lng"),
                    starts_with("place"))

#write data to the database
dbWriteTable(con,'dorian',doriansql, overwrite=TRUE)

# try also writing the november tweet data to the database! Add code below:
novembersql <- select(november,c("user_id","status_id","text","lat","lng"),starts_with("place"))
dbWriteTable(con,'november',novembersql,overwrite=TRUE)

# SQL to add geometry column of type point and crs NAD 1983:
# SELECT AddGeometryColumn ('maja','dorian','geom',4269,'POINT',2, false);
# SQL to calculate geometry:
# update dorian set geom = st_transform(st_setsrid(st_makepoint(lng,lat),4326),4269)

#make all lower-case names for counties, because PostGreSQL is not into capitalization

dbWriteTable(con,'counties',lownames(counties), overwrite=TRUE)


############### SPATIAL JOIN AND MAPPING NORMALIZED TWEETS ###############

# Either in R or in PostGIS (via QGIS DB Manager)...

# Count the number of dorian points in each county

# create table dorian_by_county_2 as
# select dorian.*, counties.geoid
# from dorian left join counties
# on st_intersects(dorian.geom, counties.geometry)

# create table dorian_by_county_counted_1 as
# select count(status_id) as count_tweets, geoid
# from dorian_by_county_2 group by geoid;

# Count the number of november points in each county

# SELECT AddGeometryColumn ('maja','november','geom',4269,'POINT',2, false);
# update november set geom = st_transform(st_setsrid(st_makepoint(lng,lat),4326),4269)

# create table november_by_county as
# select november.*, counties.geoid
# from november left join counties
# on st_intersects(november.geom, counties.geometry)

# create table november_by_county_counted as
# select count(status_id) as count_tweets, geoid
# from november_by_county group by geoid;


# Set counties with no points to 0 for the november count

# create table counties_with_dorian as
# select dorian_by_county_counted_1.count_tweets, counties.*
#   from counties left join dorian_by_county_counted_1
# on counties.geoid = dorian_by_county_counted_1.geoid;

# update counties_with_dorian
# set count_tweets = 0
# where count_tweets is null;

# create table counties_with_november as
# select november_by_county_counted.count_tweets, counties.*
#   from counties left join november_by_county_counted
# on counties.geoid = november_by_county_counted.geoid;

# update counties_with_november
# set count_tweets = 0
# where count_tweets is null;

# create table counties_fully_joined as
# select counties_with_november.count_tweets as november_tweets, counties_with_dorian.*
#   from counties_with_dorian left join counties_with_november
# on counties_with_dorian.geoid = counties_with_november.geoid;

# alter table counties_fully_joined rename column count_tweets to dorian_tweets;

# Calculate the normalized difference tweet index (made this up, based on NDVI), where
# ndti = (tweets about storm – baseline twitter activity) / (tweets about storm + baseline twitter activity)
# remember to multiply something by 1.0 so that you'll get decimal devision, not integer division
# also if the denominator would end up being 0, set the result to 0

# alter table counties_fully_joined add column ndti real;
# alter table counties_fully_joined set ndti = 0 where dorian_count = 0 and november_count = 0;

# update counties_fully_joined
# set ndti = (dorian_tweets*1.0 - november_tweets*1.0)/(dorian_tweets*1.0 + november_tweets*1.0)
# where dorian_tweets != 0 or november_tweets != 0;

# Either in QGIS or in R...
# Map the normalized tweet difference index for Hurricane Dorian
# Try using the heatmap symbology in QGIS to visualize kernel density of tweets


############### SAVE RESULTS FROM POSTGIS SPATIAL JOIN ###############

# pull results back from the database, replacing 'tablename' with the name
# of table with results at county level
county_tweets = dbReadTable(con, "tablename")

# save county-level results to derived/public
saveRDS(county_tweets, here("data","derived","public","county_tweets.RDS"))

#disconnect from the database
dbDisconnect(con)
