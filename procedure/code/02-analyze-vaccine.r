# Analyze Hurricane Dorian Twitter data, by Joseph Holler, 2019
# following tutorial at:
# https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
# also get advice from the rtweet page: https://rtweet.info/

#install packages for twitter, census, data management, and mapping
packages = c("rtweet","tidycensus","tidytext","maps","RPostgres","igraph","tm",
             "ggplot2","RColorBrewer","rccmisc","ggraph","here", "pkgcond")
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
library(pkgcond)

############# TEMPORAL ANALYSIS #############

#create temporal data frame & graph it

vaccineByHour <- ts_data(vaccine, by="hours")
ts_plot(vaccine, by="hours")


############# NETWORK ANALYSIS #############

#create network data frame. Other options for 'edges' in the network include mention, retweet, and reply
vaccineNetwork <- network_graph(vaccine, c("quote"))

plot.igraph(vaccineNetwork)
#Please, this is incredibly ugly... if you finish early return to this function and see if we can modify its parameters to improve aesthetics

############# TEXT / CONTEXTUAL ANALYSIS #############

# remove urls, fancy formatting, etc. in other words, clean the text content
vaccineText = vaccine %>% select(text) %>% plain_tweets()

# parse out words from tweet text
vaccineWords = vaccineText %>% unnest_tokens(word, text)

# how many words do you have including the stop words?
count(vaccineWords)

# create list of stop words (useless words not worth analyzing)
data("stop_words")

# add "t.co" twitter links to the list of stop words
# also add the twitter search terms to the list
stop_words = stop_words %>%
  # add_row(word="t.co",lexicon = "SMART") %>%
  # add_row(word="vaccine",lexicon = "Search") %>%
  # add_row(word="vaccines",lexicon = "Search") %>%
  # add_row(word="vaccinations",lexicon = "Search") %>% 
  # add_row(word="covid",lexicon = "Search")
  add_row(word="19",lexicon = "Search")

#delete stop words from vaccineWords with an anti_join
vaccineWords =  vaccineWords %>% anti_join(stop_words)

# how many words after removing the stop words?
count(vaccineWords)

# graph frequencies of words
vaccineWords %>%
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
vaccineWordPairs = vaccineText %>%
  mutate(text = removeWords(tolower(text), stop_words$word)) %>%
  unnest_tokens(paired_words, text, token = "ngrams", n = 2) %>%
  separate(paired_words, c("word1", "word2"),sep=" ") %>%
  count(word1, word2, sort=TRUE)

# graph a word cloud with space indicating association.
# you may change the filter to filter more or less than pairs with 30 instances
vaccineWordPairs %>%
  filter(n >= 25 & !is.na(word1) & !is.na(word2)) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network of Tweets about COVID Vaccinations",
       x = "", y = "") +
  theme_void()


############# SPATIAL ANALYSIS #############

# obtain county population estimates using tidycensus
counties <- get_estimates("county",
                          product="population",
                          output="wide",
                          geometry=TRUE, keep_geo_vars=TRUE,
                          key="")

# select only the states you want, with FIPS state codes
# look up fips codes here:
# https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
counties = filter(counties,
                  STATEFP %!in% c('15', '02') )

# save counties to Derived/Public folder
saveRDS(counties, here("data","vaccine", "derived","public","counties.RDS"))

# optionally, load counties from derived/public/counties.RDS
counties = readRDS(here("data","vaccine", "derived","public","counties.RDS"))

# map results with GGPlot
# note: cut_interval is an equal interval classification function, while
# cut_number is a quantile / equal count function
# you can change the colors, titles, and transparency of points
ggplot() +
  geom_sf(data=counties, aes(fill=cut_number(DENSITY,5)), color="grey")+
  scale_fill_brewer(palette="GnBu")+
  guides(fill=guide_legend(title="Population Density"))+
  geom_point(data = vaccine, aes(x=lng,y=lat),
             colour = 'purple', alpha = .2) +
  labs(title = "Locations of Tweets about COVID Vaccinations")+
  theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())