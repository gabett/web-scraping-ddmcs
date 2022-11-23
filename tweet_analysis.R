# Data Wrangling
library(tidyverse)
library(dplyr)
# For graph and visualization
library(tidygraph)
library(ggraph)
library(igraph)
library(rtweet)


bearer = "AAAAAAAAAAAAAAAAAAAAAJ16bQEAAAAAinIZbtcXIuBJdvB3fQfpCFqdZuw%3DanZdgCl7cckwtg66jDEPk8ua0Im4GKywbNsKXeabJpV9BRsI53"

auth <- rtweet_app()
auth_setup_default()

rt_all <-
  search_tweets(
    "#Crypto",
    n = 50000,
    include_rts = TRUE,
    retryonratelimit = TRUE
  )

rt_all$

# keep only the screen_name and the retweet_screen_name
rt <- rt_all[, c("screen_name", "retweet_screen_name")]

# remove the NA rows and duplicates
rt <- rt %>% na.omit() %>% distinct_all()

# We can have a look at the tweets that got the most likes.
View(rt_all %>% arrange(desc(favorite_count)) %>% head(10))


# Graph analysis ####
# create the directed network graph
crypto_network <-
  graph_from_edgelist(el = as.matrix(rt), directed = TRUE)
# get the in-degree i.e. users who are re-tweeted
in_degree <- degree(crypto_network, mode = c("in"))
# get the top 10 users in terms of in degree
as.data.frame(in_degree) %>% arrange(desc(in_degree)) %>% head(10)


"Apart from the in-degree centrality, there is also the out-degree centrality, meaning the users who retweet.
Let’s see the accounts with the highest out-degree centrality."
# get the out-degree i.e. users who re-tweet
out_degree <- degree(crypto_network, mode = c("out"))
# get the top 10 users in terms of out degree
as.data.frame(out_degree) %>% arrange(desc(out_degree)) %>% head(10)

"Finally, we can get the betweenness centrality, which is a measurement of a user’s influence in the flow of information in the social network.
We can consider them as the bridge between two key players."
# between centrality
bt_degree <- betweenness(crypto_network, directed = TRUE)
as.data.frame(bt_degree) %>% arrange(desc(bt_degree)) %>% head(10)