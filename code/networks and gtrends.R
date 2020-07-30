# load old gtrendsR (1.4.5) first, then load the library
library(tidyverse)
library(devtools)
#install_version("gtrendsR", version = "1.4.5")
library(gtrendsR)


# Process:
#   1. Show full trend of relative interest via Google searches for "coronavirus"

# create new data frame
covid_trends <- gtrends(keyword = "coronavirus", 
                        geo = "US", 
                        gprop = "web",
                        time = "2019-12-01 2020-07-30")[[1]] %>% 
  mutate(hits = replace(hits, hits < 0, 0)) 


# viz
covid_trends %>% 
  ggplot(aes(x = date, 
             y = as.numeric(hits), 
             group = keyword, 
             col = keyword)) +
  geom_line() + 
  labs(title = "Google Trends Web Searches for 'coronavirus'", 
       subtitle = "December 2019 - July 2020", 
       x = "Time", 
       y = "Relative Interest") +
  theme_minimal()

#   2. Show choropleths of US in relative interest by month (start with 1-16-2020; first search)

# first, run the original search (and don't store as DF), but for our narrower time window
covid_trends_narrow <- gtrends(keyword = "coronavirus", 
                                               geo = "US", 
                                               gprop = "web",
                                               time = "2019-01-16 2020-07-30")

# get state polygon coordinates from the maps library
library(maps)

state <- map_data("state")

# create subset of data to make names from states data and search data consistent for mapping
sub_covid <- covid_trends_narrow$interest_by_region %>%
  mutate(region = tolower(location)) %>%
  filter(region %in% state$region) %>%
  select(region, hits)

# viz
ggplot() +
  geom_map(data = state,
           map = state,
           aes(x = long, y = lat, map_id = region),
           fill="darkgray", 
           color="darkgray"
  ) +
  geom_map(data = sub_covid,
           map = state,
           aes(fill = hits, map_id = region),
           color="darkgray", size = 0.1
  ) +
  scale_fill_continuous(low = '#cbcaca', # my favorite shade of gray
                        high = '#013364') + # my favorite shade of blue
  labs(title = "Relative Interest in 'coronavirus' by State",
       subtitle = "Google Trends Web Searches, Jan 16, 2020 - July 30, 2020",
       fill = "Relative\nInterest") +
  theme_void() 



# THUS, interested in the dynamics of how people's views of coronavirus/COVID changed over the short course of the virus (7 month window here). Changes are considered here as changes in network topology of RELATED QUERIES with the constant base query "coronavirus" (especially as COVID-19 cropped up later; coronavirus has been more constant)


#   3. Quant analysis --> first gather data: search in 7-day chunks of time and then store the related queries (things they also searched for) in individual tibbles; then rbind full set for mining

# selected start of 01-16-2020, as no other searches with "coronavirus" appeared in the US prior to this date

set.seed(1234)

## jan 16 - jan 22
c_1_22 <- gtrends(keyword = "coronavirus", 
                        geo = "US", 
                        time = "2020-01-16 2020-01-22") 

q_1_22 <- c_1_22$related_queries %>%
  as_tibble() %>% 
  mutate(week = 1)

## jan 23 - jan 29
c_1_29 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-01-23 2020-01-29") 

q_1_29 <- c_1_29$related_queries %>%
  as_tibble() %>% 
  mutate(week = 2)

## jan 30 - feb 5
c_2_5 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-01-30 2020-02-05") 

q_2_5 <- c_2_5$related_queries %>%
  as_tibble() %>% 
  mutate(week = 3)

## feb 6 - feb 12
c_2_12 <- gtrends(keyword = "coronavirus", 
                 geo = "US", 
                 time = "2020-02-06 2020-02-12") 

q_2_12 <- c_2_12$related_queries %>%
  as_tibble() %>% 
  mutate(week = 4)

## feb 13 - feb 19
c_2_19 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-02-13 2020-02-19") 

q_2_19 <- c_2_19$related_queries %>%
  as_tibble() %>% 
  mutate(week = 5)

## feb 20 - feb 26
c_2_26 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-02-20 2020-02-26") 

q_2_26 <- c_2_26$related_queries %>%
  as_tibble() %>% 
  mutate(week = 6)

## feb 27 - march 4
c_3_4 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-02-27 2020-03-04") 

q_3_4 <- c_3_4$related_queries %>%
  as_tibble() %>% 
  mutate(week = 7)

## march 5 - march 11
c_3_11 <- gtrends(keyword = "coronavirus", 
                 geo = "US", 
                 time = "2020-03-05 2020-03-11") 

q_3_11 <- c_3_11$related_queries %>%
  as_tibble() %>% 
  mutate(week = 8)

## march 12 - march 18
c_3_18 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-03-12 2020-03-18") 

q_3_18 <- c_3_18$related_queries %>%
  as_tibble() %>% 
  mutate(week = 9)

## march 19 - march 25
c_3_25 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-03-19 2020-03-25") 

q_3_25 <- c_3_25$related_queries %>%
  as_tibble() %>% 
  mutate(week = 10)

## march 26 - april 1
c_4_1 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-03-26 2020-04-01") 

q_4_1 <- c_4_1$related_queries %>%
  as_tibble() %>% 
  mutate(week = 11)

## april 2 - april 8
c_4_8 <- gtrends(keyword = "coronavirus", 
                geo = "US", 
                time = "2020-04-02 2020-04-08") 

q_4_8 <- c_4_8$related_queries %>%
  as_tibble() %>% 
  mutate(week = 12)

## april 9 - april 15
c_4_15 <- gtrends(keyword = "coronavirus", 
                geo = "US", 
                time = "2020-04-09 2020-04-15") 

q_4_15 <- c_4_15$related_queries %>%
  as_tibble() %>% 
  mutate(week = 13)

## april 16 - april 22
c_4_22 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-04-16 2020-04-22") 

q_4_22 <- c_4_22$related_queries %>%
  as_tibble() %>% 
  mutate(week = 14)

## april 23 - april 29
c_4_29 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-04-23 2020-04-29") 

q_4_29 <- c_4_29$related_queries %>%
  as_tibble() %>% 
  mutate(week = 15)

## april 30 - may 6
c_5_6 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-04-30 2020-05-06") 

q_5_6 <- c_5_6$related_queries %>%
  as_tibble() %>% 
  mutate(week = 16)

## may 7 - may 13
c_5_13 <- gtrends(keyword = "coronavirus", 
                 geo = "US", 
                 time = "2020-05-07 2020-05-13") 

q_5_13 <- c_5_13$related_queries %>%
  as_tibble() %>% 
  mutate(week = 17)

## may 14 - may 20
c_5_20 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-05-14 2020-05-20") 

q_5_20 <- c_5_20$related_queries %>%
  as_tibble() %>% 
  mutate(week = 18)

## may 21 - may 27
c_5_27 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-05-21 2020-05-27") 

q_5_27 <- c_5_27$related_queries %>%
  as_tibble() %>% 
  mutate(week = 19)

## may 28 - june 3
c_6_3 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-05-28 2020-06-03") 

q_6_3 <- c_6_3$related_queries %>%
  as_tibble() %>% 
  mutate(week = 20)

## june 4 - june 10
c_6_10 <- gtrends(keyword = "coronavirus", 
                 geo = "US", 
                 time = "2020-06-04 2020-06-10") 

q_6_10 <- c_6_10$related_queries %>%
  as_tibble() %>% 
  mutate(week = 21)

## june 11 - june 17
c_6_17 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-06-11 2020-06-17") 

q_6_17 <- c_6_17$related_queries %>%
  as_tibble() %>% 
  mutate(week = 22)

## june 18 - june 24
c_6_24 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-06-18 2020-06-24") 

q_6_24 <- c_6_24$related_queries %>%
  as_tibble() %>% 
  mutate(week = 23)

## june 25 - july 1
c_7_1 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-06-25 2020-07-01") 

q_7_1 <- c_7_1$related_queries %>%
  as_tibble() %>% 
  mutate(week = 24)

## july 2 - july 8
c_7_8 <- gtrends(keyword = "coronavirus", 
                 geo = "US", 
                 time = "2020-07-02 2020-07-08") 

q_7_8 <- c_7_8$related_queries %>%
  as_tibble() %>% 
  mutate(week = 25)

## july 9 - july 15
c_7_15 <- gtrends(keyword = "coronavirus", 
                 geo = "US", 
                 time = "2020-07-09 2020-07-15") 

q_7_15 <- c_7_15$related_queries %>%
  as_tibble() %>% 
  mutate(week = 26)

## july 16 - july 22
c_7_22 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-07-16 2020-07-22") 

q_7_22 <- c_7_22$related_queries %>%
  as_tibble() %>% 
  mutate(week = 27)

## july 23 - july 29
c_7_29 <- gtrends(keyword = "coronavirus", 
                  geo = "US", 
                  time = "2020-07-23 2020-07-29") 

q_7_29 <- c_7_29$related_queries %>%
  as_tibble() %>% 
  mutate(week = 28)

## july 30 - august 5
#c_8_5<- gtrends(keyword = "coronavirus", 
#                  geo = "US", 
#                  time = "2020-07-30 2020-08-05") 
#
#q_8_5 <- c_8_5$related_queries %>%
#  as_tibble() %>% 
#  mutate(week = 29)

# full set
full <- rbind(q_1_22, q_1_29, 
              q_2_5, q_2_12, q_2_19, q_2_26,
              q_3_4, q_3_11, q_3_18, q_3_25,
              q_4_1, q_4_8, q_4_15, q_4_22, q_4_29,
              q_5_6, q_5_13, q_5_20, q_5_27,
              q_6_3, q_6_10, q_6_17, q_6_24,
              q_7_1, q_7_8, q_7_15, q_7_22, q_7_29)

# create month feature 
full <- full %>% 
  mutate(month = case_when(week == 1 ~ 1, week == 2 ~ 1,
                           week == 3 ~ 2, week == 4 ~ 2, week == 5 ~ 2, week == 6 ~ 2,
                           week == 7 ~ 3, week == 8 ~ 3, week == 9 ~ 3, week == 10 ~ 3,
                           week == 11 ~ 4, week == 12 ~ 4, week == 13 ~ 4, week == 14 ~ 4, week == 15 ~ 4,
                           week == 16 ~ 5, week == 17 ~ 5, week == 18 ~ 5, week == 19 ~ 5,
                           week == 20 ~ 6, week == 21 ~ 6, week == 22 ~ 6, week == 23 ~ 6,
                           week == 24 ~ 7, week == 25 ~ 7, week == 26 ~ 7, week == 27 ~ 7, week == 28 ~ 7)
  )


# load some library for networks adn text processing
library(tm)
library(skimr)
library(igraph)
library(here)

theme_set(theme_minimal())

# do the following first for each month; THEN for the whole set at the end with color and community detection for the whole set

month1 <- full %>% 
  filter(month == 1)

month2 <- full %>% 
  filter(month == 2)

month3 <- full %>% 
  filter(month == 3)

month4 <- full %>% 
  filter(month == 4)

month5 <- full %>% 
  filter(month == 5)

month6 <- full %>% 
  filter(month == 6)

month7 <- full %>% 
  filter(month == 7)


# start network modeling
set.seed(1234)

# Build corpus and clean a bit
{ 
  corpus <- iconv(month7$value, # only need to change the "month*"; then run all subsequent code
                  to = "utf-8-mac")
  corpus <- Corpus(VectorSource(corpus))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  #corpus <- tm_map(corpus, removeNumbers)
  cleanset <- tm_map(corpus, removeWords, stopwords('english'))
  for (j in seq(cleanset)) {
    cleanset[[j]] <- gsub("and", " ", cleanset[[j]]) 
    cleanset[[j]] <- gsub("coronavirus", " ", cleanset[[j]]) 
  }
  cleanset <- tm_map(cleanset, stripWhitespace)
}

tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)

# inspect first 10 rows and first 10 columns
tdm[1:10,1:10] # numbers are how many times the term/word appeared in each tweet
#tdm <- tdm[rowSums(tdm) > 50, ] # reduce size of the space to keep terms that show up at least X number of times across all tweets to make a more interpretable network (below) - if don't do this row, then it will be a very busy/messy network that's hard to read - further, we are only looking at more frequently occurring terms suggesting greater importance

# Network of terms
# some times val more than 1 because it appeared multiple times in a single tweet
# convert to only boolean
tdm[tdm > 1] <- 1 

termM <- tdm %*% t(tdm) # term-term matrix - each term is on rows and columns - to show combinations of words in different tweets; this takes some time
termM[1:10,1:10]

# build the graph for month 1
g_month1 <- graph.adjacency(termM, 
                     weighted = TRUE, 
                     mode = "undirected") # don't specify the direction from node to node
g_month1 <- simplify(g_month1)

# labels: V = vertices --> indicate label to be the name
V(g_month1)$label <- V(g_month1)$name
V(g_month1)$label

V(g_month1)$degree <- degree(g_month1) # number of connections between terms - how often each term combination has appeared
V(g_month1)$degree


# build the graph for month 2
g_month2 <- graph.adjacency(termM, 
                            weighted = TRUE, 
                            mode = "undirected") 
g_month2 <- simplify(g_month2)

# labels
V(g_month2)$label <- V(g_month2)$name
V(g_month2)$label

V(g_month2)$degree <- degree(g_month2) 
V(g_month2)$degree


# build the graph for month 3
g_month3 <- graph.adjacency(termM, 
                            weighted = TRUE, 
                            mode = "undirected") 
g_month3 <- simplify(g_month3)

# labels
V(g_month3)$label <- V(g_month3)$name
V(g_month3)$label

V(g_month3)$degree <- degree(g_month3) 
V(g_month3)$degree



# build the graph for month 4
g_month4 <- graph.adjacency(termM, 
                            weighted = TRUE, 
                            mode = "undirected") 
g_month4 <- simplify(g_month4)

# labels
V(g_month4)$label <- V(g_month4)$name
V(g_month4)$label

V(g_month4)$degree <- degree(g_month4) 
V(g_month4)$degree


# build the graph for month 5
g_month5 <- graph.adjacency(termM, 
                            weighted = TRUE, 
                            mode = "undirected") 
g_month5 <- simplify(g_month5)

# labels
V(g_month5)$label <- V(g_month5)$name
V(g_month5)$label

V(g_month5)$degree <- degree(g_month5) 
V(g_month5)$degree


# build the graph for month 6
g_month6 <- graph.adjacency(termM, 
                            weighted = TRUE, 
                            mode = "undirected") 
g_month6 <- simplify(g_month6)

# labels
V(g_month6)$label <- V(g_month6)$name
V(g_month6)$label

V(g_month6)$degree <- degree(g_month6) 
V(g_month6)$degree


# build the graph for month 7
g_month7 <- graph.adjacency(termM, 
                            weighted = TRUE, 
                            mode = "undirected") 
g_month7 <- simplify(g_month7)

# labels
V(g_month7)$label <- V(g_month7)$name
V(g_month7)$label

V(g_month7)$degree <- degree(g_month7) 
V(g_month7)$degree



#
# plot with node importance
#par(mfrow = c(4,2))

## jan
V(g_month1)$label.cex <- 2.2*V(g_month1)$degree / max(V(g_month1)$degree) + 0.6
plot(g_month1,
     vertex.color = "dark gray",
     vertex.size = 2,
     vertex.label.dist = 1.5,
     main = "January")

## feb
V(g_month2)$label.cex <- 2.2*V(g_month2)$degree / max(V(g_month2)$degree) + 0.6
plot(g_month2,
     vertex.color = "dark gray",
     vertex.size = 2,
     vertex.label.dist = 1.5,
     main = "February")

## march
V(g_month3)$label.cex <- 2.2*V(g_month3)$degree / max(V(g_month3)$degree) + 0.6
plot(g_month3,
     vertex.color = "dark gray",
     vertex.size = 2,
     vertex.label.dist = 1.5,
     main = "March")

## april
V(g_month4)$label.cex <- 2.2*V(g_month4)$degree / max(V(g_month4)$degree) + 0.6
plot(g_month4,
     vertex.color = "dark gray",
     vertex.size = 2,
     vertex.label.dist = 1.5,
     main = "April")

## may
V(g_month5)$label.cex <- 2.2*V(g_month5)$degree / max(V(g_month5)$degree) + 0.6
plot(g_month5,
     vertex.color = "dark gray",
     vertex.size = 2,
     vertex.label.dist = 1.5,
     main = "May")

## june
V(g_month6)$label.cex <- 2.2*V(g_month6)$degree / max(V(g_month6)$degree) + 0.6
plot(g_month6,
     vertex.color = "dark gray",
     vertex.size = 2,
     vertex.label.dist = 1.5,
     main = "June")

## july
V(g_month7)$label.cex <- 2.2*V(g_month7)$degree / max(V(g_month7)$degree) + 0.6
plot(g_month7,
     vertex.color = "dark gray",
     vertex.size = 2,
     vertex.label.dist = 1.5,
     main = "July")


#par(mfrow = c(1,1))




#
# everything below for full set
#

# Build FULLcorpus and clean a bit
{ 
  corpus <- iconv(full$value, # only need to change the "month*"; then run all subsequent code
                  to = "utf-8-mac")
  corpus <- Corpus(VectorSource(corpus))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  #corpus <- tm_map(corpus, removeNumbers)
  cleanset <- tm_map(corpus, removeWords, stopwords('english'))
  for (j in seq(cleanset)) {
    cleanset[[j]] <- gsub("and", " ", cleanset[[j]]) 
    cleanset[[j]] <- gsub("coronavirus", " ", cleanset[[j]]) 
  }
  cleanset <- tm_map(cleanset, stripWhitespace)
}

tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)

# inspect first 10 rows and first 10 columns
tdm[1:10,1:10] # numbers are how many times the term/word appeared in each tweet
#tdm <- tdm[rowSums(tdm) > 50, ] # reduce size of the space to keep terms that show up at least X number of times across all tweets to make a more interpretable network (below) - if don't do this row, then it will be a very busy/messy network that's hard to read - further, we are only looking at more frequently occurring terms suggesting greater importance

# Network of terms
# some times val more than 1 because it appeared multiple times in a single tweet
# convert to only boolean
tdm[tdm > 1] <- 1 

termM <- tdm %*% t(tdm) # term-term matrix - each term is on rows and columns - to show combinations of words in different tweets; this takes some time
termM[1:10,1:10]

# build the graph for month 1
g <- graph.adjacency(termM, 
                            weighted = TRUE, 
                            mode = "undirected") # don't specify the direction from node to node
g <- simplify(g)

# labels: V = vertices --> indicate label to be the name
V(g)$label <- V(g)$name
V(g)$label

V(g)$degree <- degree(g) # number of connections between terms - how often each term combination has appeared
V(g)$degree

# Histogram of node degree
# most of the time the degree is on the lower side
qplot(as.numeric(V(g)$degree), 
     geom = "histogram",
      alpha = 0.8) +
  labs(x = "Degree of Vertices",
       y = "Frequency",
       title = "Node Degrees") +
  theme(legend.position = "none")


# color by month; full with node importance
coords <- layout.fruchterman.reingold(g)
V(g)$label.cex <- 2.2*V(g)$degree / max(V(g)$degree) + 0.6
plot(g,
     layout = coords,
     vertex.color = full$month,
     vertex.size = 2,
     vertex.label.dist = 1.5,
     main = "Google Search Queries Related to 'coronavirus'\nJanuary 2020 - July 2020")


#
# a closer look at grouping patterns in related search queries with community detection algorithms

# first, edge betweenness; dense regions mean a community
comm <- cluster_edge_betweenness(g)

# another approach is using propogating labels
prop <- cluster_label_prop(g)

# another approach is using greedy optimization of modularity - seems to pick up the natural grouping the best
greed <- cluster_fast_greedy(as.undirected(g))

# check out memebrships to see different results
## edge-betweenness
table(comm$membership) 
length(unique(comm$membership)) 

## prop labels
table(prop$membership)
length(unique(prop$membership)) 

## greedy optimization
table(greed$membership)
length(unique(greed$membership)) 


## Plot all four (withOUT labels):
## first - base structure (with no labels to get a sense of the raw space)
coords <- layout.fruchterman.reingold(g)
{
par(mfrow = c(2,2))
par(oma=c(2,2,2,2))  

plot(g,
     layout = coords,
     vertex.color = "dark gray",
     vertex.size = 4,
     vertex.label.dist = 1.5,
     vertex.label = NA,
     main = "Base Network")

## second - colored by edge betweetnness membership 
plot(g,
     layout = coords,
     vertex.color = comm$membership,
     vertex.size = 4,
     vertex.label.dist = 1.5,
     vertex.label = NA,
     main = "Edge Betweenness")

## third - colored by propogating labels membership 
plot(g,
     layout = coords,
     vertex.color= prop$membership,
     vertex.size = 4,
     vertex.label.dist = 1.5,
     vertex.label = NA,     
     main = "Propogating Labels")

## fourth - colored by greedy optimization of modularity membership
plot(g,
     layout = coords,
     vertex.color= greed$membership,
     vertex.size = 4,
     vertex.label.dist = 1.5,
     vertex.label = NA,
     main = "Greedy Optimization\nof Modularity")

title("", outer = TRUE)
mtext(text = "Note: Using Fruchterman Reingold layout for consistent node placement.", font = 3, side = 1, outer = TRUE)
par(mfrow = c(1,1))
}
#
