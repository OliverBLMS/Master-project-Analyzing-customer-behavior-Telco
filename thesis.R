

# get / set workdirectory
################################################################################################################

getwd()
setwd(dir = "/Users/oliver.belmans/Data/R_workdirectory/Thesis")

# load packages
################################################################################################################

pkg <- c("tidyverse","dtplyr",  "statnet", "igraph", "data.table")
lapply(pkg, require, character.only = TRUE)

# read sample dataset (for now make sample set)
################################################################################################################
# sample_cdr <- fread(input = "sample_cdr.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Make Dummy set
net <- sample_smallworld(dim = 1, size = 2000, nei =  30, p = 0.8, loops = FALSE, multiple = TRUE)
size_net <- ecount(net)

date_time_gerenator <- function(N, st="2010/01/01", et="2010/01/31") {
  st <- as.POSIXct(as.Date(st))
  et <- as.POSIXct(as.Date(et))
  dt <- as.numeric(difftime(et,st,unit="sec"))
  ev <- sort(runif(N, 0, dt))
  rt <- st + ev
}

# transform graph dataset to dataframe and add extra variables (call date time, call duration)
sample_cdr <- data.table(as_data_frame(x = net)) %>%
  mutate(
    # use size_net for correct number of records to generate
    CALL_START_DT_TM = date_time_gerenator(size_net),
    # generate random call duration 
    CALL_ACTUAL_DURATION = round(abs(rnorm(size_net, 100, 200)),0)
  )
# print head
head(sample_cdr)
# Basic Checks
################################################################################################################

# Check if caller appear as callee
# table(sample_cdr$A_NUMBER %in% sample_cdr$B_NUMBER)

# Variable manipulations
################################################################################################################

## CALL_START_DT to date format
## Create extra date/time variables like week-, daynumber, houres
sample_cdr <-
  sample_cdr %>%
  mutate(
  # CALL_START_DT_TM = strptime(paste(sample_cdr$CALL_START_DT, sample_cdr$CALL_START_TM),"%d%b%Y %H:%M:%S")
  WEEK_NR = format(CALL_START_DT_TM, "%U"),
  DAY_NR = format(CALL_START_DT_TM, "%w"),
  DAY = format(CALL_START_DT_TM, "%a"),
  HOUR = as.integer(format(CALL_START_DT_TM, "%H")),
  PEAK_HOUR = ifelse(HOUR >= 8 & HOUR <= 18, 1, 0),
  WEEKDAYS = ifelse(DAY_NR %in% c(0,6), 0, 1)
  )

# todo: Filter outliers or non relevant items
# based on duration

# todo: filter irrelevant user for network features, outside network? 
# which hold no node information (no outgoing)


# Question: how to label new customers?


# Variable creation based on usage
# create usage KPI 
################################################################################################################

# Create stats which will focus on week

# per Hour
week_set <- 
  sample_cdr %>%
  group_by(WEEK_NR, from) %>%
  summarise(
    nr_call= n(),
    ave_call_time = mean(CALL_ACTUAL_DURATION),
    total_call_time = sum(CALL_ACTUAL_DURATION)
  ) %>%
  gather(variable, value, -c(from, WEEK_NR)) %>%
  unite(temp, variable, WEEK_NR) %>%
  spread(temp, value, fill = 0)

day_set <- 
  sample_cdr %>%
  group_by(WEEK_NR, from, DAY) %>%
  summarise(
    nr_call= n(),
    ave_call_time = mean(CALL_ACTUAL_DURATION),
    total_call_time = sum(CALL_ACTUAL_DURATION)
  ) %>%
  gather(variable, value, -c(from, WEEK_NR, DAY)) %>%
  unite(temp, variable, DAY) %>%
  spread(temp, value, fill = 0)

peak_set <-
  sample_cdr %>%
  group_by(WEEK_NR, from, PEAK_HOUR) %>%
  summarise(
    nr_call= n(),
    ave_call_time = mean(CALL_ACTUAL_DURATION),
    total_call_time = sum(CALL_ACTUAL_DURATION)
  ) %>%
  gather(variable, value, -c(from, WEEK_NR, PEAK_HOUR)) %>%
  unite(temp, variable, PEAK_HOUR) %>%
  spread(temp, value, fill = 0)

weekday_set <- 
  sample_cdr %>%
  group_by(WEEK_NR, from, WEEKDAYS) %>%
  summarise(
    nr_call= n(),
    ave_call_time = mean(CALL_ACTUAL_DURATION),
    total_call_time = sum(CALL_ACTUAL_DURATION)
  ) %>%
  gather(variable, value, -c(from, WEEK_NR, WEEKDAYS)) %>%
  unite(temp, variable, WEEKDAYS) %>%
  spread(temp, value, fill = 0)

base_dataset <-
  unique(select(sample_cdr, from, WEEK_NR)) %>% arrange(from, WEEK_NR)

full_dataset <-
  


# prev_call: is the elapsed time (in minutes) from the previous call
# weekend_voice_out_calls – number of outgoing voice calls during weekends;
# workdays_voice_out_calls – number of outgoing voice calls on working days;
# worktime_voice_out_calls – duration of outgoing voice calls on working hours (8:00 a.m. – 6:00 p.m.);
# average call duration
# % of weekday calls (Monday – Friday)
# % of daytime calls (9am – 5pm)
# average # calls received per day
# average # calls originated per day

# Variable creation 
# SOCIAL NETWORK ANALYSIS: package statnet
################################################################################################################
# load our data into a network object
# assume that the raw data for our analysis is in a transactional format like CDR

## make empty graphobject, with preserved memory from CDR data
# net <- graph.empty(n = nrow(sample_cdr))

# net <- graph_from_data_frame(sample_cdr, directed = FALSE)
# plot.igraph(net, vertex.size = 2)

# plot sample network
plot.igraph(net,vertex.label=V(net)$name, vertex.size=5)

# Using edgelists with associated edge values to create a weighted network. 
# which kpi as weight?
# E(net)$weight = as.numeric(<< add weight column >>)


# Network characteristics
################################################################################################################

# type of network
# directed or undirected
# total number of vertices n
# total number of edges m
# mean degree c
# fraction of vertices in the largest component S (or the largest weakly connected component in the case of a directed network);
# mean geodesic distance between connected vertex pairs `
# exponent α of the degree distribution if the distribution follows a power
# law (or - if not; in/out-degree exponents are given for directed graphs);
# local clustering coefficient C:
#   Average local clustering coefficient over all nodes
# the degree correlation coefficient r

# NODE CHARACTERISTICS
################################################################################################################

# !! normalized output: This step is very important when dealing with parameters of different units and scales.!!
# typical neural network algorithm require data that on a 0-1 scale.

# Degree's: The nodes with higher degree is more central.
# in, out, all : choose whether directed or undirected graph
# normalized: degree / number of nodes
net_degree <- degree(graph = net, v = V(net), mode = "all", loops = FALSE, normalized = TRUE)
degree_in = degree(graph = net, v =V(net), normalized = TRUE, mode="in", loops = FALSE)
degree_out= degree(graph = net, v = V(net), normalized = TRUE, mode="out", loops = FALSE)

# plot network with degree as size of the nodes
V(net)$size=degree(net, normalized = FALSE)
plot.igraph(net,vertex.label=NA)

# closeness
# farness/peripherality of a node v is defined as the sum of its distances to all other nodes
net_closeness <- closeness(graph = net, vids = V(net), mode = "all", normalized = TRUE)

# The distance dij (shortest path, geodesic path) between two nodes i and j is the number of edges along the shortest path connecting them.
net_shortestpath <- shortest.paths(graph = net, v = V(net))

# P(two randomly selected friends of A are friends)
net_transitiity <- transitivity(graph = net, isolates = 'zero', type="localundirected") # per node
# transitivity(graph = net, type="average") # global

# Count how many triangles a vertex is part of, in a graph, or just list the triangles of a graph.
net_triangles <- count_triangles(graph = net, vids = V(net))

# Betweenness: defined by the number of geodesics (shortest paths) going through a vertex or an edge.
# Betweenness centrality quantifies the number of times a node acts as a bridge along the shortest path between two other nodes
net_betweenness <-  betweenness(graph = net, v = V(net), directed = FALSE, normalized = TRUE)

# Eigenvector centrality
# importance comes from degree level of other connected nodes
net_ev <- evcent(graph = net, directed = FALSE)

# commands for reach or distance-weighted reach
# Reach: 2-reach and 3-reach is simply the proportion of nodes you can reach within 2 steps or 3 steps, respectively.

# 2-reach:
reach2=function(x){
  r=vector(length=vcount(x))
  for (i in 1:vcount(x)){
    n=neighborhood(x,2,nodes=i)
    ni=unlist(n)
    l=length(ni)
    r[i]=(l)/vcount(x)}
  r}

reach2(net)

# 3-reach:
reach3=function(x){
  r=vector(length=vcount(x))
  for (i in 1:vcount(x)){
    n=neighborhood(x,3,nodes=i)
    ni=unlist(n)
    l=length(ni)
    r[i]=(l)/vcount(x)}
  r}

reach3(net)

# ?
# distance-weighted reach:
dwreach=function(x){
  distances=shortest.paths(x) #create matrix of geodesic distances
  diag(distances)=1 # replace the diagonal with 1s
  weights=1/distances # take the reciprocal of distances
  apply(weights,1,sum) # sum for each node (row)
}

dwreach(net)


# Google’s PageRank is a variant of the Eigenvector centrality measure for directed network
net_pagerank <- page_rank(graph = net, vids = V(net), directed = FALSE, damping = 0.85)


# Check correlation
################################################################################################################

df <- data.frame(net_degree, net_betweenness, net_closeness, net_pagerank[[1]], net_ev[[1]])
require(corrplot)
cor_df <- cor(df, method = "spearman") # Spearman correlation matrix
pairs(~ net_degree + net_betweenness + net_closeness + net_pagerank[[1]] + net_ev[[1]], main="Simple Scatterplot Matrix")
corrplot(cor_df)

# Generally, the 3 centrality types will be positively correlated
# When they are not (low) correlated, it probably tells you something interesting about the network
# http://www2.unb.ca/~ddu/6634/Lecture_notes/Lecture_4_centrality_measure.pdf slide 51


