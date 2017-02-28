########################################################
# get / set workdirectory
########################################################
getwd()
setwd(dir = "/Users/oliver.belmans/Data/R_workdirectory/Thesis")


########################################################
# load packages
########################################################
pkg <- c("tidyverse", "statnet", "igraph")
lapply(pkg, require, character.only = TRUE)

########################################################
# read sample dataset
########################################################
sample_cdr <- read.table("sample_cdr.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)

########################################################
# Basic Checks
########################################################
# Check if caller appear as callee
table(sample_cdr$A_NUMBER %in% sample_cdr$B_NUMBER)

########################################################
# Variable manipulations
########################################################
## CALL_START_DT to date format
## Create extra date/time variables like week-, daynumber, houres
sample_cdr$CALL_START_DT_TM <- strptime(paste(sample_cdr$CALL_START_DT, sample_cdr$CALL_START_TM),"%d%b%Y %H:%M:%S")
sample_cdr$WEEK_NR <- format(sample_cdr$CALL_START_DT_TM, "%U")
sample_cdr$DAY_NR <- format(sample_cdr$CALL_START_DT_TM, "%w")
sample_cdr$DAY <- format(sample_cdr$CALL_START_DT_TM, "%a")
sample_cdr$HOUR <- as.integer(format(sample_cdr$CALL_START_DT_TM, "%H"))
sample_cdr$PEAK_HOUR <- ifelse(sample_cdr$HOUR >= 8 & sample_cdr$HOUR <= 18, "PEAK", "NON-PEAK")

# Filter outliers or non relevant items
## based on duration


########################################################
# Variable creation 
# Based on usage
########################################################


# create usage KPI like 
# % ...
# % ...



########################################################
# Variable creation 
# SOCIAL NETWORK ANALYSIS: package statnet
########################################################

# load our data into a network object
# assume that the raw data for our analysis is in a transactional format that is typical at least in the Telecommunications

# statnet pkg
net <- network(sample_cdr[, c(1,2)], matrix.type = "edgelist", directed = TRUE, multiple = TRUE)
summary(net)

# igraph pkg
## make sample network
net <- sample_smallworld(dim = 1, size = 400, nei =  1.5, p = 0.9, loops = FALSE)
plot.igraph(net,vertex.label=V(net)$name, vertex.size=5)

## make graph from CDR data
# net <- graph.empty(n = nrow(sample_cdr))

## Edge lists
net <- graph.data.frame(d = sample_cdr, directed = FALSE)
plot(net)
summary(net)
# net2 <- graph.edgelist(as.matrix(sample_cdr[, c(1,2)]), directed = TRUE)
# plot(net2)

V(net) # vertices id
E(net) # edges


########################################################
# NODE CHARACTERISTICS
########################################################

# Degree's
# in, out, all : choose whether directed or undirected graph
# Indegree of any node i: the number of nodes destined to i.
# Outdegree of any node i: the number of nodes originated at i.
degree(graph = net, v = V(net), mode = "all", loops = FALSE, normalized = TRUE)
degree_in = degree(graph = net, v =V(net), normalized = TRUE, mode="in", loops = FALSE)
degree_out= degree(graph = net, v = V(net), normalized = TRUE, mode="out", loops = FALSE)

V(net)$size=degree(net, normalized = FALSE)
plot.igraph(net,vertex.label=NA)


# The distance dij (shortest path, geodesic path) between two nodes i and j is the number of edges along the shortest path connecting them.
shortest.paths(graph = net, v = V(net))

# P(two randomly selected friends of A are friends)
transitivity(graph = net, isolates = 'zero', type="localundirected") # per node
transitivity(graph = net, type="average") # global

# Count how many triangles a vertex is part of, in a graph, or just list the triangles of a graph.
count_triangles(graph = net, vids = V(net))

# Betweenness: defined by the number of geodesics (shortest paths) going through a vertex or an edge.
betweenness(graph = net, v = V(net), directed = FALSE, normalized = TRUE)

# Eigenvector centrality
evcent(graph = net, directed = FALSE)

# commands for reach or distance-weighted reach
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

# distance-weighted reach:
dwreach=function(x){
  distances=shortest.paths(x) #create matrix of geodesic distances
  diag(distances)=1 # replace the diagonal with 1s
  weights=1/distances # take the reciprocal of distances
  apply(weights,1,sum) # sum for each node (row)
}

dwreach(net)




