library(rdracor)
library(igraph)

# overview of the graph structure of the satyr play "Ichneutae" by Sophocles

# list all play names
greek$playName

# chosen is "sophocles-ichneutae" because it is the only satyr play, has multi choral nodes and very interesting graph

exampleForGraph <- "sophocles-ichneutae"

# 1: Graph Overview --------------------------------------------------------------

# get igraph from GreekDraCor API

# co-occurence with undirected weighted graph
G <- get_net_cooccur_igraph(exampleForGraph, corpus="greek")

# Make undirected
if (is_directed(G)) G <- as.undirected(G, mode = "collapse")

# weighted edges
edge_attr_names(G)

summary(G)

# small play with 7 roles, all binary (FEMALES & MALES) gendered
# density: 1, meaning all characters co-occur with each other at least once
# degree: 6, meaning all characters co-occur with all other characters at least once
# distance: 1, meaning all characters are directly connected to each other
# clustering: 1, meaning all characters form a complete subgraph, which is expected given the density of 1
# cohesion: 6, meaning the minimum number of nodes that need to be removed to disconnect the graph is 6, which is the total number of nodes minus one, indicating a very cohesive graph
# assortativity: NaN, meaning the graph does not have a meaningful assortativity coefficient, likely due to the small size and complete nature of the graph

plot(G)

# graph associates with the satyr play "Ichneutae" by Sophocles, which is known for its unique choral structure and the presence of satyrs as characters. 
# a maximally connected subgraph, or clique, highest interconnectedness? 
# all characters co-occur with each other at least once, indicating a strong interconnectedness among the characters in this play. 
# Héja, E., & Ligeti-Nagy, N. (2022). A proof-of-concept meaning discrimination experiment to compile a word-in-context dataset for adjectives – A graph-based distributional approach. Acta Linguistica Academica, 69(4), 521-548. https://doi.org/10.1556/2062.2022.00579 
# Figure 1. A maximally connected subgraph – a clique – representing a near-synonymy class for varázslatos ‘magical’

# would be interesting to analyze shape of graphs along whole greek corpus, but idk how to do that yet, maybe in future work

# node attributes
vertex_attr_names(G)

# important node attributes for choral identity:
# - name: character name, used to identify chorus nodes
# - sex: character 
# - isGroup: whether the role is a group or an individual, chorus can be group but also chorai?? chorleiter

# centrality measures for character importance in the network:
# - degree: number of connections
# - betweenness: how often a node lies on the shortest path between other nodes
# - closeness: how close a node is to all other nodes in the network
# - eigenvector: how influential a node is in the network, based on the importance of its neighbors

# 2: Cast Overview --------------------------------------------------------------

# cast: char names, sex and is group
cast <- tibble(
  name = V(G)$name,
  sex = V(G)$sex,
  is_group = V(G)$isGroup
)
cast

# simple cast statistics, count is_group and gender 
castStatistics <- cast %>%
  group_by(sex) %>%
  summarise(
    total_characters = n(),
    total_groups = sum(is_group),
    total_individuals = total_characters - total_groups
  )
castStatistics

# cast consists of 7 characters,
# 1 individual woman 'Κυλλήνη'
# 6 Male characters, of which 4 are groups (the chorus of satyrs??) and 2 are individual (the protagonist, the satyr leader???).


# 3: Chorus Overview --------------------------------------------------------------

# chorus nodes identities 
chorus_identity(G)

# plot graph with chorus nodes highlighted and edge only between chorus nodes highlighted
# detect chorus nodes
chorus_nodes <- V(G)$name[detect_chorus(G)]

# get edge list
edge_list <- ends(G, E(G))

# color ONLY chorus ↔ chorus edges
E(G)$color <- ifelse(
  edge_list[,1] %in% chorus_nodes &
    edge_list[,2] %in% chorus_nodes,
  "red",
  "grey80"
)

# add color to chorus nodes
V(G)$color <- ifelse(
  V(G)$name %in% chorus_nodes,
  "lightblue",
  "lightgrey"
)

# --- Plot ---
# igraph uses a stochastic layout algorithm by default (Fruchterman-Reingold),
# so the plot changes each time. Setting a seed makes it reproducible.
set.seed(5)

# plot with chorus nodes highlighted and edge only between chorus nodes highlighted
plot.igraph(
  G,
  vertex.label = V(G)$name,
  vertex.size = 30,
  vertex.color = V(G)$color,
  edge.width = E(G)$weight,
  edge.color = E(G)$color
)

# ?? could the layout be analyized across the whole corpus to see if there are patterns in the way chorus nodes are positioned in the graph?
# match_vertices {igraph}	R Documentation
# Match Graphs given a seeding of vertex correspondences
# Description
# Given two adjacency matrices A and B of the same size, match the two graphs with the help of m seed vertex pairs which correspond to the first m rows (and columns) of the adjacency matrices.
# 
# Usage
# match_vertices(A, B, m, start, iteration)