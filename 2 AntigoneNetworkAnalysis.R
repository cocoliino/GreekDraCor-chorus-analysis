library(rdracor)

# goal: demonstrate how to compute and interpret key network metrics for a single play, focusing on chorus nodes

# chosen as example is "sophocles-antigone" because I started close reading the reclam edition
# it also does not have a fully connected graph like the satyr play "ichneutae" and 
# thus allows for more interesting metric network analysis
# it also has only one chorus node, making certain metrics easier to interpret for the chorus

exampleForSNA <- "sophocles-antigone"

# co-occurence with undirected weighted graph
GSNA <- get_net_cooccur_igraph(exampleForSNA, corpus="greek")

# Make undirected
if (is_directed(GSNA)) GSNA <- as.undirected(GSNA, mode = "collapse")

detect_chorus(GSNA)
chorus_name <- V(GSNA)$name[detect_chorus(GSNA)]
chorus_name

chorus_identity(GSNA)
# unknown sex and group status, but singular chorus node, so we can classify as "individual chorus"

# 1: Global Network Metrics -----------------------------------------------

#Compute all metrics for Antigone

# Density
# "How many of the possible connections are actually realized?"
# Formula: actual edges / possible edges
# Range: 0 (no edges) to 1 (every node connected to every other node)

density_val <- edge_density(GSNA)
density_val

cat("Interpretation: In Antigone,", round(density_val * 100, 1),
    "% of all possible character pairs actually co-occur in a scene.")

# Shortest path
# "What is the shortest route between two specific nodes?"

# choose specific character to demonstrate. e.g., the first non-chorus character
other_chars <- V(GSNA)$name[V(GSNA)$name != chorus_name]
example_target <- other_chars[1]

sp <- shortest_paths(GSNA, from = chorus_name, to = example_target)
cat("Shortest path from", chorus_name, "to", example_target, ":\n")
cat("  Path:", paste(V(GSNA)$name[sp$vpath[[1]]], collapse = " -> "), "\n")
cat("  Length:", length(sp$vpath[[1]]) - 1, "steps\n")

# Diameter
# "What is the longest shortest path in the network?"
# maximum 'distance' between any two characters

diam <- diameter(GSNA)
cat("Diameter:", diam, "\n")
cat("Interpretation: The two most distant characters in Antigone are",
    diam, "steps apart.\n")

# is the chorus in the longest shortes path in the network? (i.e., does it help connect distant characters?)
# we can check if the chorus is on any of the shortest paths between all pairs of characters
chorus_on_shortest <- 0
total_pairs <- 0

for (i in 1:(vcount(GSNA) - 1)) {
  for (j in (i + 1):vcount(GSNA)) {
    sp <- shortest_paths(GSNA, from = i, to = j)$vpath[[1]]
    total_pairs <- total_pairs + 1
    if (chorus_name %in% V(GSNA)$name[sp]) {
      chorus_on_shortest <- chorus_on_shortest + 1
    }
  }
}
cat("Chorus is on", chorus_on_shortest, "out of", total_pairs,
    "shortest paths between character pairs.\n")

# check percentage of fullfillment of chorus being on shortest paths
chorus_shortest_pct <- chorus_on_shortest / total_pairs * 100
cat("Fullfilling", round(chorus_shortest_pct, 1),
    "% of shortest paths between character pairs.\n")

# Components
# "Is the network connected, or does it break into separate pieces?"
# Greek plays are usually a single connected component

comp <- components(GSNA)
comp

cat("Number of components:", comp$no, "\n")
cat("Size of largest component:", max(comp$csize), "\n")
if (comp$no == 1) {
  cat("The network is fully connected,every character can reach every other.\n")
  cat("(This is typical for Greek drama, especially when the chorus is present.)\n")
} else {
  cat("The network has disconnected parts.\n")
}

# Transitivity (Clustering coefficient)
# "If A knows B and B knows C, do A and C also know each other?"
# High transitivity = many closed triangles = tight-knit network

trans <- transitivity(GSNA, type = "global")
trans

cat("Global transitivity:", round(trans, 4), "\n")
cat("Interpretation:", round(trans * 100, 1),
    "% of connected character triples form closed triangles.\n")

# ? is chorus involved in many triangles? (i.e., does it co-occur with pairs of characters who also co-occur with each other?)
chorus_triangles <- sum(count_triangles(GSNA, vids = detect_chorus(GSNA)))
cat("Chorus is involved in", chorus_triangles, "triangles.\n")
cat("Interpretation: The chorus co-occurs with pairs of characters who also co-occur with each other", 
    chorus_triangles, "times.\n")

# 2: Local Metrics of Centrality -------------------------------------------

#Compute Degree, Eigenvector and Betweenness and specifically compare the chorus to other characters

#  Degree centrality
# "How many other characters does each character co-occur with?"

deg <- degree(GSNA)
sort( deg, decreasing = TRUE)

# how does the chorus rank in degree centrality compared to other characters?
cat("\nChorus degree:", deg[chorus_name], "out of", vcount(GSNA) - 1, "possible\n")
cat("Chorus rank:", which(names(sort(deg, decreasing = TRUE)) == chorus_name),
    "of", vcount(GSNA), "\n")

# Eigenvector centrality
# "Is this character connected to other well-connected characters?"
# High eigenvector centrality = connected to central/important nodes

eig <- eigen_centrality(GSNA)$vector
sort(eig, decreasing = TRUE)
cat("\nChorus eigenvector centrality:", round(eig[chorus_name], 4), "\n")
cat("Chorus rank:", which(names(sort(eig, decreasing = TRUE)) == chorus_name),
    "of", vcount(GSNA), "\n")

# Betweenness centrality
# "Does this character lie on the shortest paths between other characters?"
# High betweenness = the character is a bridge/mediator
# interesting for chorus since scholars like Bacon (1994)
# argue the chorus MEDIATES between characters

betw <- betweenness(GSNA)
sort(betw, decreasing = TRUE)

cat("\nChorus betweenness:", round(betw[chorus_name], 4), "\n")
cat("Chorus rank:", which(names(sort(betw, decreasing = TRUE)) == chorus_name),
    "of", vcount(GSNA), "\n")

# 3: Community Detection ---------------------------------------------------

# "Can we identify subgroups (communities) within the network?"
# The exercise used greedy modularity optimization: cluster_fast_greedy(G)
# Communities = groups of characters who interact more with each other
# than with the rest of the network

c1 <- cluster_fast_greedy(GSNA)

# Store community membership as a vertex attribute
GSNA <- set_vertex_attr(GSNA, "modularity", index = V(GSNA), membership(c1))

# Modularity score:how well-separated are the communities?
# Range: -0.5 to 1. Higher = more distinct communities
cat("Modularity:", round(modularity(c1), 4), "\n")

# Number of communities
cat("Number of communities:", length(c1), "\n")

# Size of each community
print(sizes(c1))

# Which community is the chorus in?
chorus_community <- membership(c1)[chorus_name]
cat("\nChorus is in community:", chorus_community, "\n")

# Who else is in the chorus's community?
same_community <- names(membership(c1)[membership(c1) == chorus_community])
cat("Other members of the chorus's community:",
    paste(same_community[same_community != chorus_name], collapse = ", "), "\n")