# goal:
# analyze how GENRE (Tragedy / Comedy / Satyr) shapes the chorus's network position
# - do choruses in comedies have higher degree centrality than those in tragedies?
# - do choruses in satyr plays have higher betweenness centrality than those in tragedies?
# - do choruses in comedies have higher eigenvector centrality than those in satyr plays?
library(rdracor)
library(igraph)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# using the choralIdentity_df we created in the previous step, 
# one row per chorus node with all the relevant attributes 
# anayze relationship between genre and chorus network position
# comparing the mean degree, betweenness, and eigenvector centrality of chorus nodes across genres (tragedy, comedy, satyr)
chorus_genre_analysis <- choralIdentity_df %>%
  group_by(genre) %>%
  summarise(
    mean_degree = mean(degree, na.rm = TRUE),
    mean_betweenness = mean(betweenness, na.rm = TRUE),
    mean_eigenvector = mean(eigenvector, na.rm = TRUE),
    n_chorus_nodes = n(),
    n_plays = n_distinct(play_id)
  )
chorus_genre_analysis