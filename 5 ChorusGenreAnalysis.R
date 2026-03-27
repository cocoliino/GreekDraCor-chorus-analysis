library(rdracor)
library(igraph)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# goal:
# analyze how GENRE (Tragedy / Comedy / Satyr) shapes the chorus's network position
# - do choruses in comedies have higher degree centrality than those in tragedies?
# - do choruses in satyr plays have higher betweenness centrality than those in tragedies?
# - do choruses in comedies have higher eigenvector centrality than those in satyr plays?

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

# we can also analyze the relationship between genre and chorus network position at the play level
# averaging the chorus node metrics for each play and then comparing these averages across genres

# --- Aggregate to play level -----------------------------------------------

greek_df <- as_tibble(greek) %>%
  rename(play_id = playName)

choralIdentity_df <- choralIdentity_df %>%
  left_join(
    greek_df %>% select(play_id, year = yearNormalized),
    by = "play_id"
  )

chorus_play_df <- choralIdentity_df %>%
  group_by(play_id) %>%
  summarise(
    degree = mean(degree, na.rm = TRUE),
    betweenness = mean(betweenness, na.rm = TRUE),
    eigenvector = mean(eigenvector, na.rm = TRUE),
    closeness = mean(closeness, na.rm = TRUE),
    genre = first(genre),
    author = first(author),
    year = first(year),
    n_chorus = n(),
    .groups = "drop"
  )
chorus_play_df