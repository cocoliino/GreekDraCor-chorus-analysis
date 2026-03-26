library(igraph)
library(dplyr)
library(tidyverse)

# common publication theme
theme_pub <- theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#  define a custom color palette for the authors and use it consistently in all plots
scale_author_fill <- scale_fill_manual(values = c(
  "Aeschylus" = "#affc41",
  "Sophocles" = "#1dd3b0",
  "Euripides" = "#7570b3",
  "Aristophanes" = "#3a6ea5",
  "Menander" = "#3c1642"
))

scale_author_color <- scale_color_manual(values = c(
  "Aeschylus" = "#affc41",
  "Sophocles" = "#1dd3b0",
  "Euripides" = "#7570b3",
  "Aristophanes" = "#3a6ea5",
  "Menander" = "#3c1642"
))


# 1: help functions -----------------------------------------------------------
# return indices of all nodes containing "χορ"
# attention, can be more than one chorus node per play, e.g. different chorai, or chorus + chorleiter
detect_chorus <- function(g){

  chorus_idx <- grep("χορ", V(g)$name, ignore.case = TRUE)
  if (length(chorus_idx) == 0) {
    warning("No chorus node detected in play: ",
            graph_attr(g, "name") %||% "(unnamed graph)")
  }
  return(chorus_idx)
}

# what exactly is matched in detect_chorus?
# it matches any node name that contains the substring "χορ" (case-insensitive)

# detect chorus nodes (as names)
chorus_nodes_name <- function(g){
  
  chorus_idx <- detect_chorus(g)
  
  chorus_names <- V(g)$name[chorus_idx]
  
  return(chorus_names)
  
}

# return chorus node info:
# nameOfChorus, chorus_sex, chorus_isGroup
chorus_identity <- function(g){
  
  chorus_idx <- detect_chorus(g)
  
  tibble(
    
    Name = V(g)$name[chorus_idx],
    
    sex = V(g)$sex[chorus_idx],
    
    is_group = V(g)$isGroup[chorus_idx]
    
  )
}


# use chorus_identity for get_chorus_metrics() that returns a data frame with one row per chorus node, and all the relevant metrics and attributes for that node, including its identity (group vs individual), its centrality measures, its proximity to the protagonist, etc.
# get_chorus_metrics() should at least return the chorus node indices, their names, their is_group and sex attributes, and their centrality measures (degree, betweenness, closeness, eigenvector). Then in the loop we can compute the proximity to protagonist and community membership, which require additional graph-level calculations (e.g. identifying the protagonist node, running community detection) that we only want to do once per play rather than once per chorus node.
get_chorus_metrics <- function(g){

  chorus_idx <- detect_chorus(g)

  # Build from chorus_identity and add centrality in one pipeline.
  # Using mutate instead of bind_cols avoids fragile row-order assumptions.
  chorus_identity(g) %>%
    rename(name = Name) %>%
    mutate(
      degree      = degree(g)[chorus_idx],
      betweenness = betweenness(g)[chorus_idx],
      closeness   = closeness(g)[chorus_idx],
      eigenvector = eigen_centrality(g)$vector[chorus_idx]
    )
}