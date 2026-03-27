# goal:
# analyze how choral identity (group vs individual, singular vs plural)
# relates to network position (degree, betweenness, eigenvector, closeness)
#
# approach:
# - one row per CHORUS NODE (not per play), so multi-chorus plays like Ichneutae get 3 rows
# - map chorus_identity(g) output to the chorus nodes detected in the graph
# - add this as node attributes: is_group, sex, chorus_plurality (singular/plural)

library(rdracor)
library(igraph)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# filter out plays without chorus nodes for chorus-specific analysis

chorus_plays <- greek %>%
  rowwise() %>%
  mutate(has_chorus = tryCatch({
    g <- get_net_cooccur_igraph(play = playName, corpus = "greek")
    if (is_directed(g)) g <- as.undirected(g, mode = "collapse")
    length(detect_chorus(g)) > 0
  }, error = function(e) {
    FALSE
  })) %>%
  ungroup() %>%
  filter(has_chorus) %>%
  select(-has_chorus)

# 1: extract per-chorus-node metrics from every play --------------------------------------------------------------

node_results <- list()

# loop over all plays
# suppress expected warnings for plays without chorus nodes (e.g. Menander's Pan)
for (i in seq_len(nrow(chorus_plays))) {
  
  play_id <- chorus_plays$playName[i]
  play_title <- chorus_plays$title[i]
  
  # author extraction
  author_name <- if ("firstAuthorName" %in% names(chorus_plays)) {
    chorus_plays$firstAuthorName[i]
  } else if ("author" %in% names(chorus_plays)) {
    chorus_plays$author[i]
  } else {
    "Unknown"
  }
  
  cat("Processing", i, "/", nrow(chorus_plays), ":", play_title,
      "(", play_id, ") by", author_name, "... ")
  
  tryCatch({
    
    # Load graph
    g <- get_net_cooccur_igraph(play = play_id, corpus = "greek")

    # Ensure undirected (centrality measures require it)
    if (is_directed(g)) g <- as.undirected(g, mode = "collapse")

    # Get chorus node metrics
    chorus_metrics <- get_chorus_metrics(g)
    
    if (nrow(chorus_metrics) > 0) {
      chorus_metrics <- chorus_metrics %>%
        mutate(
          play_id = play_id,
          title = play_title,
          author = author_name,
          genre = chorus_plays$normalizedGenre[i]
        ) %>%
        select(play_id, title, author, everything())
      
      node_results[[length(node_results) + 1]] <- chorus_metrics
    }
    
    cat("Done.\n")
    
  }, error = function(e) {
    cat("Error processing", play_title, ":", conditionMessage(e), "\n")
  })
}

# check how many chorus nodes we have in total across all plays
sum(sapply(node_results, nrow))

# combine results into one data frame
choralIdentity_df <- bind_rows(node_results)
choralIdentity_df

# summarise how many plays with multi chorus nodes
# how many chorus nodes in total across all plays
chorus_node_counts <- choralIdentity_df %>%
  group_by(play_id) %>%
  summarise(num_chorus_nodes = n())

# print summary
cat("Total chorus nodes:", nrow(choralIdentity_df), "across", length(unique(choralIdentity_df$play_id)), "plays\n\n")
cat("Plays with multiple chorus nodes:\n")
print(chorus_node_counts %>% filter(num_chorus_nodes > 1))