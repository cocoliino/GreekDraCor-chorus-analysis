# goal:
# analyze how choral identity (group vs individual, singular vs plural)
# relates to network position (degree, betweenness, eigenvector, closeness)
#
# approach:
# - one row per CHORUS NODE (not per play), so multi-chorus plays like Ichneutae get 3 rows
# - map chorus_identity(g) output to the chorus nodes detected in the graph
# - add this as node attributes: is_group, sex, chorus_plurality (singular/plural)
# - then test: do group choruses have different network metrics than individual choruses?

library(rdracor)
library(igraph)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# 1: extract per-chorus-node metrics from every play --------------------------------------------------------------

node_results <- list()

# loop over all plays
# suppress expected warnings for plays without chorus nodes (e.g. Menander's Pan)
for (i in seq_len(nrow(greek))) {
  
  play_id <- greek$playName[i]
  play_title <- greek$title[i]
  
  # author extraction
  author_name <- if ("firstAuthorName" %in% names(greek)) {
    greek$firstAuthorName[i]
  } else if ("author" %in% names(greek)) {
    greek$author[i]
  } else {
    "Unknown"
  }
  
  cat("Processing", i, "/", nrow(greek), ":", play_title,
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
          genre = greek$normalizedGenre[i]
        ) %>%
        select(play_id, title, author, everything())
      
      node_results[[length(node_results) + 1]] <- chorus_metrics
    }
    
    cat("Done.\n")
    
  }, error = function(e) {
    cat("Error processing", play_title, ":", conditionMessage(e), "\n")
  })
}

# combine results into one data frame
node_df <- bind_rows(node_results)
node_df

cat("Total chorus nodes:", nrow(node_df), "\n")
cat("Across", length(unique(node_df$play_id)), "plays\n\n")