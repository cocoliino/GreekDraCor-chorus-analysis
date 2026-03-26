# goal:
# analyze how choral identity (group vs individual, singular vs plural)
# relates to network position (degree, betweenness, eigenvector, closeness)
#
# approach:
# - one row per CHORUS NODE (not per play), so multi-chorus plays like Ichneutae get 3 rows
# - map chorus_identiy(g) output to the chorus nodes detected in the graph
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
row_counter <- 0

# loop over all plays
# loop get_chorus_metrics()
# debug: surpress warning messages for plays without chorus nodes, since they are expected and handled by tryCatch
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

# Warning messages:
# 1: In detect_chorus(g) : No chorus node detected in play: Πάν (Pan)
# 2: In detect_chorus(g) : No chorus node detected in play: Πάν (Pan)

# Note: these warnings are expected for plays without chorus nodes, and are handled gracefully by the tryCatch block, so they do not indicate a problem with the script.

# combine results into one data frame
node_df <- bind_rows(node_results)
node_df

cat("Total chorus nodes:", nrow(node_df), "\n")
cat("Across", length(unique(node_df$play_id)), "plays\n\n")


