library(rdracor)
library(igraph)
library(dplyr)

# goal: compute key network metrics for all Greek plays in Dracor, focusing on chorus nodes
# maybe better to directly use the node_df with chorus node attributes and then merge with the play-level metrics, but for now I will compute everything from the graph for each play and then merge with the node_df later if needed 


cat("Total plays in GreekDraCor:", nrow(greek), "\n\n")

results_list <- vector("list", nrow(greek))

# loop over plays
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
    
    # Detect chorus nodes
    ch_nodes <- detect_chorus(g)
    ch_found <- length(ch_nodes) > 0
    
    if (ch_found) {
      ch_names <- V(g)$name[ch_nodes]
    } else {
      ch_names <- character(0)
    }
    
    # Global metrics
    dens <- edge_density(g)
    diam_val <- if (is_connected(g)) diameter(g) else NA
    comp_val <- components(g)
    trans_val <- transitivity(g, type = "global")
    
    # Centrality measures
    deg_all <- degree(g)
    eig_all <- eigen_centrality(g)$vector
    betw_all <- betweenness(g)
    
    # Chorus metrics
    # use get_chorus_metrics() to get chorus node attributes and centrality measures, then average across chorus nodes if multiple
    if (ch_found) {
      chorus_metrics <- get_chorus_metrics(g)
      ch_deg <- mean(chorus_metrics$degree, na.rm = TRUE)
      ch_eig <- mean(chorus_metrics$eigenvector, na.rm = TRUE)
      ch_betw <- mean(chorus_metrics$betweenness, na.rm = TRUE)
      
      # Normalized degree
      ch_deg_norm <- ch_deg / (vcount(g) - 1)
      
      # Ranks
      ch_deg_rank <- rank(-deg_all)[ch_nodes] %>% mean(na.rm = TRUE)
      ch_eig_rank <- rank(-eig_all)[ch_nodes] %>% mean(na.rm = TRUE)
      ch_betw_rank <- rank(-betw_all)[ch_nodes] %>% mean(na.rm = TRUE)
      
      # Chorus type classification
      chorus_type <- if (all(chorus_metrics$is_group)) {
        "group"
      } else if (all(!chorus_metrics$is_group)) {
        "individual"
      } else {
        "mixed"
      }
    } 
    else {
      ch_deg <- NA
      ch_eig <- NA
      ch_betw <- NA
      ch_deg_norm <- NA
      ch_deg_rank <- NA
      ch_eig_rank <- NA
      ch_betw_rank <- NA
      chorus_type <- NA
    }
    
    # Community detection
    if (ecount(g) > 0) {
      c_det <- cluster_fast_greedy(g)
      num_comm <- length(c_det)
      mod_val <- modularity(c_det)
      ch_comm <- if (ch_found) {
        mean(membership(c_det)[ch_names], na.rm = TRUE)
      } else NA
    } else {
      num_comm <- NA
      mod_val <- NA
      ch_comm <- NA
    }
    
    # Optional: annotate graph
    V(g)$chorus_flag <- ifelse(
      V(g)$name %in% ch_names,
      "chorus",
      "non-chorus"
    )

    # Store results
    results_list[[i]] <- data.frame(
      play_name = play_title,
      author = author_name,
      num_characters = vcount(g),
      num_edges = ecount(g),
      density = round(dens, 4),
      diameter = diam_val,
      num_components = comp_val$no,
      transitivity = round(trans_val, 4),
      
      # Chorus info
      chorus_found = ch_found,
      chorus_type = chorus_type,
      num_chorus_nodes = length(ch_nodes),
      
      # Centrality
      chorus_degree = round(ch_deg, 4),
      chorus_degree_norm = round(ch_deg_norm, 4),
      chorus_degree_rank = round(ch_deg_rank, 2),
      
      chorus_eigenvector = round(ch_eig, 4),
      chorus_eigenvector_rank = round(ch_eig_rank, 2),
      
      chorus_betweenness = round(ch_betw, 4),
      chorus_betweenness_rank = round(ch_betw_rank, 2),
      
      # Community
      chorus_community = ch_comm,
      num_communities = num_comm,
      modularity = round(mod_val, 4),
      
      stringsAsFactors = FALSE
    )
    
    cat("OK\n")
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    results_list[[i]] <- NULL
  })
}

# Combine results
results <- do.call(rbind, results_list)
results <- as_tibble(results)
results

# filter out plays without chorus nodes for chorus-specific analysis
chorus_plays <- results %>% filter(chorus_found == TRUE)

cat("\nPlays with a chorus node:", nrow(chorus_plays),
    "out of", nrow(results), "\n\n")

# 2: analyze relationship between choral identity and network position ----------------------------------------------
# - do group choruses have different network metrics than individual choruses?
# - do singular choruses have different network metrics than plural choruses?
# - sex differences in chorus network position?

# @ use t-tests or ANOVAs to compare the network metrics (degree, betweenness, eigenvector) between groups (group vs individual)
# @ visualize these differences using boxplots or violin plots
# @ we can also explore correlations between the network metrics and the chorus attributes (e.g. is there a correlation between degree and is_group?)

