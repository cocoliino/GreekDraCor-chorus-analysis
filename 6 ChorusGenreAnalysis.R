# 6 Chorus Genre Analysis
#
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

# I do have node_df of all chorus nodes and it does have added Genre
node_df

# what is happening with plays without a genre?
# I will have NA for genre, so they will be filtered out of the genre analysis, but they will still be included in the overall chorus metrics analysis (script 4) since that is per play not per chorus node

# clean up the node_df to have consistent categories for group

node_df <- node_df %>%
  mutate(
    # clean group status
    group_status = case_when(
      is.na(is_group) ~ "Unknown",
      is_group == TRUE ~ "Group",
      is_group == FALSE ~ "Individual"
    ),
    # clean gender
    gender_cat = case_when(
      sex == "FEMALE" ~ "Female",
      sex == "MALE" ~ "Male",
      TRUE ~ "Unknown"
    )
  )