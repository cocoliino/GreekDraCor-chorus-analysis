library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# PLOTS comparing chorus centrality metrics and structural features across different authors and chorus types
# utilizing the choralIdentity_df we created in the previous step, which 
# has one row per chorus node with all the relevant attributes
# important! we are plotting the centrality metrics for the chorus nodes, not for the plays
# this means that if a play has multiple chorus nodes (e.g. Ichneutae has 3 choruses), 
# we will have 3 rows for that play, and each chorus node's centrality metrics will be plotted separately
# maybe interesting to also plot the average centrality metrics for the chorus nodes in each play, to see if there are differences at the play level as well as at the node level

# - degree centrality: how many connections the chorus has to other nodes in the network, indicating its level of interaction
# - betweenness centrality: how much the chorus serves as a bridge or connector in the network
# - eigenvector centrality: how influential the chorus is in the network, taking into account the centrality of its neighbors
# - closeness centrality: how close the chorus is to all other nodes in the network, indicating how quickly it can interact with others
# add: community membership: which community or cluster the chorus belongs to in the network, which can indicate its role

# additional structural features to analyze:
# structural features to be plotted:
# - number of chorus nodes in the play: does having more chorus nodes affect the centrality metrics?
# - is the chorus a group or individual: does being a group chorus vs an individual chorus affect the centrality metrics?
# - sex of the chorus nodes: does the sex of the chorus nodes affect their centrality metrics?

# 1: Degree Centrality by Playwright -----
# plotting the distribution of chorus degree centrality across playwrights
# boxplots with jittered points to show variability and individual play values
fig1 <- ggplot(choralIdentity_df, 
               aes(
                 x = author, 
                 y = degree, 
                 fill = author)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  scale_author_fill +
  labs(
    title = "Chorus Degree",
    x = NULL,
    y = "Degree"
  ) +
  theme_pub +
  theme(legend.position = "none")
fig1
ggsave("fig1_chorus_degree.png", fig1, width = 6, height = 4, dpi = 300)

# 2: Betweenness Centrality by Playwright -----
# plotting the distribution of chorus betweenness centrality across playwrights
# this will help us see if there are differences in how much the chorus serves as a bridge or connector in the networks of different authors' plays,
fig2 <- ggplot(choralIdentity_df, aes(x = author, y = betweenness, fill = author)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  scale_author_fill +
  labs(
    title = "Chorus Betweenness",
    x = NULL,
    y = "Betweenness"
  ) +
  theme_pub +
  theme(legend.position = "none")
fig2
ggsave("fig2_chorus_betweenness.png", fig2, width = 6, height = 4, dpi = 300)

# 3: Eigenvector Centrality by Playwright -----
# plotting the distribution of chorus eigenvector centrality across playwrights
# this will help us see if there are differences in how influential the chorus is in the network
fig3 <- ggplot(choralIdentity_df, aes(x = author, y = eigenvector, fill = author)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  scale_author_fill +
  labs(
    title = "Chorus Eigenvector",
    x = NULL,
    y = "Eigenvector Centrality"
  ) +
  theme_pub +
  theme(legend.position = "none")
fig3
ggsave("fig3_chorus_eigenvector.png", fig3, width = 6, height = 4, dpi = 300)

# 4: Closeness Centrality by Playwright -----
# plotting the distribution of chorus closeness centrality across different authors and chorus types
# this will help us see if there are differences in how close the chorus is to all other nodes in the network
# which can indicate how quickly it can interact with others
fig4 <- ggplot(choralIdentity_df, aes(x = author, y = closeness, fill = author)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  scale_author_fill +
  labs(
    title = "Chorus Closeness",
    x = NULL,
    y = "Closeness Centrality"
  ) +
  theme_pub +
  theme(legend.position = "none")
fig4
ggsave("fig4_chorus_closeness.png", fig4, width = 6, height = 4, dpi = 300)

# 5: Degree Centrality by Genre -----
# maybe boxplots with jittered points to show variability and individual play values, similar to the playwright plots, but with genre on the x-axis instead of author 
fig5 <- ggplot(choralIdentity_df, aes(x = genre, y = degree, fill = genre)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  labs(
    title = "Chorus Degree",
    x = NULL,
    y = "Degree"
  ) +
  theme_pub +
  theme(legend.position = "none")
fig5
ggsave("fig5_chorus_degree_genre.png", fig5, width = 6, height = 4, dpi = 300)
# 6: Betweenness Centrality by Genre -----
# plotting the distribution of chorus betweenness centrality across genres
# this will help us see if there are differences in how much the chorus serves as a bridge
fig6 <- ggplot(choralIdentity_df, aes(x = genre, y = betweenness, fill = genre)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  labs(
    title = "Chorus Betweenness",
    x = NULL,
    y = "Betweenness"
  ) +
  theme_pub +
  theme(legend.position = "none")
fig6
ggsave("fig6_chorus_betweenness_genre.png", fig6, width = 6, height = 4, dpi = 300)

# 7: Eigenvector Centrality by Genre -----
# plotting the distribution of chorus eigenvector centrality across genres
# this will help us see if there are differences in how influential the chorus is in the network
fig7 <- ggplot(choralIdentity_df, aes(x = genre, y = eigenvector, fill = genre)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  labs(
    title = "Chorus Eigenvector",
    x = NULL,
    y = "Eigenvector Centrality"
  ) +
  theme_pub +
  theme(legend.position = "none")
fig7
ggsave("fig7_chorus_eigenvector_genre.png", fig7, width = 6, height = 4, dpi = 300)

# 8: Closeness Centrality by Genre -----
# plotting the distribution of chorus closeness centrality across genres
# this will help us see if there are differences in how close the chorus is to all other
# nodes in the network, which can indicate how quickly it can interact with others
fig8 <- ggplot(choralIdentity_df, aes(x = genre, y = closeness, fill = genre)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  labs(
    title = "Chorus Closeness",
    x = NULL,
    y = "Closeness Centrality"
  ) +
  theme_pub +
  theme(legend.position = "none")
fig8
ggsave("fig8_chorus_closeness_genre.png", fig8, width = 6, height = 4, dpi = 300)

# multiple plots together for easier comparison
fig1 + fig2 + fig3 + fig4 + fig5 + fig6 + fig7 + fig8 + plot_layout(ncol = 4) +
  plot_annotation(title = "Chorus Centrality Metrics by Playwright and Genre") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
ggsave("fig_chorus_centrality_comparison.png", width = 16, height = 8, dpi = 300)

# 9: degree vs. betweenness node level by Genre----

chorus_node_df <- choralIdentity_df %>%
  mutate(
    deg_norm = degree / max(degree, na.rm = TRUE),
    btw_norm = betweenness / max(betweenness, na.rm = TRUE)
  )

deg_med <- median(chorus_node_df$deg_norm, na.rm = TRUE)
btw_med <- median(chorus_node_df$btw_norm, na.rm = TRUE)

p_struct_node <- ggplot(chorus_node_df, aes(deg_norm, btw_norm, color = genre, shape = genre)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_vline(xintercept = deg_med, linetype = "dashed") +
  geom_hline(yintercept = btw_med, linetype = "dashed") +
  annotate("text", x = 0.1, y = 0.6, label = "Broker", alpha = 0.6) +
  annotate("text", x = 0.9, y = 0.6, label = "Hub", alpha = 0.6) +
  annotate("text", x = 0.9, y = 0.05, label = "Popular", alpha = 0.6) +
  annotate("text", x = 0.1, y = 0.05, label = "Peripheral", alpha = 0.6) +
  labs(
    title = "Chorus Structural Roles (Node-level)",
    subtitle = "Degree vs Betweenness (normalised)",
    x = "Normalised Degree",
    y = "Normalised Betweenness"
  ) +
  theme_pub
p_struct_node
ggsave("fig_chorus_structural_roles_node.png", p_struct_node, width = 6, height = 4, dpi = 300)

# 10: Chorus Structural Role play level by Genre ----
chorus_play_df <- chorus_play_df %>%
  mutate(
    deg_norm = degree / max(degree, na.rm = TRUE),
    btw_norm = betweenness / max(betweenness, na.rm = TRUE)
  )

deg_med_play <- median(chorus_play_df$deg_norm, na.rm = TRUE)
btw_med_play <- median(chorus_play_df$btw_norm, na.rm = TRUE)

p_struct_play <- ggplot(chorus_play_df, aes(deg_norm, btw_norm, color = genre, shape = genre)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_vline(xintercept = deg_med_play, linetype = "dashed") +
  geom_hline(yintercept = btw_med_play, linetype = "dashed") +
  labs(
    title = "Chorus Structural Roles (Play-level)",
    x = "Normalised Degree",
    y = "Normalised Betweenness"
  ) +
  theme_pub
p_struct_play
ggsave("fig_chorus_structural_roles_play.png", p_struct_play, width = 6, height = 4, dpi = 300)

# 11: role distribution by genre ----

# node level:
role_node <- chorus_node_df %>%
  mutate(
    role = case_when(
      deg_norm < deg_med & btw_norm < btw_med ~ "Peripheral",
      deg_norm >= deg_med & btw_norm < btw_med ~ "Popular",
      deg_norm < deg_med & btw_norm >= btw_med ~ "Broker",
      TRUE ~ "Hub"
    )
  ) %>%
  count(genre, role) %>%
  group_by(genre) %>%
  mutate(prop = n / sum(n))

p_role_node <- ggplot(role_node, aes(genre, prop, fill = role)) +
  geom_col() +
  labs(title = "Structural Roles (Node-level)", y = "Proportion") +
  theme_pub
p_role_node
ggsave("fig_chorus_structural_roles_node_dist.png", p_role_node, width = 6, height = 4, dpi = 300)

# play level:
role_play <- chorus_play_df %>%
  mutate(
    role = case_when(
      deg_norm < deg_med_play & btw_norm < btw_med_play ~ "Peripheral",
      deg_norm >= deg_med_play & btw_norm < btw_med_play ~ "Popular",
      deg_norm < deg_med_play & btw_norm >= btw_med_play ~ "Broker",
      TRUE ~ "Hub"
    )
  ) %>%
  count(genre, role) %>%
  group_by(genre) %>%
  mutate(prop = n / sum(n))

p_role_play <- ggplot(role_play, aes(genre, prop, fill = role)) +
  geom_col() +
  labs(title = "Structural Roles (Play-level)", y = "Proportion") +
  theme_pub
p_role_play
ggsave("fig_chorus_structural_roles_play_dist.png", p_role_play, width = 6, height = 4, dpi = 300)

# 12: time plot play level ----
chorus_play_df <- chorus_play_df %>%
  mutate(
    deg_norm = degree / max(degree, na.rm = TRUE)
  )

chorus_play_df <- chorus_play_df %>%
  mutate(
    deg_norm = degree / max(degree, na.rm = TRUE)
  )

chorus_play_df <- chorus_play_df %>%
  mutate(
    deg_norm = degree / max(degree, na.rm = TRUE)
  )

# plot degree over time by genre, with regression line to show overall trend, and points colored by author and shaped by genre
p_time_genre <- ggplot(
  chorus_play_df,
  aes(x = year, y = deg_norm, color = author, shape = genre)
) +
  geom_point(size = 3, alpha = 0.9) +
  
  # regression line (overall trend)
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = TRUE,
    linetype = "dashed",
    color = "black"
  ) +
  
  scale_author_color +  # your custom colors
  
  labs(
    title = "Chorus Degree over Time by Genre",
    x = "Year (BCE)",
    y = "Normalised Degree",
    shape = "Genre",
    color = "Playwright",
    note = "no Satyr Play Data point because it no normalized Year"
  ) +
  theme_pub
p_time_genre
ggsave("fig_chorus_degree_time_genre.png", p_time_genre, width = 6, height = 4, dpi = 300)

# plot together for easier comparison
# @ could be nice to remove the legends from the node and play structural role plots to save space
final_plot <- (p_struct_node + p_struct_play) /
  (p_role_node + p_role_play) /
  p_time_genre +
  plot_annotation(title = "Chorus Structural Roles and Time Trends") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))  

final_plot
ggsave("fig_chorus_structural_roles_time.png", final_plot, width = 12, height = 12, dpi = 300)