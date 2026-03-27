library(ggplot2)
library(dplyr)
library(tidyr)

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
    title = "Chorus Degree Centrality by Playwright",
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
    title = "Chorus Betweenness Centrality by Playwright",
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
    title = "Chorus Eigenvector Centrality by Playwright",
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
    title = "Chorus Closeness Centrality by Playwright",
    x = NULL,
    y = "Closeness Centrality"
  ) +
  theme_pub +
  theme(legend.position = "none")
fig4
ggsave("fig4_chorus_closeness.png", fig4, width = 6, height = 4, dpi = 300)

