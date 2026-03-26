library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)

# color palette for chorus types (group vs individual vs mixed)
scale_chorus_fill <- scale_fill_manual(values = c(
  "group"      = "grey50",
  "individual" = "steelblue",
  "mixed"      = "darkorange"
))

# what is plotted here?
# 9 figures comparing chorus centrality metrics and structural features across different authors and chorus types 

# Figure 1: Chorus Degree Centrality by Playwright -----
# boxplot of chorus_degree by author, with jittered points overlaid, colored by author 
# this shows the distribution of degree centrality for chorus nodes across different playwrights
# compare how central the chorus is in the networks of different authors' plays
# jittered points provide a sense of the individual play values and variability within each playwright
fig1 <- ggplot(chorus_plays, aes(x = author, y = chorus_degree, fill = author)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  geom_jitter(aes(color = author), width = 0.2, alpha = 0.5, size = 1.5) +
  scale_author_fill +
  scale_author_color +
  labs(
    title = "Chorus Degree Centrality across Playwrights",
    x = NULL,
    y = "Degree"
  ) +
  theme_pub +
  theme(legend.position = "none")

ggsave("fig1_degree_author.png", fig1, width = 6, height = 4, dpi = 300)

# Figure 2: Normalized Degree Centrality by Playwright -----
# boxplot of chorus_degree_norm by author, colored by author
# this shows the distribution of normalized degree centrality for chorus nodes across different playwrights, 
# allowing us to compare how central the chorus is relative to the size of the network
fig2 <- ggplot(chorus_plays, aes(x = author, y = chorus_degree_norm, fill = author)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  scale_author_fill +
  labs(
    title = "Normalized Chorus Degree",
    x = NULL,
    y = "Degree / (n-1)"
  ) +
  theme_pub +
  theme(legend.position = "none")

ggsave("fig2_degree_norm.png", fig2, width = 6, height = 4, dpi = 300)

# Figure 3: Chorus Betweenness Centrality by Playwright -----
# boxplot of chorus_betweenness by author, colored by author
# this shows the distribution of betweenness centrality for chorus nodes across different playwrights,
# allowing us to compare how much the chorus serves as a bridge or connector
fig3 <- ggplot(chorus_plays, aes(x = author, y = chorus_betweenness, fill = author)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  scale_author_fill +
  labs(
    title = "Chorus Betweenness Centrality",
    x = NULL,
    y = "Betweenness"
  ) +
  theme_pub +
  theme(legend.position = "none")

ggsave("fig3_betweenness.png", fig3, width = 6, height = 4, dpi = 300)

# Figure 4: Chorus Eigenvector Centrality by Playwright -----
# boxplot of chorus_eigenvector by author, colored by author
# this shows the distribution of eigenvector centrality for chorus nodes across different playwrights,
# allowing us to compare how connected the chorus is to other well-connected characters in the network
fig4 <- ggplot(chorus_plays, aes(x = author, y = chorus_eigenvector, fill = author)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  scale_author_fill +
  labs(
    title = "Chorus Eigenvector Centrality",
    x = NULL,
    y = "Eigenvector"
  ) +
  theme_pub +
  theme(legend.position = "none")

ggsave("fig4_eigenvector.png", fig4, width = 6, height = 4, dpi = 300)

# Figure 5: Chorus Degree Centrality by Chorus Type -----
# boxplot of chorus_degree by chorus_type (group vs individual vs mixed), colored by chorus_type
# this shows how the degree centrality of chorus nodes varies based on their structural identity (group vs individual vs mixed)
# we can compare whether group choruses tend to have higher or lower degree centrality than individual choruses
# how mixed choruses fit into this pattern
fig5 <- ggplot(chorus_plays, aes(x = chorus_type, y = chorus_degree)) +
  geom_boxplot(fill = "#cccccc", alpha = 0.85, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  labs(
    title = "Chorus Structure vs Degree Centrality",
    x = "Chorus Type",
    y = "Degree"
  ) +
  theme_pub

ggsave("fig5_chorus_structure.png", fig5, width = 6, height = 4, dpi = 300)

# Figure 6: Distribution of Chorus Types by Author -----
# stacked bar chart of chorus_type by author, filled by chorus_type
# this shows the distribution of chorus types (group vs individual vs mixed) across different playwrights
# we can compare which authors tend to use more group choruses vs individual choruses.
# the stacked bars show the proportion of each chorus type within each author's plays,
# allowing us to see if certain authors favor specific chorus structures
fig6 <- ggplot(chorus_plays, aes(x = author, fill = chorus_type)) +
  geom_bar(position = "fill", color = "black", alpha = 0.9) +
  scale_chorus_fill +
  labs(
    title = "Distribution of Chorus Structures by Author",
    x = NULL,
    y = "Proportion",
    fill = "Chorus Type"
  ) +
  theme_pub

ggsave("fig6_chorus_distribution.png", fig6, width = 6, height = 4, dpi = 300)

# Figure 7: Chorus Degree vs Network Density -----
# scatter plot of chorus_degree vs density, colored by author, with a linear regression line
# this shows the relationship between the degree centrality of the chorus and the overall density of the play's network, colored by author
# we can see if there is a positive or negative correlation between how central the chorus is and how dense the interactions are in the play
# the regression line helps to visualize the overall trend across all plays, while the colors show if this relationship differs by author
# if the points are clustered by author, it may indicate that certain authors have both higher chorus degree and higher density, or vice versa
# this can help us understand if the chorus's centrality is related to the overall structure of the play's network and if this varies by playwright
fig7 <- ggplot(chorus_plays, aes(x = density, y = chorus_degree)) +
  geom_point(aes(color = author), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_author_color +
  labs(
    title = "Chorus Degree vs Network Density",
    x = "Density",
    y = "Degree"
  ) +
  theme_pub

ggsave("fig7_density_relationship.png", fig7, width = 6, height = 4, dpi = 300)

# Figure 8: Multi-panel Comparison of Chorus Centrality Metrics by Playwright -----
# create long format dataframe for the three centrality metrics (degree, betweenness, eigenvector)
# plot multi-panel to compare the distribution of all three centrality measures across playwrights in one figure
# the boxplots show central tendency and variability of each metric for each playwright,
# see how the patterns differ across the different centrality measures
# understand if certain authors have consistently higher centrality across all metrics, 
# or if there are differences in how the chorus is positioned in the network depending on the metric used 
chorus_long <- chorus_plays %>%
  select(author, chorus_degree, chorus_betweenness, chorus_eigenvector) %>%
  pivot_longer(
    cols = starts_with("chorus_"),
    names_to = "metric",
    values_to = "value"
  )

fig8 <- ggplot(chorus_long, aes(x = author, y = value, fill = author)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  scale_author_fill +
  facet_wrap(~metric, scales = "free_y") +
  labs(
    title = "Chorus Centrality Measures across Authors",
    x = NULL,
    y = "Value"
  ) +
  theme_pub +
  theme(legend.position = "none")

ggsave("fig8_multipanel.png", fig8, width = 8, height = 5, dpi = 300)

# Figure 9: Chorus Community Membership vs Modularity Score -----
# use computed metrics from 5 CombinedAnalysisAllPlays.R
# plot chorus community membership vs modularity score across plays, colored by author
# chorus_community is a categorical label, so treat it as a factor
fig9 <- ggplot(
    results %>% filter(!is.na(chorus_community)),
    aes(x = factor(chorus_community), y = modularity, color = author)
  ) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.7) +
  scale_author_color +
  labs(
    title = "Chorus Community Membership vs Modularity",
    x = "Chorus Community (categorical)",
    y = "Modularity Score",
    color = "Author"
  ) +
  theme_pub
ggsave("fig9_community_modularity.png", fig9, width = 6, height = 4, dpi = 300)

# Preview all figures
fig1
fig2
fig3
fig4
fig5
fig6
fig7
fig8
fig9