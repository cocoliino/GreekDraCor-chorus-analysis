library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)

# what is plotted here?
# 9 figures comparing chorus centrality metrics and structural features across different authors and chorus types 

# FIGURE 1: Degree Centrality by Author
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
    x = "Author",
    y = "Degree"
  ) +
  theme_pub +
  theme(legend.position = "none")

ggsave("fig1_degree_author.png", fig1, width = 6, height = 4, dpi = 300)

# FIGURE 2: Normalized Degree
# boxplot of chorus_degree_norm by author, colored by author
# this shows the distribution of normalized degree centrality for chorus nodes across different playwrights, 
# allowing us to compare how central the chorus is relative to the size of the network in different authors' plays
fig2 <- ggplot(chorus_plays, aes(x = author, y = chorus_degree_norm, fill = author)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  scale_author_fill +
  labs(
    title = "Normalized Chorus Degree",
    x = "Author",
    y = "Degree / (n-1)"
  ) +
  theme_pub +
  theme(legend.position = "none")

ggsave("fig2_degree_norm.png", fig2, width = 6, height = 4, dpi = 300)

# =========================================
# FIGURE 3: Betweenness Centrality
# =========================================
fig3 <- ggplot(chorus_plays, aes(x = author, y = chorus_betweenness, fill = author)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  scale_author_fill +
  labs(
    title = "Chorus Betweenness Centrality",
    x = "Author",
    y = "Betweenness"
  ) +
  theme_pub +
  theme(legend.position = "none")

ggsave("fig3_betweenness.png", fig3, width = 6, height = 4, dpi = 300)

# =========================================
# FIGURE 4: Eigenvector Centrality
# =========================================
fig4 <- ggplot(chorus_plays, aes(x = author, y = chorus_eigenvector, fill = author)) +
  geom_boxplot(alpha = 0.85, color = "black") +
  scale_author_fill +
  labs(
    title = "Chorus Eigenvector Centrality",
    x = "Author",
    y = "Eigenvector"
  ) +
  theme_pub +
  theme(legend.position = "none")

ggsave("fig4_eigenvector.png", fig4, width = 6, height = 4, dpi = 300)

# =========================================
# FIGURE 5: Chorus Structure vs Degree
# =========================================
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

# =========================================
# FIGURE 6: Chorus Type Distribution by Author
# =========================================
fig6 <- ggplot(chorus_plays, aes(x = author, fill = chorus_type)) +
  geom_bar(position = "fill", color = "black", alpha = 0.9) +
  scale_chorus_fill +
  labs(
    title = "Distribution of Chorus Structures by Author",
    x = "Author",
    y = "Proportion",
    fill = "Chorus Type"
  ) +
  theme_pub

ggsave("fig6_chorus_distribution.png", fig6, width = 6, height = 4, dpi = 300)

# =========================================
# FIGURE 7: Degree vs Network Density
# =========================================
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

# =========================================
# FIGURE 8: Multi-panel Centrality Comparison
# =========================================
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
    x = "Author",
    y = "Value"
  ) +
  theme_pub +
  theme(legend.position = "none")

ggsave("fig8_multipanel.png", fig8, width = 8, height = 5, dpi = 300)

# figure 9 
# use computed metrics from 4 NetworkAnalysisAllPlays.R to plot chorus community membership vs modularity score across plays, colored by author
fig9 <- ggplot(results, aes(x = chorus_community, y = modularity, color = author)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_author_color +
  labs(
    title = "Chorus Community Membership vs Modularity",
    x = "Chorus Community Membership",
    y = "Modularity Score",
    color = "Author"
  )
ggsave("fig9_community_modularity.png", fig9, width = 6, height = 4, dpi = 300)

# =========================================
# PREVIEW
# =========================================
fig1
fig2
fig3
fig4
fig5
fig6
fig7
fig8
fig9
