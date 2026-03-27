# Corpus Analysis with GreekDraCor from API via RDracor package

# goal:
# overview of the GreekDraCor corpus in terms of metadata (playwrights, genres, years)
# plot choral presence across the corpus, to inform subsequent analyses of choral identity and co-occurrence networks

library(rdracor) # for accessing Dracor corpora
library(dplyr) # for data manipulation
library(ggplot2) # for plotting

# 1: Overview of GreekDraCor corpus ------------------------------------------------

greek <- get_dracor(corpus = "greek", full_metadata = TRUE)

summary(greek)

# 40 plays in Greek Drama Corpus	
# Corpus id: greek, repository: https://github.com/dracor-org/greekdracor	
# Description: 39 plays derived from [Perseus Digital Library](http://www.perseus.tufts.edu/hopper/opensource/download), licenced under CC BY-SA 3.0 US. Menander's comedy "Dyskolos" derived from Wikisource, licenced under CC BY-SA 3.0. Maintained by Julia Jennifer Beine and Frank Fischer. Corpus description and list of enhancements in the [README on GitHub](https://github.com/dracor-org/greekdracor).
# No information on written years	
# Premiere years (range): -472–-316	
# No information on years of the first printing

# what metadata fields are available?
colnames(greek)

# interesting metadata fields for overview:

# playName: "playwright-playname"
# firstAuthorName: "playwright"
# yearNormalized: "year of premiere, normalized to a common era (CE/BCE)
# normalizedGenre: "genre of the play, normalized to a common set of genres (e.g., Tragedy, Comedy, Satyr Play)"
# characterNames: "list of character names in the play, including the chorus" 

# list all plays from Greek Corpus by playName with normalizedGenre
all_plays <- greek %>% select(playName, normalizedGenre)
all_plays

# list all plays by Genre and Playwright
all_plays_by_genre <- greek %>% 
  group_by(normalizedGenre) %>% 
  summarise(
    n = n(), 
    playwrights = paste(unique(firstAuthorName), collapse = ", "))
all_plays_by_genre

# 12 Comedy (Aristophanes and Menander)
# 1 Saryr (Sophocles)
# 27 Tragedy (Euripides, Aeschylus, Sophocles)

# 2: Plot Overview of Genre and Year for Playwrights ------------------------------------------------

#prepare data for year plotting
yearPlot_df <- greek %>%
  filter(!is.na(yearNormalized),
         !is.na(firstAuthorName),
         !is.na(normalizedGenre)) %>%
  mutate(
    year = as.numeric(yearNormalized),
    author = factor(firstAuthorName),
    genre = factor(normalizedGenre)
  )

#check which plays have been filtered out
filtered_out <- greek %>%
  filter(is.na(yearNormalized) | is.na(firstAuthorName) | is.na(normalizedGenre)) %>%
  select(playName, yearNormalized, firstAuthorName, normalizedGenre)
filtered_out
# two plays by Sophocles because they do not have a year, but they do have a genre and author
# 1. sophocles-ichneutae, the only Satyr play -> left out if year-based trend analysis, will be analyzed later for choral identity and co-occurrence networks
# 2. sophocles-trachiniae, Tragedy -> left out of year-based trend analysis, but will be included in genre-based analysis and choral co-occurrence network analysis

# plot Playwrights and Genres over Year
# use scale_author_color for consistent colors across all plots
# genre is shape
plot1 <- ggplot(yearPlot_df, aes(x = year, y = author, color = author, shape = genre)) +
  geom_point(size = 3) +
  scale_author_color +
  labs(title = "Overview of GreekDraCor Corpus",
       x = "Year of Premiere (normalized)",
       y = "Playwright",
       color = "Playwright",
       shape = "Genre") +
  theme_pub

plot1

ggsave("plot1_CorpusOverview.png", plot1, width = 7, height = 4, dpi = 300)

# Notes:
# No satyr genre shown since the only satyr play was filtered due to missing year.
# Clustering of authors and genres in similar time periods is visible.
# Menander occupies a special late placement with only one comedy play.
# The corpus is missing several Euripides plays due to copyright:
# Alcestis, Andromache, Cyclops, Heracleidae, Hippolytus, Medea
# Cyclops would have been interesting to analyze as a satyr play.