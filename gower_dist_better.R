library(readr)
library(tidyverse)
library(cluster)
library(ggplot2)
library(umap)
library(reshape2)

# Read dataset
anime_dataset <- read_csv("anime_dataset.csv")

# Prepare the data
anime_clean <- anime_dataset |> 
  mutate(
    critic_score = as.numeric(sub("%", "", critic_score)) / 100,
    audience_score = as.numeric(sub("%", "", audience_score)) / 100
  ) |> 
  select(-movieTitle, -`NOTES:`) |> 
  mutate(across(where(is.character), as.factor)) |> 
  mutate(
    rating = factor(rating, ordered = TRUE,
                    levels = c("g", "pg", "pg_13", "r+"))
  )

# Remove row 101
anime_clean <- anime_clean[-101, ]
anime_dataset_clean <- anime_dataset[-101, ]

# Compute Gower distance
gower_dist <- daisy(anime_clean, metric = "gower")

# Convert to similarity
similarity_matrix <- 1 - as.matrix(gower_dist)
diag(similarity_matrix) <- 1

# Helper function to truncate titles
truncate_title <- function(title, max_chars = 30) {
  if (nchar(title) > max_chars) {
    paste0(substr(title, 1, max_chars - 3), "...")
  } else {
    title
  }
}

# Apply truncation to row and column names
truncated_titles <- sapply(anime_dataset_clean$movieTitle, truncate_title)
rownames(similarity_matrix) <- truncated_titles
colnames(similarity_matrix) <- truncated_titles

# Convert to data frame and save
similarity_df <- as.data.frame(similarity_matrix)
write_csv(similarity_df, "anime_similarity_matrix.csv")

##### Plot full heatmap
similarity_long <- melt(similarity_matrix, varnames = c("Anime1", "Anime2"), value.name = "Similarity")

ggplot(similarity_long, aes(x = Anime1, y = Anime2, fill = Similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6)
  ) +
  labs(title = "Anime Similarity Heatmap", fill = "Similarity")

##### Plot small subset heatmap
subset_titles <- truncated_titles[1:20]
sim_subset <- similarity_matrix[subset_titles, subset_titles]
similarity_long_subset <- melt(sim_subset, varnames = c("Anime1", "Anime2"), value.name = "Similarity")

ggplot(similarity_long_subset, aes(x = Anime1, y = Anime2, fill = Similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6)
  ) +
  labs(title = "Anime Similarity Heatmap (Subset)", fill = "Similarity")

#### similar???
library(dplyr)
library(stringr)

# Filter out self-similarity and same first word matches
similarity_long_no_diag <- similarity_long %>%
  filter(Anime1 != Anime2) %>%
  mutate(
    Anime1 = as.character(Anime1),
    Anime2 = as.character(Anime2),
    first_word1 = str_to_lower(word(Anime1, 1)),
    first_word2 = str_to_lower(word(Anime2, 1))
  ) %>%
  filter(first_word1 != first_word2) %>%
  # Keep only one direction to avoid duplicates
  rowwise() %>%
  filter(Anime1 < Anime2) %>%
  ungroup()

# Get top 20 most similar pairs
top20_pairs_unique <- similarity_long_no_diag %>%
  arrange(desc(Similarity)) %>%
  slice(1:20)

# Plot
ggplot(top20_pairs_unique, aes(x = reorder(paste(Anime1, Anime2, sep = " & "), Similarity), y = Similarity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 20 Most Similar Anime Pairs",
    x = "Anime Pairs",
    y = "Similarity"
  )
