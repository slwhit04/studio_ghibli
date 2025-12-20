library(readr)
library(tidyverse)
library(cluster)
library(ggplot2)
library(umap)


anime_dataset <- read_csv("anime_dataset.csv")

# prepare the data

anime_clean <- anime_dataset |> 
  # convert percentages so its numerical
  mutate(
    critic_score = as.numeric(sub("%", "", critic_score)) / 100,
    audience_score = as.numeric(sub("%", "", audience_score)) / 100
  ) |> 
  # drop identifier columns and notes 
  select(-movieTitle, -`NOTES:`) |> 
  # convert remaining character columns to factors
  mutate(across(where(is.character), as.factor)) |> 
  # convert rating after factor conversion to set order
  mutate(
    rating = factor(rating, ordered = TRUE,
                    levels = c("g", "pg", "pg_13", "r+"))
  )



# compute

gower_dist <- daisy(anime_clean, metric = "gower")

##### try and figure out how to actually use it
### PAM

pam_fit <- pam(gower_dist, k = 4)   # try different k
pam_fit$clustering

mds <- cmdscale(gower_dist, k = 2) |>  as.data.frame()

ggplot(mds, aes(V1, V2, color = factor(pam_fit$clustering))) +
  geom_point(size = 3) +
  labs(color = "Cluster")


# Hierarchical Clustering

hc <- hclust(gower_dist, method = "average")
plot(hc, labels = FALSE)
# no clue what i am looking at 

clusters <- cutree(hc, k = 4)

### Finding Similarities 

gmat <- as.matrix(gower_dist)

# Find most similar to anime #10
order(gmat[10, ])[1:10]

# most similar to studio ghibli
ghibli_idx <- which(anime_clean$STUDIO == "GHIBLI")
ghibli_idx

find_similar <- function(row_index, n = 10) {
  # Distances from this anime to all others
  dists <- gmat[row_index, ]
  
  # Exclude itself (distance 0)
  dists[row_index] <- NA
  
  # Sort and return top n closest titles
  closest_idx <- order(dists, na.last = NA)[1:n]
  
  tibble(
    target_anime = anime_dataset$movieTitle[row_index],
    similar_anime = anime_dataset$movieTitle[closest_idx],
    distance = dists[closest_idx]
  )
}

ghibli_similar <- map_df(ghibli_idx, find_similar, n = 10)

ghibli_similar <- ghibli_similar |> 
  left_join(
    anime_dataset |> 
      select(movieTitle, `NOTES:`),
    by = c("similar_anime" = "movieTitle")
  )

ghibli_similar


anime_labeled <- anime_dataset |> 
  mutate(cluster = pam_fit$clustering)


library(dplyr)
library(purrr)

# Function to find top n most similar non-Ghibli anime for a given anime
find_similar_non_ghibli <- function(row_index, n = 10) {
  # Distances from this anime to all others
  dists <- gmat[row_index, ]
  
  # Exclude itself
  dists[row_index] <- NA
  
  # Get indices sorted by similarity (closest first)
  sorted_idx <- order(dists, na.last = NA)
  
  # Filter out any anime that is also Ghibli
  non_ghibli_idx <- sorted_idx[anime_clean$STUDIO[sorted_idx] != "GHIBLI"]
  
  # Take top n
  closest_idx <- non_ghibli_idx[1:min(n, length(non_ghibli_idx))]
  
  tibble(
    target_anime = anime_dataset$movieTitle[row_index],
    similar_anime = anime_dataset$movieTitle[closest_idx],
    distance = dists[closest_idx]
  )
}

# Indices of Ghibli movies
ghibli_idx <- which(anime_clean$STUDIO == "GHIBLI")

# Find top 10 most similar non-Ghibli anime for each Ghibli movie
ghibli_similar_filtered <- map_df(ghibli_idx, find_similar_non_ghibli, n = 10)

# Optional: join notes or other info
ghibli_similar_filtered <- ghibli_similar_filtered %>% 
  left_join(
    anime_dataset %>% select(movieTitle, `NOTES:`),
    by = c("similar_anime" = "movieTitle")
  )

ghibli_similar_filtered


### heat plot

library(ggplot2)
library(dplyr)

# Create a similarity measure (1 - distance) for plotting
ghibli_similar_filtered <- ghibli_similar_filtered %>%
  mutate(similarity = 1 - distance)

truncate_title <- function(title, max_chars = 30) {
  if (is.na(title)) return(NA)  # handle NA
  if (nchar(title) > max_chars) {
    paste0(substr(title, 1, max_chars - 3), "...")
  } else {
    title
  }
}

# Apply truncation safely
ghibli_similar_filtered <- ghibli_similar_filtered %>%
  mutate(
    target_anime_trunc = sapply(target_anime, truncate_title),
    similar_anime_trunc = sapply(similar_anime, truncate_title)
  )

# Heatmap
ggplot(ghibli_similar_filtered, aes(x = similar_anime_trunc, y = target_anime_trunc, fill = 1 - distance)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8)
  ) +
  labs(
    title = "Studio Ghibli Movies vs. Top Non-Ghibli Similar Anime",
    x = "Similar Anime (Non-Ghibli)",
    y = "Studio Ghibli Movie",
    fill = "Similarity"
  )
