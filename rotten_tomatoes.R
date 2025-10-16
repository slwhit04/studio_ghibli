##### Load Libraries
library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(gt)


##### Read in Data
critic_reviews <- read_csv("ghibli/critic_reviews.csv/critic_reviews.csv")
movies <- read_csv("ghibli/movies.csv/movies.csv")
user_reviews <- read_csv("ghibli/user_reviews.csv/user_reviews.csv")
studioghibli <- read.csv("C:/Users/19132/Downloads/Studio Ghibli.csv", header=FALSE)
rotten_tomatoes_movies <- read_csv("ghibli/rotten_tomatoes_movies.csv")


##### Cleaning Data
colnames(studioghibli) <- studioghibli[1, ]

studioghibli <- studioghibli[-1, ]


### changing class
studioghibli$Revenue <- as.numeric(gsub("[$,]", "", studioghibli$Revenue))
studioghibli$Budget <- as.numeric(gsub("[$,]", "", studioghibli$Budget))
studioghibli$Year <- as.integer(studioghibli$Year)
studioghibli$Director <- as.factor(studioghibli$Director)
studioghibli$Screenplay <- as.factor(studioghibli$Screenplay)
studioghibli$`Genre 1` <- as.factor(studioghibli$`Genre 1`)
studioghibli$`Genre 2` <- as.factor(studioghibli$`Genre 2`)
studioghibli$`Genre 3` <- as.factor(studioghibli$`Genre 3`)

studioghibli$Revenue <- as.numeric(gsub("[$,]", "", studioghibli$Revenue)) / 1e6
studioghibli$Revenue <- round(studioghibli$Revenue, 2)



studioghibli_clean <- studioghibli |> 
  rename(movieTitle = Name) |> 
  mutate(
    movieTitle = str_trim(str_remove(movieTitle, "\\s*\\(\\d{4}\\)$"))
  )
studioghibli_clean <- studioghibli_clean |> 
  mutate(movieTitle = str_to_upper(str_squish(str_trim(movieTitle))))

### pom poko revenue listed in yen instead of usd
studioghibli$Revenue[studioghibli$Name == "Pom Poko"] <- 29669178



movies <- movies |> 
  mutate(movieTitle = str_to_upper(str_squish(str_trim(movieTitle))))



japanese_movies <- rotten_tomatoes_movies %>%
  filter(tolower(originalLanguage) == "japanese")

##### Joining Data

movies_joined <- movies |> 
  left_join(critic_reviews, by = "movieId") |> 
  left_join(studioghibli_clean, by = "movieTitle")

movies_joined <- movies_joined |> 
  mutate(is_ghibli = if_else(!is.na(Director), 1, 0))

##### EDA

movies_joined |> 
  filter(movieRank <= 100) |> 
  summarise(
    total_movies   = n_distinct(movieId),
    ghibli_movies  = n_distinct(movieId[is_ghibli == 1]),
    percent_ghibli = 100 * ghibli_movies / total_movies
  )

movies_joined |> 
  filter(movieRank <= 100) |> 
  group_by(is_ghibli) |> 
  summarise(count = n_distinct(movieId)) |> 
  mutate(percent = 100 * count / sum(count),
         label = paste0(ifelse(is_ghibli == 1, "Ghibli", "Non-Ghibli"),
                        " (", round(percent, 1), "%)")) |> 
  ggplot(aes(x = "", y = percent, fill = label)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Percent of Ghibli Movies in Top 100 Anime Films", x = NULL, y = NULL) +
  theme_void() +
  theme(legend.title = element_blank())




# build a top-10 table at the movie level
top100 <- movies_joined |> 
  distinct(movieTitle, movieYear, movieRank, critic_score, audience_score, is_ghibli) %>%
  arrange(movieRank) |> 
  slice_head(n = 100)

top100 |> 
  gt() |> 
  cols_label(
    movieTitle = "Title",
    movieYear = "Year",
    movieRank = "Rank",
    critic_score = "Critic Score",
    audience_score = "Audience Score",
    is_ghibli = "Is Ghibli"
  ) |> 
  tab_style(
    style = list(
      cell_fill(color = "pink"),   # subtle light-blue fill
      cell_text(weight = "bold")      # bold text for emphasis
    ),
    locations = cells_body(columns = everything(), rows = is_ghibli == 1)
  ) |> 
  cols_align(columns = c(movieRank, critic_score, audience_score, is_ghibli), align = "center")


##### compare revenue to rank

adj_data <- tibble(
  movieTitle = str_to_upper(c(
    "Nausicaä of the Valley of the Wind",
    "Castle in the Sky",
    "Grave of the Fireflies",
    "My Neighbor Totoro",
    "Kiki's Delivery Service",
    "Only Yesterday",
    "Porco Rosso",
    "Ocean Waves",
    "Pom Poko",
    "Whisper of the Heart",
    "Princess Mononoke",
    "My Neighbors the Yamadas",
    "Spirited Away",
    "The Cat Returns",
    "Howl's Moving Castle",
    "Tales from Earthsea",
    "Ponyo",
    "The Secret World of Arrietty",
    "From Up on Poppy Hill",
    "The Tale of The Princess Kaguya",
    "The Wind Rises",
    "When Marnie Was There",
    "The Boy and the Heron"
  )),
  Revenue_adj = c(
    10.49, 15.46, 1.46, 114.81, 12.01, 1.13, 80.00, 22.16, 65.75, 75.23, 344.14,
    329.30, 508.68, 98.82, 412.93, 112.13, 310.65, 223.49, 89.80, 34.29,
    165.91, 48.41, 180.85 
  )
)

# Join into ghibli
studioghibli_clean <- studioghibli_clean |>
  left_join(adj_data, by = "movieTitle")


ghibli_rank_revenue <- movies_joined |>
  filter(is_ghibli == 1) |>
  select(movieTitle, movieRank) |>
  distinct() |>
  left_join(studioghibli_clean |> select(movieTitle, Revenue_adj, Revenue), by = "movieTitle")

# Ensure rank is numeric first
ghibli_rank_revenue <- ghibli_rank_revenue |>
  mutate(movieRank = as.numeric(movieRank))

# Plot: Adjusted Revenue vs. Rank
ggplot(ghibli_rank_revenue, aes(x = movieRank, y = Revenue_adj, label = movieTitle)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(nudge_y = 20, size = 3, check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  labs(
    title = "Studio Ghibli: Adjusted Revenue vs. Movie Rank",
    x = "Movie Rank (Lower Rank = Better)",
    y = "Adjusted Revenue (Million USD)"
  ) +
  theme_minimal()



ghibli_rank_revenue |>
  summarise(correlation = cor(movieRank, Revenue_adj, use = "complete.obs", method = "spearman"))





