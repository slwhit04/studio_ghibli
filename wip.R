#### load libraries 

library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(readxl)


##### read in data
ANIME <- read_excel("ANIME.xlsx")
critic_reviews <- read_csv("ghibli/critic_reviews.csv/critic_reviews.csv")
movies <- read_csv("ghibli/movies.csv/movies.csv")
studioghibli <- read.csv("C:/Users/19132/Downloads/Studio Ghibli.csv", header=FALSE)
rotten_tomatoes_movies <- read_csv("ghibli/rotten_tomatoes_movies.csv")
anime <- read_csv("ghibli/anime.csv")
anime_dataset <- read_csv("anime_dataset.csv")


#### clean studioghibli
colnames(studioghibli) <- studioghibli[1, ]

studioghibli <- studioghibli[-1, ]


# changing class
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

# pom poko revenue listed in yen instead of usd
studioghibli$Revenue[studioghibli$Name == "Pom Poko"] <- 29669178


#### clean movies
movies <- movies |> 
  mutate(movieTitle = str_to_upper(str_squish(str_trim(movieTitle))))




#### clean anime
anime <- anime %>%
  # Keep only the specified columns
  select(score, scored_by, rating, genres, themes, demographics, title_english) %>%
  # Rename the columns
  rename(
    score_animelist = score,
    movieTitle = title_english
  ) %>%
  # Make movie titles all uppercase
  mutate(
    movieTitle = str_to_upper(movieTitle),
    # Clean up brackets and quotes in genres/themes
    genres = str_remove_all(genres, "\\[|\\]|'"),
    themes = str_remove_all(themes, "\\[|\\]|'")
  ) %>%
  # Split genres and themes into separate columns
  separate(genres, into = c("genre1", "genre2", "genre3", "genre4"), sep = ",\\s*", fill = "right") %>%
  separate(themes, into = c("theme1", "theme2", "theme3", "theme4"), sep = ",\\s*", fill = "right")


anime <- anime |> 
  mutate(movieTitle = ifelse(movieTitle == "YOUR NAME.", "YOUR NAME", movieTitle))


#### clean ANIME


ANIME <- ANIME %>%
  rename(movieTitle = NAME)

ANIME <- ANIME %>%
  mutate(movieTitle = ifelse(movieTitle == "NAUSICAA OF THE VALLEY OF THE WIND",
                             "NAUSICAÄ OF THE VALLEY OF THE WIND",
                             movieTitle))


ANIME <- ANIME %>%
  mutate(movieTitle = case_when(
    movieTitle == "THE TALE OF THE PRINCECSS KAGUYA" ~ "THE TALE OF THE PRINCESS KAGUYA",
    movieTitle == "DEMON SLAYER -KIMESTSU NO YAIBA- THE MOVIE: MUGEN TRAIN" ~ "DEMON SLAYER -KIMETSU NO YAIBA- THE MOVIE: MUGEN TRAIN",
    movieTitle == "DRAGON BALL SUPER:BROLY" ~ "DRAGON BALL SUPER: BROLY",
    movieTitle == "ON-GAKU:OUR SOUND" ~ "ON-GAKU: OUR SOUND",
    movieTitle == "THE CASTLE OF CAGLISTRO" ~ "THE CASTLE OF CAGLIOSTRO",
    TRUE ~ movieTitle
  ))

#### joining data my nightmare


merged <- movies %>%
  left_join(anime, by = "movieTitle")

anime_dataset <- merged %>%
  right_join(ANIME, by = "movieTitle") |> 
  select(-movieId, -movieURL, )

write_csv(anime_dataset, "anime_dataset.csv")


#### EDA


ghibli <- anime_dataset %>% filter(STUDIO == "Ghibli")
non_ghibli <- anime_dataset %>% filter(STUDIO != "Ghibli")

library(ggplot2)


# genre

anime_dataset %>%
  mutate(is_ghibli = if_else(STUDIO == "Ghibli", "Ghibli", "Other")) %>%
  pivot_longer(
    cols = starts_with("genre"), 
    names_to = "genre_rank", 
    values_to = "genre"
  ) %>%
  filter(!is.na(genre) & genre != "") %>%
  count(is_ghibli, genre, sort = TRUE) %>%
  ggplot(aes(x = reorder(genre, n), y = n, fill = is_ghibli)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Genre Counts (All Genre Columns): Ghibli vs. Others",
    x = "Genre",
    y = "Count"
  )


# theme

anime_dataset %>%
  mutate(is_ghibli = if_else(STUDIO == "Ghibli", "Ghibli", "Other")) %>%
  pivot_longer(
    cols = starts_with("theme"), 
    names_to = "theme_rank", 
    values_to = "theme"
  ) %>%
  filter(!is.na(theme) & theme != "") %>%
  count(is_ghibli, theme, sort = TRUE) %>%
  ggplot(aes(x = reorder(theme, n), y = n, fill = is_ghibli)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Theme Counts (All Theme Columns): Ghibli vs. Others",
    x = "Theme",
    y = "Count"
  )

# setting
anime_dataset %>%
  mutate(is_ghibli = if_else(STUDIO == "Ghibli", "Ghibli", "Other")) %>%
  count(is_ghibli, SETTING, sort = TRUE) %>%
  ggplot(aes(x = reorder(SETTING, n), y = n, fill = is_ghibli)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Setting Counts: Ghibli vs. Others",
       x = "Setting",
       y = "Count")


# hair and eye color
anime_dataset %>%
  mutate(is_ghibli = if_else(STUDIO == "Ghibli", "Ghibli", "Other")) %>%
  count(is_ghibli, MC_HAIRCOLOR, MC_EYECOLOR) %>%
  group_by(is_ghibli) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = MC_HAIRCOLOR, y = MC_EYECOLOR, fill = prop)) +
  geom_tile(color = "white") +
  facet_wrap(~is_ghibli) +
  labs(title = "Hair vs. Eye Color Patterns: Ghibli vs. Others", 
       x = "Hair Color", y = "Eye Color", fill = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










