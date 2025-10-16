### load libraries
library(ggplot2)
library(dplyr)
library(knitr)
library(stringr)
library(scales)
library(readr)


### read in data

anime <- read_csv("ghibli/anime.csv")
studioghibli <- read.csv("C:/Users/19132/Downloads/Studio Ghibli.csv", header=FALSE)


### little bit of data cleaning

colnames(studioghibli) <- studioghibli[1, ]

studioghibli <- studioghibli[-1, ]

### pom poko revenue listed in yen instead of usd
studioghibli$Revenue[studioghibli$Name == "Pom Poko"] <- 29669178

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

### join datasets

studioghibli_clean <- studioghibli |> 
  rename(title_english = Name) |> 
  mutate(
    title_english = str_trim(str_remove(title_english, "\\s*\\(\\d{4}\\)$"))
  )

anime_data <- anime |> 
  left_join(
    studioghibli_clean |>  select(title_english, Director, Revenue),
    by = "title_english"
  )

anime_data <- anime_data |> 
  mutate(studios = ifelse(anime_id == 572, "['Studio Ghibli']", studios),)


### eda time baby lets gooo

## i have no idea where to start lol

## i need to find variables to compare anime, maybe genre, themes, duration, 
# demographics, rating, source, type



## studio ghibli data

ghibli <- anime_data |> 
  filter(studios == "['Studio Ghibli']") |> 
  filter(!is.na(score), !is.na(Revenue)) |> 
  mutate(
    Miyazaki_flag = ifelse(Director == "Hayao Miyazaki", "Miyazaki", "Non-Miyazaki")
  )




ggplot(ghibli, aes(x = reorder(title_english, Revenue), 
                   y = Revenue, 
                   fill = Miyazaki_flag)) +
  geom_col() +
  geom_text(aes(label = paste0("$", round(Revenue), "M")), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Miyazaki" = "red", "Non-Miyazaki" = "blue")) +
  labs(title = "Studio Ghibli Films by Adjusted Revenue",
       x = "Film", y = "Adjusted Revenue (Millions USD)",
       fill = "Director") +
  theme_minimal()


ggplot(ghibli, aes(x = Revenue, y = score, color = Miyazaki_flag, label = title_english)) +
  geom_point(size = 3) +
  geom_text(vjust = -1, hjust = 0.5, size = 3, check_overlap = TRUE) +
  scale_color_manual(values = c("Miyazaki" = "red", "Non-Miyazaki" = "blue")) +
  labs(title = "Studio Ghibli Films: Score vs. Revenue",
       x = "Revenue (Millions USD)", y = "Score",
       color = "Director") +
  theme_minimal()


### Time adjusted revenue (im not entirely sure what i am doing lol)
# Make a lookup table
adj_data <- tibble(
  title_english = c(
    "Nausicaä of the Valley of the Wind",
    "Castle in the Sky",
    "Grave of the Fireflies",
    "My Neighbor Totoro",
    "Kiki's Delivery Service",
    "Only Yesterday",
    "Porco Rosso",
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
  ),
  Revenue_adj = c(
    10.49, 15.46, 1.46, 114.81, 12.01, 1.13, 80.00, 65.75, 75.23, 344.14,
    329.30, 508.68, 98.82, 412.93, 112.13, 310.65, 223.49, 89.80, 34.29,
    165.91, 48.41, 180.85 
  )
)

# Join into ghibli
ghibli <- ghibli |>
  left_join(adj_data, by = "title_english")



ggplot(ghibli, aes(x = reorder(title_english, Revenue_adj), 
                   y = Revenue_adj, 
                   fill = Miyazaki_flag)) +
  geom_col() +
  geom_text(aes(label = paste0("$", round(Revenue_adj), "M")), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Miyazaki" = "red", "Non-Miyazaki" = "blue")) +
  labs(title = "Studio Ghibli Films by Adjusted Revenue",
       x = "Film", y = "Adjusted Revenue (Millions USD)",
       fill = "Director") +
  theme_minimal()


ggplot(ghibli, aes(x = Revenue_adj, y = score, color = Miyazaki_flag, label = title_english)) +
  geom_point(size = 3) +
  geom_text(vjust = -1, hjust = 0.5, size = 3, check_overlap = TRUE) +
  scale_color_manual(values = c("Miyazaki" = "red", "Non-Miyazaki" = "blue")) +
  labs(title = "Studio Ghibli Films: Score vs. Revenue",
       x = "Revenue (Millions USD)", y = "Score",
       color = "Director") +
  theme_minimal()


## other ideas
# Ratings vs. release year: Miyazaki/Ghibli films compared to industry average
# look only at anime movies for now I think?
# look at post popular genre per year and ghibli movie released per year (plus ghibli genre)

anime_movies <- anime_data |> 
  mutate(
    scored_by = as.numeric(scored_by),   # convert first
    start_year = as.numeric(start_year),
    score = as.numeric(score),
    Miyazaki_flag = ifelse(Director == "Hayao Miyazaki", "Miyazaki", "Non-Miyazaki")
  ) |> 
   filter(scored_by >= 10000 | title == "Kimitachi wa Dou Ikiru ka")


# industry average per year
industry_avg <- anime_movies |> 
  group_by(start_year) |> 
  summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop")

# ghibli average per year (only with director info)
ghibli_avg <- anime_movies |> 
  filter(studios == "['Studio Ghibli']", !is.na(Director)) |> 
  group_by(start_year) |> 
  summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop")

ggplot() +
  # solid industry line
  geom_line(data = industry_avg, aes(x = start_year, y = avg_score), 
            color = "gray50", size = 1) +
  # solid Ghibli line
  geom_line(data = ghibli_avg, aes(x = start_year, y = avg_score), 
            color = "darkgreen", size = 1.2) +
  # points for Ghibli movies (Miyazaki = red, Non-Miyazaki = blue)
  geom_point(data = anime_movies |> filter(studios == "['Studio Ghibli']", !is.na(Director)),
             aes(x = start_year, y = score, color = Miyazaki_flag), size = 3) +
  scale_color_manual(values = c("Miyazaki" = "red", "Non-Miyazaki" = "blue")) +
  scale_x_continuous(breaks = seq(1950, 2025, 10), limits = c(1950, 2025)) +
  labs(title = "Ratings vs. Release Year",
       subtitle = "Gray line = industry average, Green line = Ghibli average",
       x = "Year", y = "Score", color = "Director") +
  theme_minimal()



### scored_by count x start_year

anime_data <- anime_data %>%
  mutate(
    start_year = as.numeric(start_year),
    scored_by = as.numeric(scored_by)
  )


anime_data <- anime_data %>%
  mutate(
    start_year = as.numeric(start_year),
    scored_by = as.numeric(scored_by)
  )

score_counts <- anime_data %>%
  group_by(start_year) %>%
  summarise(
    scored_by_total = sum(scored_by, na.rm = TRUE)
  ) %>%
  ungroup()

nausicaa_point <- data.frame(
  start_year = 1984,
  scored_by_total = 225975,
  label = "Nausicaä: 225,975"
)

spirited_away <- data.frame(
  start_year = 2001,
  scored_by_total = 1246643,
  label = "Spirited Away: 1,246,643"
)

ggplot(score_counts, aes(x = start_year, y = scored_by_total)) +
  geom_line(color = "lightblue", size = 1) +
  geom_point(color = "red", size = 2) +
  # Special Nausicaa point
  geom_point(data = nausicaa_point, aes(x = start_year, y = scored_by_total),
             color = "black", size = 3) +
  geom_text(data = nausicaa_point, aes(x = start_year, y = scored_by_total, label = label),
            vjust = -1, color = "black") +
  geom_point(data = spirited_away, aes(x = start_year, y = scored_by_total),
             color = "black", size = 3) +
  geom_text(data = spirited_away, aes(x = start_year, y = scored_by_total, label = label),
            vjust = -1, color = "black") +
  labs(
    title = "Total Number of Scores by Start Year",
    x = "Start Year",
    y = "Total Scored_by"
  ) +
  scale_x_continuous(
    breaks = seq(min(score_counts$start_year, na.rm = TRUE),
                 max(score_counts$start_year, na.rm = TRUE),
                 by = 10)
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  theme_minimal()

### zoomin in

ggplot(score_counts, aes(x = start_year, y = scored_by_total)) +
  geom_line(color = "lightblue", size = 1) +
  geom_point(color = "red", size = 2) +
  # Nausicaa point + label
  geom_point(data = nausicaa_point, aes(x = start_year, y = scored_by_total),
             color = "black", size = 3) +
  geom_text(data = nausicaa_point, aes(x = start_year, y = scored_by_total, label = label),
            vjust = -1, color = "black") +
  labs(
    title = "Total Number of Scores (1970–1990)",
    x = "Start Year",
    y = "Total Scored_by"
  ) +
  scale_x_continuous(breaks = seq(1970, 1990, by = 5)) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    breaks = seq(0, 3e6, by = 5e5)    # ticks at 0M, 0.5M, 1.0M, ... 3.0M
  ) +
  coord_cartesian(xlim = c(1970, 1990), ylim = c(0, 2e6)) +
  theme_minimal()
