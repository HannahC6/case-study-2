install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyr")
install.packages("stringr")
install.packages("forcats")


library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(forcats)


data <- read_csv("/Users/haenacho/Downloads/netflix_series.csv")

data_clean <- data %>%
  separate_rows(production_countries, sep = ", ") %>%
  separate_rows(genres, sep = ", ") %>%
  mutate(production_countries = str_trim(production_countries),
         genres = str_trim(genres)) %>%
  filter(!is.na(production_countries) & production_countries != "" & 
           !is.na(genres) & genres != "")

top_countries <- data_clean %>%
  count(production_countries, name = "frequency") %>%
  arrange(desc(frequency)) %>%
  slice(1:20) %>%
  mutate(rank = row_number()) %>%
  pull(production_countries)

# Filter the data to include only the top 20 production countries
data_top_countries <- data_clean %>%
  filter(production_countries %in% top_countries)

country_genre_scores <- data_top_countries %>%
  group_by(production_countries, genres) %>%
  summarize(avg_imdb_score = mean(imdb_score, na.rm = TRUE)) %>%
  ungroup()

country_ranks <- data_top_countries %>%
  count(production_countries, name = "frequency") %>%
  arrange(desc(frequency)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# Merge the ranks back into the main dataset
country_genre_scores <- country_genre_scores %>%
  left_join(country_ranks, by = "production_countries")

# Create a heatmap of average IMDb scores by production country and genre, ordered by rank
ggplot(country_genre_scores, aes(x = genres, y = reorder(production_countries, rank), 
                                 fill = avg_imdb_score)) +
  geom_tile(colour = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = median(country_genre_scores$avg_imdb_score, na.rm = TRUE),
                       name = "Avg IMDb Score") +
  labs(title = "Average IMDb Score by Production Country and Genre",
       x = "Genre",
       y = "Production Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
