install.packages("dplyr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyverse")
install.packages("forcats")

library(dplyr)
library(tidytext)
library(ggplot2)
library(readr)
library(tidyverse)
library(forcats)

data <- read.csv("/Users/haenacho/Downloads/netflix_series.csv")

# Separate genres into multiple rows
data_genres <- data %>%
  separate_rows(genres, sep = ", ") %>%
  mutate(genres = str_trim(genres),
         genres = tolower(genres)) %>%
  filter(genres != "", !is.na(genres))


# Calculate the frequency of each genre
genre_counts <- data_genres %>%
  count(genres, name = "freq")

# Join the frequencies back to the original data
data_genres <- data_genres %>%
  left_join(genre_counts, by = "genres")

# Reorder the genres based on their frequencies
data_genres <- data_genres %>%
  mutate(genres = fct_reorder(genres, freq, .desc = TRUE))

# Create a box plot with median value annotations
ggplot(data_genres, aes(x = genres, y = imdb_score)) +
  geom_boxplot() +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(..y.., 2)),
    vjust = -0.5,
    colour = "brown",
    size = 2
  ) +
  labs(title = "Box Plot of IMDb Scores by Genre",
       subtitle = "in the order of popularity",
       x = "Genre",
       y = "IMDb Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

