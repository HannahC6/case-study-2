install.packages("dplyr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(tidytext)
library(ggplot2)
library(readr)


data <- read.csv("/Users/haenacho/Downloads/netflix_series.csv")

# Split the genre values into separate rows
data_split <- data %>%
  separate_rows(genres, sep = ",") %>%
  mutate(genres = str_trim(genres),
         genres = tolower(genres)) %>%
  filter(genres != "", !is.na(genres))

# Count the frequency of genres
genre_count <- data_split %>%
  group_by(genres) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(genre_count, aes(x = reorder(genres, -count), y = count)) +
  geom_bar(stat = "identity", fill = 'brown') +
  geom_text(aes(label = count), vjust = -0.3, size = 2) +
  labs(title = "Popularity in Genres", x = "Genre", y = "Count") +
  theme(axis.text.x = element_text(size = 7, angle = 45),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



