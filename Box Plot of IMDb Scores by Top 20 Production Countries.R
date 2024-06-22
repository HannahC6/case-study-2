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

# Separate production countries into multiple rows
data_countries <- data %>%
  separate_rows(production_countries, sep = ", ") %>%
  mutate(production_countries = str_trim(production_countries)) %>%
  filter(!is.na(production_countries) & !is.na(imdb_score))


# Count the number of entries for each production country
country_counts <- data_countries %>%
  count(production_countries, name = "frequency") %>%
  arrange(desc(frequency))

# Get the top 15 countries
top_countries <- head(country_counts, 20)

# Filter the original data to include only the top 15 countries
filtered_data <- data_countries %>%
  filter(production_countries %in% top_countries$production_countries)

# Join the frequency information back to the filtered data
filtered_data <- filtered_data %>%
  left_join(top_countries, by = "production_countries")

# Reorder the countries by frequency
filtered_data <- filtered_data %>%
  mutate(production_countries = fct_reorder(production_countries, -frequency))

# Calculate the median IMDb score for each country
median_scores <- filtered_data %>%
  group_by(production_countries) %>%
  summarize(median_score = median(imdb_score, na.rm = TRUE))

# Create a box plot of IMDb scores by the top 15 production countries with median annotations
ggplot(filtered_data, aes(x = production_countries, y = imdb_score)) +
  geom_boxplot() +
  geom_text(data = median_scores, 
            aes(x = production_countries, y = median_score, label = round(median_score, 2)),
            position = position_dodge(width = 0.75), vjust = -0.5, colour = "blue", size = 2) +
  labs(title = "Box Plot of IMDb Scores by Top 20 Production Countries",
       x = "Production Country",
       y = "IMDb Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5),
        plot.title = element_text(hjust = 0.5))

