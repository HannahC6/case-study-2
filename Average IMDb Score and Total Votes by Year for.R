install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyr")
install.packages("stringr")


library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)


data <- read_csv("/Users/haenacho/Downloads/netflix_series.csv")

# Define the specific country you are interested in
specific_country <- "GB"

# Filter data for the specific country
data_filtered <- data %>%
  separate_rows(production_countries, sep = ", ") %>%
  mutate(production_countries = str_trim(production_countries)) %>%
  filter(production_countries == specific_country)

# Calculate the average IMDb score and total number of votes by year
data_summary <- data_filtered %>%
  group_by(release_year) %>%
  summarise(avg_imdb_score = mean(imdb_score, na.rm = TRUE),
            total_votes = sum(imdb_votes, na.rm = TRUE))

# Apply a logarithmic transformation to the total votes
data_summary <- data_summary %>%
  mutate(log_total_votes = log10(total_votes + 1))  # Adding 1 to avoid log(0)

# Create the line graph with a transformed y-axis for total votes
ggplot(data_summary, aes(x = release_year)) +
  geom_line(aes(y = avg_imdb_score, colour = "Average IMDb Score")) +
  geom_line(aes(y = log_total_votes, colour = "Total Votes (Log)")) +
  scale_y_continuous(
    name = "Average IMDb Score",
    sec.axis = sec_axis(~ . * max(data_summary$total_votes) / max(data_summary$log_total_votes), name = "Total Votes (Log)")
  ) +
  labs(title = paste("Average IMDb Score and Total Votes by Year for", specific_country),
       x = "Year") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(colour = "darkolivegreen"),
    axis.title.y.right = element_text(colour = "orange")
  ) +
  scale_colour_manual(
    name = "Index",
    values = c("Average IMDb Score" = "darkolivegreen", "Total Votes (Log)" = "orange")
  )

