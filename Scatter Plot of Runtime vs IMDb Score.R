
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")


library(dplyr)
library(ggplot2)
library(readr)


data <- read_csv("/Users/haenacho/Downloads/netflix_series.csv")


#Runtime of shows
shows_only <- data %>% filter(series_type == "MOVIE")

ggplot(shows_only, aes(x = runtime, y = imdb_score)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", col = "red", se = FALSE) +  
  labs(title = "Scatter Plot of Runtime vs IMDb Score",
       x = "Runtime (minutes)",
       y = "IMDb Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))




#Runtime of movies 
movies_only <- data %>% filter(series_type == "MOVIE")

ggplot(movies_only, aes(x = runtime, y = imdb_score)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", col = "red", se = FALSE) +  
  labs(title = "Scatter Plot of Runtime vs IMDb Score",
       x = "Runtime (minutes)",
       y = "IMDb Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))



# Create a scatter plot of runtime of all vs IMDb score
ggplot(data, aes(x = runtime, y = imdb_score)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", col = "red", se = FALSE) +  
  labs(title = "Scatter Plot of Runtime vs IMDb Score",
       x = "Runtime (minutes)",
       y = "IMDb Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
