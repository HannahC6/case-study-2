install.packages("dplyr")
install.packages("tidyverse")

library(dplyr)
library(tidyverse)


imdb <- read.csv("/Users/haenacho/Downloads/netflix_series.csv")

films_info <- imdb

ggplot(films_info, aes(x = imdb_score, y = imdb_votes))+
  geom_point(aes(col = series_type)) +
  geom_smooth(method = 'lm', col = 'black', size = 1.5) +
  xlim(c(0, 10)) + 
  ylim(c(0, 1000000)) +
  labs(title = 'Correlation between IMDb score and votes',
       subtitle = 'of all films and shows on Netflix',
       y = 'Votes',
       x = 'Score') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


