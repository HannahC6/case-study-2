SELECT title, series_type, release_year, imdb_score
FROM CS_1.netflix_series
WHERE series_type = 'show'
ORDER BY imdb_score DESC
LIMIT 100;  