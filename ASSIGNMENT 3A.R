# Assignment 3A: Global Baseline Estimate
#
# Approach:
# I implemented the Global Baseline Estimate method using the survey ratings
# in MovieRatings.xlsx. I reshaped the ratings into a long format, treated
# blanks and “?” as missing values, and computed the global mean rating across
# all observed entries. Then I calculated each user’s average rating and each
# movie’s average rating to determine user and movie biases relative to the
# global mean. For each missing rating, I predicted it using the formula:
# global mean + user bias + movie bias, and recommended the highest predicted
# unrated movie for each user.

library(readxl)
library(dplyr)
library(tidyr)

file_path <- "MovieRatings.xlsx"

wide <- read_excel(file_path, sheet = "MovieRatings")

ratings_long <- wide %>%
  pivot_longer(-Critic, names_to = "Movie", values_to = "Rating_raw") %>%
  mutate(
    Rating_raw = as.character(Rating_raw),
    Rating = suppressWarnings(as.numeric(ifelse(Rating_raw == "?", NA, Rating_raw)))
  ) %>%
  select(Critic, Movie, Rating)

global_mean <- mean(ratings_long$Rating, na.rm = TRUE)

user_stats <- ratings_long %>%
  group_by(Critic) %>%
  summarise(
    user_avg = mean(Rating, na.rm = TRUE),
    user_bias = user_avg - global_mean,
    .groups = "drop"
  )

movie_stats <- ratings_long %>%
  group_by(Movie) %>%
  summarise(
    movie_avg = mean(Rating, na.rm = TRUE),
    movie_bias = movie_avg - global_mean,
    .groups = "drop"
  )

predictions <- ratings_long %>%
  left_join(user_stats, by = "Critic") %>%
  left_join(movie_stats, by = "Movie") %>%
  mutate(pred_rating = global_mean + user_bias + movie_bias)

missing_preds <- predictions %>%
  filter(is.na(Rating)) %>%
  arrange(Critic, desc(pred_rating))

recommendations <- missing_preds %>%
  group_by(Critic) %>%
  slice_max(order_by = pred_rating, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Critic, Recommended_Movie = Movie, Predicted_Rating = pred_rating)

print(recommendations)
