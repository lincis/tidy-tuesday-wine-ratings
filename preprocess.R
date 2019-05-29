library(dplyr)
library(tidyr)

wine.ratings <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv"
) %>%
  rename(ID = X1)

saveRDS(wine.ratings, "wine.ratings.rds")

wine.ratings %>%
  # head(10) %>%
  select(ID, points, description) %>%
  unnest(
    words = strsplit(description, split = "([\\. \\,\\;0-9\\?\\!]+)")
  ) %>%
  select(-description) %>%
  mutate(
    words = tolower(words)
  ) %>%
  filter(grepl("^[^0-9]*^", words)) %>%
  filter(nchar(words) > 2) %>%
  saveRDS("words.scores.rds")

readRDS("words.scores.rds") %>%
  group_by(words) %>%
  summarise(
    occurences.unique = length(unique(ID))
  ) %>%
  ungroup() %>%
  mutate(
    rate.unique = occurences.unique / nrow(wine.ratings)
  ) %>%
  arrange(-rate.unique) %>%
  saveRDS("words.occurences.rds")
