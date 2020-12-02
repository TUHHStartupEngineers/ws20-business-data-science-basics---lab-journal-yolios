library(tidyverse)
library(rvest)
library(stringr)
library(httr)

# From HTML Table
snp_500_tbl <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
  html_nodes(css = "#constituents") %>%
  html_table() %>%
  .[[1]] %>%
  as_tibble()

# From DOM Objects
resp <- GET(url = "https://www.imdb.com/chart/top/?ref_=nv_mv_250",  
            add_headers('Accept-Language' = "en-US, en;q=0.5"))

imdb_page <- content(resp)

imdb_title_column <- imdb_page %>%
  html_nodes(css = ".titleColumn")
  
imdb_rank <- imdb_title_column %>%
  html_text() %>%
  str_extract("(?<= )[0-9]+")

imdb_title <- imdb_title_column %>%
  html_node(xpath = "a") %>%
  html_text()

imdb_year <- imdb_title_column %>%
  html_node(xpath = "span") %>%
  html_text() %>%
  str_extract("[^()]+")

imdb_people <- imdb_title_column %>%
  html_node(xpath = "a") %>%
  html_attr(name="title")

imdb_rating_column <- imdb_page %>%
  html_nodes(css = ".imdbRating")

imdb_rating <- imdb_rating_column %>%
  html_node(xpath = "strong") %>%
  html_text()

imdb_ratings <- imdb_rating_column %>%
  html_node(xpath = "strong") %>%
  html_attr(name = "title") %>%
  str_extract("(?<=based on ).*(?= user ratings)")

imdb_tbl <- tibble(rank=imdb_rank, title=imdb_title, year=imdb_year,
    people=imdb_people, rating=imdb_rating, num_ratings=imdb_ratings)
