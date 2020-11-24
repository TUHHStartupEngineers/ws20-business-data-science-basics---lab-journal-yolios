library(tidyverse)
library(stringr)
library(httr)
library(glue)
library(jsonlite)

url <- "https://youtube.googleapis.com/youtube/v3/videos"

# Get api response
response <- GET(url, query = list(
  "part" = "snippet,contentDetails,statistics",
  "maxResults" = "10",
  "chart" = "mostPopular",
  "regionCode" = "US",
  "key" = Sys.getenv("YT_API_KEY")))

# Convert response content to object
data <- response$content %>% rawToChar() %>% fromJSON()

# Flatten object for easier access
videos <- data[["items"]] %>% flatten() %>% as_tibble()

# Select interesting columns
videos_wrangled <- videos %>%
  transmute(id, title=snippet.title, published=snippet.publishedAt,
      creator=snippet.channelTitle, views=statistics.viewCount,
      likes=statistics.likeCount, dislikes=statistics.dislikeCount,
      comments=statistics.commentCount)

# Print (all) 10 lines
videos_wrangled %>% head(n=10)
