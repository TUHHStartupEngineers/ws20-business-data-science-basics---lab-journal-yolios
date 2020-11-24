library(tidyverse)
library(rvest)
library(stringr)
library(httr)

home_url <- "https://www.rosebikes.de"

# On error print message and continue
on_error <- function(e) {
  message(e)
  NULL
}

# Get all information about the model / color combination
scrape_color <- function(color_path){
  message(paste("color:", color_path))
  
  link <- paste(home_url, color_path, sep = "")
  
  page <- read_html(link) %>%
    tryCatch(error = on_error)
  
  if (!is.null(page)){
    price <- page %>%
      html_nodes(css = ".buybox__prize__wrapper") %>%
      html_node(xpath = "span") %>%
      html_attr(name = "data-test") %>%
      str_extract("[0-9/.]+")
    
    color <- page %>%
      html_nodes(css = ".radiobutton--color.is-active") %>%
      html_attr(name = "title")
    
    model <- page %>%
      html_node(css = "h1.h2") %>%
      html_text()
    
    breadcrumbs <- page %>%
      html_node(css = ".catalog-breadcrumb__list") %>%
      html_nodes(xpath = "child::li/child::a/child::span") %>%
      html_text() %>%
      str_remove_all("\\n")
    
    category_1 <- breadcrumbs[3]
    category_2 <- breadcrumbs[4]
      
    variant <- page %>%
      html_nodes(css = ".buybox__variantfinder + *") %>%
      html_nodes(xpath = "button/span") %>%
      html_text() %>%
      str_extract("[^\\n]+")
    
    feature_keys <- page %>%
      html_node(css = "div#features") %>%
      html_nodes(xpath = "div/dl/dt") %>%
      html_text() %>%
      str_extract("[^:]+")
    
    feature_values <- page %>%
      html_node(css = "div#features") %>%
      html_nodes(xpath = "div/dl/dd") %>%
      html_text() %>%
      str_remove_all("\\n")
    
    features <- tibble(key=feature_keys, value=feature_values)
    
    wheels <- features[features$key == "Laufräder", "value"] %>%
      as.character()
    
    if (wheels == "character(0)") wheels <- ""
    
    switchgear <- features[features$key == "Schaltwerk", "value"] %>%
      as.character()
    
    if (switchgear == "character(0)") switchgear <- ""
    
    chain <- features[features$key == "Kette", "value"] %>%
      as.character()
    
    if (chain == "character(0)") chain <- ""
    
    brakes <- features[features$key == "Bremsen", "value"] %>%
      as.character()
    
    if (brakes == "character(0)") brakes <- ""
    
    tibble(category_1, category_2, model, variant, color, price, wheels,
        switchgear, chain, brakes, link)
  }
}

# Get the links to all colors of a model variant and visit them
scrape_variant <- function(variant_path) {
  message(paste("vraiant:", variant_path))
  page <- read_html(paste(home_url, variant_path, sep = "")) %>%
    tryCatch(error = on_error)
  
  if (!is.null(page)){
    page %>%
      html_nodes(css = ".radiobutton--color") %>%
      html_attr(name = "href") %>%
      map(scrape_color) %>%
      bind_rows()
  }
}

# Get the links to all variants of a model and visit them
scrape_model <- function(model_path) {
  message(paste("model:", model_path))
  page <- read_html(paste(home_url, model_path, sep = "")) %>%
    tryCatch(error = on_error)
  
  if (!is.null(page)){
    page %>%
      html_nodes(css = ".catalog-category-model__link") %>%
      html_attr(name = "href") %>%
      map(scrape_variant) %>%
      bind_rows()
  }
}

# Get the links to all models of a category and visit them
scrape_category <- function(category_path) {
  message(paste("category:", category_path))
  read_html(paste(home_url, category_path, sep = "")) %>%
    html_nodes(css = ".catalog-category-bikes__button") %>%
    html_attr(name = "href") %>%
    map(scrape_model) %>%
    bind_rows()
}

# Define starting path
start_path <- "/fahrräder"

# Get all category links and start scraping
result <- read_html(paste(home_url, start_path, sep = "")) %>%
  html_nodes(css = ".catalog-categories-item__link") %>%
  html_attr(name = "href") %>%
  as_tibble() %>%
  filter(!(str_detect(value, "sale") | str_detect(value, "zoovu"))) %>%
  .$value %>% as.list() %>%
  map(scrape_category) %>%
  bind_rows()

# Write results to file
result %>%
  write_rds("00_data/rose_bikes.rds")





