library(tidyverse)
library(rvest)
library(stringr)
library(httr)
library(rlist)

divs <- read_html("https://www.canyon.com/en-de/road-bikes/endurance-bikes/endurace/") %>%
  html_nodes(css = ".productGrid__listItem")

name <- divs %>%
  html_nodes(css = ".productTile__productName") %>%
  html_text() %>%
  str_extract("[A-Z].*?(?=\\n)")

price <- divs %>%
  html_nodes(css = ".productTile__priceSale") %>%
  html_text() %>%
  str_extract("[0-9].*?(?= €\\n)")
  
colors = vector("list", length(divs))
color_links = vector("list", length(divs))

for (i in 1:length(divs)) {
  colors[[i]] <- divs[[i]] %>%
    html_nodes(css = ".colorPicker__colorListItem") %>%
    html_node(xpath = "button") %>%
    html_attr(name="data-displayvalue")
  
  color_links[[i]] <- divs[[i]] %>%
    html_nodes(css = ".colorPicker__colorListItem") %>%
    html_node(xpath = "button") %>%
    html_attr(name="data-pdp-url")
}

