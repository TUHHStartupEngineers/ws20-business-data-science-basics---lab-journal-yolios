library(tidyverse)
library(rvest)
library(stringr)
library(httr)
library(rlist)

bike_page <- read_html("https://www.canyon.com/en-de/road-bikes/endurance-bikes/endurace/endurace-wmn-al-disc-7.0/2386.html?dwvar_2386_pv_rahmenfarbe=SR%2FWH")

price <- bike_page %>%
  html_node(css=".productDescription__priceSale") %>%
  html_text() %>%
  str_extract("[0-9].*?(?= €\\n)")

