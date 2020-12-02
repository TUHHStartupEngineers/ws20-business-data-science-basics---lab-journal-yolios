library(tidyverse)
library(glue)
library(httr)
library(jsonlite)

resp <- GET("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey=demo")
stop_for_status(resp) # automatically throws an error if a request did not succeed

wc_stock <- rawToChar(resp$content) %>% fromJSON()
