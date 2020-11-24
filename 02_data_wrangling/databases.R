library(tidyverse)
library(RSQLite)

# Connecting to Database
con <- dbConnect(drv = SQLite(), 
    dbname = "00_data/02_chinook/Chinook_Sqlite.sqlite")

# Retrieving Tables
album_tbl <- tbl(con, "Album") %>% collect()
artist_tbl <- dbGetQuery(con, 'SELECT * FROM Artist')

# Disconnecting from Database
dbDisconnect(con)
