# Load Libraries ----
library(vroom)
library(data.table)
library(tidyverse)


# LOAD DATA ----
# __Load assignee ----
a_col_types <- list(
  id = col_character(),
  type = col_integer(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "00_data/patents/raw/assignee.tsv", 
  delim      = "\t", 
  col_names  = names(a_col_types),
  col_types  = a_col_types,
  skip       = 1,
  na         = c("", "NA", "NULL")
)

assignee_tbl <- assignee_tbl %>% select(c("id", "type", "organization"))

setDT(assignee_tbl)


# __Load patent_assignee ----
pa_col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "00_data/patents/raw/patent_assignee.tsv", 
  delim      = "\t", 
  col_names  = names(pa_col_types),
  col_types  = pa_col_types,
  skip       = 1,
  na         = c("", "NA", "NULL")
)

patent_assignee_tbl <- patent_assignee_tbl %>%
  select(c("patent_id", "assignee_id"))

setDT(patent_assignee_tbl)


# WRANGLE DATA ----
# __Remove duplicate assignments ----
patent_assignee_tbl <- unique(patent_assignee_tbl)


# __Remove organizations != US company
assignee_tbl <- assignee_tbl[type == 2 & !is.na(organization),
    .(id, organization)]


# __Merge tables ----
combined_tbl <- merge(x=patent_assignee_tbl, y=assignee_tbl,
    by.x="assignee_id", by.y="id", all=F)[, .(patent_id, organization)]


# ALALYZE DATA ----
# __Sort organizations by number of patents ----
combined_tbl[!is.na(organization), .N, by=organization][order(-N)][1:10] %>%
  write_rds("00_data/patents/analyzed/patent_dominance.rds")
