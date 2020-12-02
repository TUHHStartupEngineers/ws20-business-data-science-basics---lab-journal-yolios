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

assignee_tbl <- assignee_tbl %>%
  select(c("id", "type", "organization"))

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


# __Load patent ----
p_col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "00_data/patents/raw/patent.tsv", 
  delim      = "\t",
  col_names  = names(p_col_types),
  col_types  = p_col_types,
  skip       = 1,
  na         = c("", "NA", "NULL")
)

patent_tbl <- patent_tbl %>% select(c("id", "date"))

setDT(patent_tbl)


# WRANGLE DATA ----
# __Remove all assignees != US Company
assignee_tbl <- assignee_tbl[type==2 & !is.na(organization),
    .(id, organization)]


# __Merge tables ----
combined_tbl <- merge(x=patent_tbl, y=patent_assignee_tbl,
    by.x="id", by.y="patent_id", all.x=T, all.y=F)

combined_tbl <- merge(x=combined_tbl, y=assignee_tbl,
    by.x="assignee_id", by.y="id", all.x=T, all.y=F)


# __Remove all patents with NA organization ----
combined_tbl <- combined_tbl[!is.na(organization), .(id, date, organization)]


# ALALYZE DATA ----
# __Select companies with most patents granted in 2019 ----
combined_tbl[lubridate::year(date) == 2019][,
    .N, by=organization][order(-N)][1:10] %>%
  write_rds("00_data/patents/analyzed/patent_activity.rds")
