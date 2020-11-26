# LOAD LIBRARIES ----
library(vroom)
library(data.table)
library(tidyverse)


# LOAD DATA ----
#__Load uspc data ----
u_col_types <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_integer()
)

uspc_tbl <- vroom(
  file       = "00_data/patents/raw/uspc.tsv", 
  delim      = "\t", 
  col_names  = names(u_col_types),
  col_types  = u_col_types,
  skip       = 1,
  na         = c("", "NA", "NULL")
)

uspc_tbl <- uspc_tbl %>% select(c("patent_id", "mainclass_id", "sequence"))

setDT(uspc_tbl)


# __Load assignee data ----
a_col_types <- list(
  id = col_character(),
  type = col_integer(),
  name_first= col_character(),
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


# __Load patent_assignee data ----
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


# DATA WRANGLING ----
# __Remove duplicate assignments
patent_assignee_tbl <- unique(patent_assignee_tbl)


# __Remove all organizations != US company
assignee_tbl <- assignee_tbl[type == 2 & !is.na(organization),
    .(id, organization)]


# __Combine patent and assignee data ----
pa_comb_tbl <- merge(x=patent_assignee_tbl, y=assignee_tbl,
    by.x="assignee_id", by.y="id", all=F)[,.(patent_id, organization)]


# __Calculate number of patents for 10th company ----
min_num_patents = pa_comb_tbl[, .N, by=organization][order(-N)][10, N]


# __Add column with number of patents for that company ----
pa_comb_tbl[, num_patents := .N, by=organization]


# __Combine patent / assignee data with uspc data ----
combined_tbl <- merge(x=pa_comb_tbl, y=uspc_tbl, by="patent_id", all=F)


# DATA ALALYSIS ----
# __Top 5 classes, if only the class with sequence 0 is counted ----
combined_tbl[num_patents >= min_num_patents & sequence == 0, .N,
    by=mainclass_id][order(-N)][1:5] %>%
  write_rds("00_data/patents/analyzed/patent_innovation_0.rds")


# __Top 5 classes, if classes with any sequence number are counted ----
combined_tbl[num_patents >= min_num_patents, .N,
    by=mainclass_id][order(-N)][1:5] %>%
  write_rds("00_data/patents/analyzed/patent_innovation.rds")
