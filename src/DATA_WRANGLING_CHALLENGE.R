library(tidyverse)
library(data.table)
library(dplyr)
library(vroom)

## Data Loading ##
# Using the reduced data set

# loading patent.tsv file
col_types_patent <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)
patent_tbl <- vroom(
  file       = "src/Patent_data_reduced/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)

# loading assignee.tsv file
col_types_assignee <- list(
  id = col_character(),
  type = col_integer(),
  organization = col_character()
)
assignee_tbl <- vroom(
  file       = "src/Patent_data_reduced/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)
assignee_tbl <- rename(assignee_tbl,assignee_id = id) # Rename id to assignee_id to make merging easier

# loading patent_assignee.tsv file
col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)
patent_assignee_tbl <- vroom(
  file       = "src/Patent_data_reduced/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)

# loading uspc.tsv file
col_types_uspc <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_integer()
)
uspc_tbl <- vroom(
  file       = "src/Patent_data_reduced/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)

## Patent Dominance ##

# Convert patent_assignee_tbl to data.frame
patent_dominance_dt <- as.data.table(patent_assignee_tbl)
# Group by assignee_id and create sum "patents" over those with same assignee_id. Order by patents to take the first 10 elements.
patent_dominance_dt <- patent_dominance_dt[,.(patents = .N),by = assignee_id][order(-patents),head(.SD, 10)]
# Merge with assignee_tbl by assignee_id, then order by descending patents again and only keep patents and organization column and then print.
patent_dominance_dt <- as.data.table(merge(as.tibble(patent_dominance_dt),assignee_tbl,by="assignee_id"))[order(-patents),patents,organization] %>% print()
