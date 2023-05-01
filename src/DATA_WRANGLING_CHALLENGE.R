library(vroom)

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


