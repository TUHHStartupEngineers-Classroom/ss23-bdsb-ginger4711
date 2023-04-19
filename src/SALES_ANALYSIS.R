# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
# 2.0 Importing Files ----
path_root <- "src/ds_data/01_bike_sales/01_raw_data/"
bikes_tbl      <- read_excel(path = paste(path_root,"bikes.xlsx",sep=""))
orderlines_tbl <- read_excel(path = paste(path_root,"orderlines.xlsx",sep=""))
bikeshops_tbl  <- read_excel(path = paste(path_root,"bikeshops.xlsx",sep=""))
# 3.0 Examining Data ----
# glimpse(orderlines_tbl)
# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
# bike_orderlines_joined_tbl %>% glimpse()
# 5.0 Wrangling Data ----
bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "Mountain")) %>% 
  unique()
# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate

# Step 2 - Visualize


# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate

# Step 2 - Visualize



# 7.0 Writing Files ----

# 7.1 Excel ----

# 7.2 CSV ----

# 7.3 RDS ----
