library(RSQLite)
library(DBI)
library(dplyr)
con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "src/Chinook_Sqlite.sqlite")

print(dbListTables(con))

album_tbl <- tbl(con, "Album") %>% collect()

dbDisconnect(con)
