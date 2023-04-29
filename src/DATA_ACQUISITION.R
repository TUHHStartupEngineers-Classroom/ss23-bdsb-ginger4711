## DATABASE ##

library(RSQLite)
library(DBI)
library(dplyr)
con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "src/Chinook_Sqlite.sqlite")

print(dbListTables(con))

album_tbl <- tbl(con, "Album") %>% collect()

dbDisconnect(con)

## API ##
library(httr)
library(glue)
library(jsonlite)
library(keyring)
library(rstudioapi)

#resp <- GET(url = "https://swapi.dev/api/people/1/")

# Wrapped into a function
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("/people/1") # Retrieve info about Luke Skywalker

content <- rawToChar(resp$content) %>% fromJSON() # Turn content into characters and convert json to list

alphavantage_api_url <- "https://www.alphavantage.co/query"
ticker               <- "WDI.DE"
# You can pass all query parameters as a list to the query argument of GET()
WDIQuote = GET(alphavantage_api_url, query = list('function' = "GLOBAL_QUOTE",
                                       symbol     = ticker,
                                       apikey     = Sys.getenv('token'))
) %>% content()

## WEB SCRAPING ##

library(rvest)
library(stringr)

url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

sp_500 <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # Get the nodes with the id
  html_nodes(css = "#constituents") %>%
  # html_nodes(xpath = "//*[@id='constituents']"") %>% 
  # Extract the table and turn the list into a tibble
  html_table() %>% 
  .[[1]] %>% 
  as_tibble()

url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- url %>% 
  read_html()

# People that worked on "The Dark Knight"
people <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  .[[3]] %>% 
  html_attr("title")
