## DATABASE ##
library(RSQLite)
library(DBI)
library(dplyr)

con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "src/Chinook_Sqlite.sqlite")

#print(dbListTables(con))

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

## Functional Programming ##
library(purrr)

numbers <- c(1:3)
#numbers_list <- map(numbers, print)

bike_data_lst <- fromJSON("src/bike_data.json")
#print(bike_data_lst %>% purrr::pluck("productDetail", "variationAttributes", "values", 1, "displayValue"))

## CHALLENGE ##
library(ggplot2)

# Weather on Gran Canaria
weather_api <- function() {
  url <- modify_url(url = "https://api.open-meteo.com/v1/forecast?latitude=27.7564556&longitude=-15.5910148&current_weather=true&hourly=temperature_2m,relativehumidity_2m,windspeed_10m")
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

weather <- weather_api() %>% content()

weather_tbl <- tibble(time = weather[["hourly"]][["time"]],temp = weather[["hourly"]][["temperature_2m"]])
weather_df <- weather_tbl %>% data.frame
#print(weather_df) # Print weather as df
weather_df$time <- as.numeric(as.POSIXct(strptime(weather_df$time, "%Y-%m-%dT%H:%M"))) # Convert time to numberic
weather_df$temp <- as.numeric(weather_df$temp) # Convert temp to numberic

ggplot(weather_df, aes(time, temp)) +
  geom_point() +
  scale_y_continuous(limits = c(-10, 40)) # Ploting weather

# Radon-bikes mountainbike category
url <- "https://www.radon-bikes.de/e-bike/mountainbike/"

# Retrieve names from bikeTitle class
bike_names <- url %>% 
  read_html() %>% 
  html_nodes(".bikeTitle > h4") %>%
  html_text() 

# Retrieve prices from info class
bike_prices <- url %>% 
  read_html() %>% 
  html_nodes(".info > div > div > span") %>%
  html_text()
# Since there is a second price in pounds sterling we remove those and only look at the price in euros
bike_prices <- bike_prices[seq(1,length(bike_prices),2)]

# Transform into tibble and print
tibble(name = bike_names,price = bike_prices) %>% print()

# Since there are only 6 different bikes in the mountainbike category, only these are shown
