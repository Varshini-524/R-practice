library(httr) #webapi
library(jsonlite) #makes it json data
library(ggplot2) #plotting
library(dplyr)
# get the API key from here -> https://docs.airnowapi.org/account/request/
# set the API_KEY env variable from the command line so that we dont have
# store it in the code
API_KEY <- Sys.getenv("API_KEY")

airnow_api_host <- "http://www.airnowapi.org"
url <- paste0(airnow_api_host, "/aq/forecast/zipCode/?format=application/json&zipCode=20871&date=2018-07-01&distance=25&API_KEY=", API_KEY)
resp   <- GET(url)
print(status_code(resp))
resp_df   <- fromJSON(content(resp, "text"))
View(resp_df)

resp_df %>%
ggplot(aes(x = as.Date(DateForecast), y = AQI)) +
  geom_line(aes(col=ParameterName), size = 2) +
  geom_point() +
  theme_minimal()

#col is color