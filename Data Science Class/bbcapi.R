library(httr) #webapi
library(jsonlite) #makes it json data
library(ggplot2) #plotting
library(dplyr)
# get the API key from here -> https://docs.airnowapi.org/account/request/
# set the API_KEY env variable from the command line so that we dont have
# store it in the code
BBC_API_KEY <- Sys.getenv("BBC_API_KEY")

bbc_api_host <- "https://newsapi.org"
url <- paste0(bbc_api_host, "/v2/top-headlines?country=us&apiKey=", BBC_API_KEY)
resp1   <- GET(url)
print(status_code(resp))
resp_df1   <- fromJSON(content(resp1, "text"))
View(resp_df1)
articles = resp_df1$articles
source = articles$source
id = as.data.frame(source$id)

p <-ggplot(id,aes(source$id))
p +geom_bar(stat = "count") 


#col is color