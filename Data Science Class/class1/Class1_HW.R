##Problem 1
#create a data frame
#7BABD56E-6206-4A63-BEA7-1CF43F01C1A7 
people = data.frame(name = c("person1","person2","person3"),age = c(10,20,30))

#print data frame
people
View(people)

library(knitr)
kable(people, col.names = c("Name","Age"))

#load purrr
library(purrr)

#use map2 
x <- as.vector(people[["name"]])
y <- people[["age"]]

sink <- map2(x,y,~cat("name is", .x, ", age is ", .y,'\n'))

sink <- map2(people$name, people$age,function(a,b) cat("name is", a, ", age is ", b,'\n') )

#print summary
summary(people)
glimpse(people)
library(psych)
describe(people)

##Problem 3
library(httr) #webapi
library(jsonlite) #makes it json data
library(ggplot2) #plotting
library(dplyr)
# get the API key from here -> https://docs.airnowapi.org/account/request/
# set the API_KEY env variable from the command line so that we dont have
# store it in the code
API_KEY <- Sys.getenv("API_KEY")

dates = c("2018-07-09","2018-07-10","2018-07-11","2018-07-12","2018-07-13","2018-07-14","2018-07-15")
get_data <- function(n){
  airnow_api_host <- "http://www.airnowapi.org"
  date = Sys.Date() - n
  url <- paste0(airnow_api_host, "/aq/forecast/zipCode/?format=application/json&zipCode=20871&date=",date,"&distance=25&API_KEY=", API_KEY)
  resp   <- GET(url)
  print(status_code(resp))
  resp_df   <- fromJSON(content(resp, "text"))
  #View(head(resp_df, n = 2))
  return (head(resp_df, n = 2))
}
x = map(c(0,1,2,3,4,5,6),get_data) %>%
  select(ParameterName) %>%
    reduce(rbind)

# total <- x[1][[1]]
# total$Category <-NULL
# for (i in seq(2:7)){
#   x[i][[1]]$Category <- NULL
#   total <- rbind(total, x[i][[1]])
# }



total %>%
  ggplot(aes(x = as.Date(DateForecast), y = AQI)) +
  geom_line(aes(col=ParameterName), size = 2) +
  geom_point() +
  theme_minimal()
