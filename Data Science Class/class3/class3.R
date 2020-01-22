library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(tidyverse)

# read the dataset
youth = read_csv("/Users/VarshiniSelvadurai/Documents/Data Science Class/class3/youth.csv")

# lets see what it constains
glimpse(youth)

# another view
View(youth)

# lets bin the data and plot a bar chart
# this is a dscrete version of the distribution
summary(youth$`Height (inches)`)
NUM_BINS <- 100
youth$height_binned <- cut(youth$`Height (inches)`, breaks = NUM_BINS)
View(youth)

youth %>%
  ggplot(aes(x = height_binned))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()

# plot a histogram of the height field
youth %>%
  ggplot(aes(x=`Height (inches)`)) + 
  geom_histogram(aes(fill=I("white"), col=I("black"))) + 
  theme_minimal()

# now a density plot
# this is a continous version of the distribution
youth %>%
  ggplot(aes(`Height (inches)`)) +
  geom_density(alpha = 0.1, fill = I("blue")) + 
  theme_minimal()

hist(youth$`Height (inches)`, breaks=30)
plot(density(youth$`Height (inches)`))

#HW 3.Gender
youth %>%
  ggplot(aes(`Height (inches)`)) +
  geom_density(alpha = 0.1, aes(fill = Gender)) + 
  theme_minimal()

#HW 3.Expected Value
height = youth$`Height (inches)`
x = height[sample(length(height),100)] #test
n = length(height)
pop_mean = mean(height)

test <- function(num){
  mean_list = unlist(map(seq(1:num), ~mean(height[sample(n,100)])))
  
  plot(density(mean_list)) #majority near 67
  return (mean(mean_list)) #67.035 which is reasonable
}
m = test(100)
print(m)
m = test(1000)
print(m)
m = test(10000)
print(m)
m = test(100000)
print(m)
m = test(1000000) #don't run this
print(m)


#Class 5
# create a glm model
df = youth %>%
  rename(gender = Gender, age=Age, height=`Height (inches)`, weight=`Weight (lbs)`) %>%
  select(gender, age, height, weight)

library(GGally)
ggscatmat(df, columns = 2:4, color="gender", alpha=0.8) +
  ggtitle("Correlation in various elements of the youth dataset") + 
  theme(axis.text.x = element_text(angle=-40, vjust=1, hjust=0, size=10))


library(glmnet) #generalized linear model
fit <- glm(as.factor(gender)~age+height+weight+how,data=df,family=binomial())
summary(fit) # display results
confint(fit) # 95% CI for the coefficients
fit
probs <- predict(fit, type="response") # predicted values
gender_predicted <- ifelse(probs > 0.5, "Male", "Female")
accuracy = mean(gender_predicted == df$gender)
accuracy #class acuracy can be impt
table(gender_predicted, df$gender)


# train test split
# Split test/training sets
set.seed(100)
require(rsample)
train_test_split <- initial_split(df, prop = 0.8)
train_test_split #<80% / 20% / 100%>

# Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split)

fit <- glm(as.factor(gender) ~ age+height+weight, data = train_tbl, family=binomial())
fit
summary(fit) # display results
confint(fit) # 95% CI for the coefficients

probs <- predict(fit, type="response") # predicted values
gender_predicted <- ifelse(probs > 0.5, "Male", "Female")
accuracy = mean(gender_predicted == train_tbl$gender)
accuracy
table(gender_predicted, train_tbl$gender)

probs <- predict(fit, test_tbl, type="response") # predicted values
gender_predicted <- ifelse(probs > 0.5, "Male", "Female")
accuracy = mean(gender_predicted == test_tbl$gender)
accuracy
table(gender_predicted, test_tbl$gender)

# randomforest
library(randomForest)
youth.rf=randomForest(as.factor(gender) ~ . , data = train_tbl, 
                      importance=TRUE, 
                      ntree=2000)
# "as.factor(gender) ~ ." means all columns except gender
youth.rf

plot(youth.rf)

varImpPlot(youth.rf)

prediction <- predict(youth.rf, test_tbl)
mean(prediction == test_tbl$gender)

# add another feature
df = youth %>%
  rename(gender = Gender, age=Age, height=`Height (inches)`, weight=`Weight (lbs)`,
       describe_wt = `How would you describe your weight?`) %>%
  select(gender, age, height, weight, describe_wt)
View(df)


df <- df %>%
  filter(!is.na(describe_wt)) %>%
  mutate(describe_wt = as.numeric(as.factor(describe_wt)))


train_test_split <- initial_split(df, prop = 0.8)
train_test_split

# Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split)

youth.rf=randomForest(as.factor(gender) ~ . , data = train_tbl, 
                      importance=TRUE, 
                      ntree=2000)
youth.rf

plot(youth.rf)

varImpPlot(youth.rf)

prediction <- predict(youth.rf, test_tbl)
mean(prediction == test_tbl$gender)




