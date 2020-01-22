#Linear Model - Converting C to F
#1
c = runif(100,0,100)
table(as.integer(c))
library(purrr)
#2
f = 1.8*c + 32 + rnorm(100,0,5)
plot(x=c,y=f,xlab="Celcius",ylab = "Fahrenheit",main="Temperature Measurement")
#3
fit = lm(f ~ c)
summary(fit)
slope = coefficients(fit)[2]
intercept = coefficients(fit)[1]

lines(x=c,y=slope*c+intercept,col="blue")
mtext("predicted line")

#Another Classification Problem
#1
a = rnorm(200,4,4)
b = rnorm(200,-4,4)
df = data.frame(x = c(a,b),y = as.factor(c(rep(1,200),rep(0,200))))
plot(df$x,col=ifelse(df$y == 1,'red','green'))
#2
library(GGally)
ggscatmat(df, columns = 1:ncol(df),color=ifelse(df$y == 1,'red','green'))
library(glmnet) #generalized linear model
fit <- glm(y~x,data=df,family=binomial())
summary(fit) # display results
confint(fit) # 95% CI for the coefficients
fit
probs <- predict(fit, type="response") # predicted values
gender_predicted <- ifelse(probs > 0.5, 1, 0)
accuracy = mean(gender_predicted == df$y)
accuracy #class acuracy can be impt
table(gender_predicted, df$y)


# train test split
# Split test/training sets
set.seed(100)
require(rsample)
train_test_split <- initial_split(df, prop = 0.8)
train_test_split #<80% / 20% / 100%>

# Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split)

fit <- glm(y~x,data=df,family=binomial())
summary(fit) # display results
confint(fit) # 95% CI for the coefficients
fit

probs <- predict(fit, type="response") # predicted values
gender_predicted <- ifelse(probs > 0.5, 1, 0)
accuracy = mean(gender_predicted == df$y)
accuracy #class acuracy can be impt
table(gender_predicted, df$y)

probs <- predict(fit, test_tbl, type="response") # predicted values
gender_predicted <- ifelse(probs > 0.5, 1, 0)
accuracy = mean(gender_predicted == test_tbl$y)
accuracy
table(gender_predicted, test_tbl$y)


# load neural network packages
install.packages(c("nnet", "NeuralNetTools"))
library(nnet)
library(NeuralNetTools)
library(dplyr)

# set a random seed to get repeatable results
set.seed(1234)

linear_model <- function(df_train, df_test) {
  # create a linear model, we want to model the target variable "t"
  # using 3 predictors x, y and z
  lm_fit <-lm(t ~ x+y+z, data=df_train)
  summary(lm_fit)
  
  # we have the model now, it is a linear model so we write it as an 
  # a linear equation in 3 variables x, y and z (which were used to create this model)
  cat(sprintf('Linear model is target = %0.4f + (%0.4fx) + (%0.4fy) + (%0.4fz))\n',
              lm_fit$coefficients[1], lm_fit$coefficients[2], 
              lm_fit$coefficients[3], lm_fit$coefficients[4])) 
  
  # lets  looking at the trainin SSE (this is what we minimized to get the parameters of the model)
  train_SSE = sum(lm_fit$residuals^2)
  cat(sprintf('Training Sum of Squared Errors (SSE) 
              for linear model is %0.4f\n', train_SSE))
  
  # now lets predict on the "unseen" a.k.a test data
  predictions = predict(lm_fit, df_test)
  
  # how well did the model perform, lets see test SSE vs training SSE
  test_SSE = sum((predictions - df_test$t)^2)
  rmse = sqrt(mean((predictions - df_test$t)^2))
  cat(sprintf('Test Sum of Squared Errors (SSE) for linear model is %0.4f,
              training SSE was %.4f\n', test_SSE, train_SSE))
  cat(sprintf('Root mean squared error %.4f', rmse))
}

nn_model <- function(df_train, df_test, size=0) {
  # now lets try a neural network with no hidden layer (size = 0)
  # this is essentialy same as the linear model as we shall see 
  # when look at the params of the model and the diagramatic representation of the
  # neural network
  nn_fit <- nnet(t ~ x+y+z,
                 size = size, data=df_train, skip = T,
                 trace = F,maxit = 1000, linout = T)
  summary(nn_fit)
  
  cat(sprintf('NN model is target = %0.4f + (%0.4fx) + (%0.4fy) + (%0.4fz))\n',
              nn_fit$wts[1], nn_fit$wts[2], nn_fit$wts[3], nn_fit$wts[4]))
  train_SSE = sum(nn_fit$residuals^2)
  
  cat(sprintf('Training SSE for nnet model is %0.4f\n', train_SSE))
  
  predictions = predict(nn_fit, df_test, type="r")
  test_SSE = sum((predictions - df_test$t)^2)
  rmse = sqrt(mean((predictions - df_test$t)^2))
  cat(sprintf('Test Sum of Squared Errors (SSE) for neural net model is 
              %0.4f\n, training SSE was %.4f\n', test_SSE, train_SSE))
  cat(sprintf('Root mean squared error %.4f', rmse))
  
  plotnet(nn_fit)
}

# read the data
df = read.csv("data.csv")
sample_n(df, 10)
glimpse(df)

# create a test train split, 80/20 split being done here
n = nrow(df)
train = sample(n, 0.8*n)
df_train = df[train,]
df_test  = df[-train,]

linear_model(df_train, df_test)
nn_model(df_train, df_test, 0)
nn_model(df_train, df_test, 3)


##setwd("")


