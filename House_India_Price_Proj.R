library(readxl)
HPI <- read_excel("House Price India.xlsx")
View(HPI)

library(tidyverse)
library(caret)

## view data distribtuion
hist(HPI$Price,
     prob = TRUE,
     main = "Histogram with density line",
     xlab = "Price",
     ylab = "Frequency",
     col = "lightblue",
     border = "blue",
     breaks =  45)

# Simple ML pipeline
# 0. prep data/clean data
# 1. split data
# 2. train model
# 3. score model aka. prediction
# 4. evaluate model

# 0. prep data
HPI <- HPI %>%
  filter(Price <= 0) # remove row where Price = 0

log_price <- log(HPI$Price)

HPI$log_price <- log(HPI$Price)
data_training <- HPI %>%
  select(`number of bedrooms`,
         `grade of the house`,
         `log_price`,
         `Built Year`)

HPI %>%
  complete.cases() %>%
  mean()


# 1. split data
split_HPI <- function(HPI) {
  set.seed(42)
  n <- nrow(HPI)
  train_id <- sample(1:n, size = 0.8*n)
  train_df <- HPI[train_id, ]
  test_df <- HPI[- train_id, ]
  return( list(training = train_df,
               testing = test_df) )
}

prep_data <- split_HPI(HPI) # split data into 2 set; train & test
n <- nrow(df)
train_df <- prep_data[[1]] # choose train data
test_df <- prep_data[[2]] # choose test data


# 2. train model
set.seed(42)
lm_model <- train(log_price ~ . ,
                  data = train_df,
                  method = "lm")

lm_model  

# 3. predict data
prediction <- predict(lm_model, newdata = test_df)


## 4.evaluate
test_df$log_price # Actual values
## 4.1 mae (mean absolute error)
mae <- mean(abs(prediction - test_df$log_price))
## 4.2 mse (mean squared error)
rmse <- sqrt(mean((prediction - test_df$log_price) ^2))

varImp(lm_model)
