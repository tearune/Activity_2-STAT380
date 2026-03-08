library(tidyverse)
library(glmnet)
install.packages("caret")
library(caret)


insurance_data <- read.csv("insurance_data_S26.csv")
View(insurance_data)

# train and validation split
set.seed(550)

train_index <- sample(
  x = 1:nrow(insurance_data),
  size = floor(0.8 * nrow(insurance_data))
)

insuranceTrain <- insurance_data[train_index, ]
insuranceValid <- insurance_data[-train_index, ]

insuranceTrain <- na.omit(insuranceTrain)
insuranceValid <- na.omit(insuranceValid)


# LASSO method
x_train <- model.matrix(charges ~ ., insuranceTrain)[,-1]
y_train <- insuranceTrain$charges

x_valid <- model.matrix(charges ~ ., insuranceValid)[,-1]
y_valid <- insuranceValid$charges


# running lasso
set.seed(550)

lasso_cv <- cv.glmnet(
  x_train,
  y_train,
  alpha = 1
)

plot(lasso_cv)


# fitting
LASSO_MODEL <- glmnet(
  x_train,
  y_train,
  alpha = 1,
  lambda = lasso_cv$lambda.min
)

coef(LASSO_MODEL)

# data prediction
lasso_predictions <- predict(
  LASSO_MODEL,
  newx = x_valid,
  s = lasso_cv$lambda.min
)


# RMSE
lasso_predictions <- as.vector(lasso_predictions)

length(lasso_predictions)
length(y_valid)

lasso_rmse <- sqrt(mean((y_valid - lasso_predictions)^2, na.rm = TRUE))

lasso_rmse

# check if numbers are the same
length(y_valid)
length(lasso_predictions)

