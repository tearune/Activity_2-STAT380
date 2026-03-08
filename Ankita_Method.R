library(tidyverse)
library(Hmisc)
library(modelr)
library(caret)

insurance_data <- read.csv("insurance_data_S26.csv")
View(insurance_data)

# Cross Validation (80-20 split) - 

set.seed(550)
train_index <- sample(
  x = 1:nrow(insurance_data),
  size = floor(0.8*nrow(insurance_data))
)

insuranceTrain <- insurance_data[train_index, ]
insuranceValid <- insurance_data[-train_index, ]

insuranceTrainModel <- lm(
  formula = charges ~ .,
  data = insuranceTrain
)

insuranceValid <- insuranceValid %>%
  mutate(
    model1_predict = predict(
      object = insuranceTrainModel,
      newdata = insuranceValid
    )
  )
  
summary(insuranceTrainModel)


#FORWARD SELECTION (AIC)
null_insurance <- lm(
  charges ~ 1,
  data = insurance_data
)

full_insurance <-lm(
  charges ~ .,
  data = insurance_data
)

stats::step(
  object = null_insurance,
  scope = list(lower = null_insurance, upper = full_insurance),
  data = insurance_data,
  direction = 'forward'
)

FORWARD_AIC <- lm(charges~smoker+age+bmi+children+priorclaims, data = insurance_data)
summary(FORWARD_AIC) 
plot(FORWARD_AIC)


#thoughts?
#' remove prior claims, moth and region <- not significant
#' check for assumptions - distribution is not normal heavily right skewed, otherwise is linear, 
#' constant error variance and no outliers.
#' r^2 adjusted = 0.7239
#' --- once removing region, month becomes not significant under 5% rule

#BACKWARD BIC
stats::step(
  object = full_insurance,
  data = insurance_data,
  direction = "backward",
  k = log(nrow(insurance_data)),
  trace = 1
)

BACKWARDS_BIC <- lm(charges~age+bmi+children+smoker, data = insurance_data)
summary(BACKWARDS_BIC)
plot(BACKWARDS_BIC)
#'r^2 = 0.72, adj-r^2 = 0.72
#' all predictors are significant
#' check assumptions
