library(tidyverse)
library(Hmisc)
library(modelr)
library(caret)

insurance_data <- read.csv("insurance_data_S26.csv")
View(insurance_data)

#Cross Validation (80-20 split)

set.seed(550)
train_index <- sample(
  x = 1:nrow(insurance_data),
  size = floor(0.8*nrow(insurance_data))
)

insuranceTrain <- insurance_data[train_index, ]
insuranceValid <- insurance_data[-train_index, ]

insuranceTrainModel <- 