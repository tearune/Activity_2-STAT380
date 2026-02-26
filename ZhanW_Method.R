---
title: "Predictive Modeling for Health Insurance Charges"
author: 
  - "Zhan Xin Wang"
- "Ankita"
format: 
  pdf:
  toc: true
number-sections: true
highlight-style: github
execute:
  echo: false
warning: false
message: false
---

  #| label: setup-modeling
#| include: false

set.seed(2004) 

# Load necessary libraries (Ensure these are installed first!)
library(tidyverse)
library(glmnet)  

install.packages("patchwork") 
install.packages("glmnet")

library(tidyverse)
library(patchwork)

data <- read_csv("~/Downloads/insurance_data_S26.csv") %>% 
  drop_na(charges, priorclaims)

p1 <- ggplot(data, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot() +
  labs(title = "Charges by Smoker Status", y = "Charges ($)") +
  theme_minimal()

p2 <- ggplot(data, aes(x = age, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Age vs Charges", x = "Age", y = "Charges ($)") +
  theme_minimal()

p1 / p2

#Ridge Regression model
library(glmnet)

x <- model.matrix(charges ~ age + sex + bmi + children + smoker + region + priorclaims, data)[,-1]
y <- data$charges

cv_model <- cv.glmnet(x, y, alpha = 0) # alpha=0 for Ridge
best_lambda <- cv_model$lambda.min

final_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)

library(broom)
coef_table <- tidy(final_model) %>% 
  select(term, estimate) %>%
  rename(Predictor = term, Coefficient = estimate)

knitr::kable(coef_table, digits = 3, caption = "Ridge Regression Coefficients")

