library(tidyverse)
library(Hmisc)
library(modelr)
library(caret)
library(leaps)

#Load the Boston Neighborhood data
getHdata(boston)

Boston <- boston %>%
  mutate( #Creating a transformation of distance that's relationship value looks more linear
    log_distance = log(distance)
  )

#Wrap Up Cross-Validation----
##Manually Writing Cross-Validation ----
#Randomly permute the fold values
set.seed(3680)
folds <- sample(folds) #randomly permute the fold values

#Assign fold labels to Boston data
Boston$fold_ID <- folds

##Write the loop
#Initialize place to store MSEs
MSEs_CV <- c()

###Find the 10-fold CV MSE ----
for (i in 1:num_folds) {
  BostonTrainCV <- Boston[Boston$fold_ID != i, ] #All but ith fold for training
  BostonTestCV <- Boston[Boston$fold_ID == i, ] #ith fold for validation
  CVModel <- lm(
    formula = value ~ rooms + river + log_distance,
    data = BostonTrainCV
  )
  MSEs_CV[i] <- modelr::mse(
    model = CVModel,
    data = BostonTestCV
  )
}

#Find the mean MSE value
CV_MSE_est <- mean(MSEs_CV)
#Find the RMSE value
CV_RMSE_est <- sqrt(CV_MSE_est)


## Performing k-fold with k = n ----
#Initialize place to store MSEs
MSEs_CV <- c()

###Find the 10-fold CV MSE ----
for (i in 1:nrow(Boston)) {
  BostonTrainCV <- Boston[-i, ] #All but ith observation for training
  BostonTestCV <- Boston[i, ] #ith observation for validation
  CVModel <- lm(
    formula = value ~ rooms + river + log_distance,
    data = BostonTrainCV
  )
  MSEs_CV[i] <- modelr::mse(
    model = CVModel,
    data = BostonTestCV
  )
}

#Find the mean MSE value
CV_MSE_est <- mean(MSEs_CV)
#Find the RMSE value
CV_RMSE_est <- sqrt(CV_MSE_est)


##caret package for CV ----
library(caret)
#Specify CV features
CV_features <- trainControl(
  method = "cv", #using cross-validation
  number = 10 #specify k
)
#Fitting model using k-fold CV
CVModel_caret <- train(
  value ~ rooms + river + log_distance, #Specify formula
  data = Boston,
  method = "lm", #Model type is linear regression
  trControl = CV_features
)
#Print RMSE
CVModel_caret$results$RMSE
#Print  MSE
CVModel_caret$results$RMSE^2





#Model Selection----
##Best Subsets Selection----
#We are not going to run best subsets on the boston data as a whole. 
#There are so many variables to consider that it is too computationally taxing.
#To model this, we're going to try this on a subset of variables
Best_Subsets_Boston <- regsubsets(
  value ~ latitude + longitude + crime + river + nox + rooms + distance + lstat + black,
  data = boston,
  nvmax = 9 #determines the max number of variables to allow in a model
)
#We need to save the summary output so we can access the metrics for each model
summary_BSB <- summary(Best_Subsets_Boston)

#The which object in our summary is a matrix of the variables included in each of the models built
summary_BSB$which
#We can also look at the model metrics for each of these models
summary_BSB$bic

##Backward Selection----
#start by building a model with all variables in it.
Full_Boston_Model <- lm(
  value ~ . - tract -cmedv, 
  data = boston)
#In the above code "value ~ ." tells R to build a model for value 
#and the "~ ." tells R to use all other variables in the dataset. 
#However, we cannot use tract for predicting value because it is a unique ID so we remove it by using "- tract".
#We also do not want to use cmedv because it is almost exactly the value

#Implement backward elimination
stats::step(object = Full_Boston_Model,
            data = boston,
            direction = "backward",
            k = 2, #log(nrow(boston)) for BIC
            trace = 1)

##Forward Selection ----
Null_Boston_Model <- lm(
  value ~ 1, #Tells the model to include only an intercept
  data = boston)

#Perform forward selection
stats::step(object = Null_Boston_Model,
            scope = list(lower = Null_Boston_Model, upper = Full_Boston_Model),
            data = boston,
            direction = "forward")
#The scope argument tells R the range of models to be considered. 
#It is necessary so R can figure out which variables might be included in the model.
