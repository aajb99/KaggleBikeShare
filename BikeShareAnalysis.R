library(tidyverse)
#install.packages('tidymodels')
#install.packages('tidyverse')
library(tidymodels)
library(vroom)
install.packages('DataExplorer')
library(patchwork)

data1 <- vroom("train.csv") # grab training data

view(data1)

# Data Clean: 

data1$weather[data1$weather == 4] <- 3 # change value of 4 for Weather to 3
  
# unique(data1$weather)

#str(data1)

factor_cols <- c(2:5)

# data1 <- data1 %>%
#   mutate(across(factor_cols, as.factor)) # convert factor variables

# str(data1)

data1 <- data1 %>%
  select(-registered, -casual) # drop registered and casual

view(data1)

################################################################################

rFormula <- count ~ .

my_recipe <- recipe(rFormula, data = data1) %>% # set model formula and dataset
  step_mutate(across(factor_cols, as.factor)) %>%
  step_time(datetime, features = 'hour') %>% # get hours
  step_dummy(all_nominal_predictors()) # get dummy variables

prepped_recipe <- prep(my_recipe) # preprocessing new data
baked_data1 <- bake(prepped_recipe, new_data = data1)

# factor_cols2 <- c(8:14)
# baked_data1 <- baked_data1 %>%
#   mutate(across(factor_cols2, as.factor)) # Convert dummy variables into factors

view(baked_data1[1:10,])


################################################################################
# Linear Regression

my_mod <- linear_reg() %>% # Type of Model
  set_engine("lm") # R function to use

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data = data1) # Fit the workflow

data_test <- vroom("test.csv") # grab testing data

data_test$weather <- data_test$weather[data_test$weather == 4] <- 3
  

bike_predictions <- predict(bike_workflow,
                            new_data = data_test)

bike_predictions <- bike_predictions %>%
  mutate(.pred = ifelse(bike_predictions$.pred < 0, 0, bike_predictions$.pred))


bike_predictions <- cbind(data_test$datetime, bike_predictions)

colnames(bike_predictions)[1] = "datetime"
colnames(bike_predictions)[2] = "count"

bike_predictions$datetime <- as.character(format(bike_predictions$datetime))

#vroom_write(bike_predictions, "bike_predictions.csv", delim = ",")


################################################################################
# Poisson Regression

install.packages("poissonreg") # install necessary packages
library(poissonreg)

# Improved Recipe
data1 <- data1 %>%
  select(-casual, - registered) # drop casual and registered variables

bike_recipe <- recipe(count ~ ., data=data1) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Change weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>% # change weather as factor INSIDE RECIPE
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>% # convert season to factor with levels
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>% # convert holiday to factor
  step_time(datetime, features="hour") %>% # this hourly variable will replace datetime
  step_rm(datetime)

prepped_recipe <- prep(bike_recipe) # preprocessing new data
baked_data1 <- bake(prepped_recipe, new_data = data1)

pois_mod <- poisson_reg() %>% #Type of model3
  set_engine("glm") # GLM = generalized linear model

data_test <- vroom("test.csv") # input test data

bike_pois_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(pois_mod) %>%
  fit(data = data1) # Fit the workflow

bike_predictions <- predict(bike_pois_workflow,
                            new_data=data_test) # Use fit to predict

# Set up for Kaggle:
bike_predictions <- cbind(data_test$datetime, bike_predictions)

colnames(bike_predictions)[1] = "datetime"
colnames(bike_predictions)[2] = "count"

bike_predictions$datetime <- as.character(format(bike_predictions$datetime))

vroom_write(bike_predictions, "bike_predictions_pr.csv", delim = ",")


################################################################################
# Penalized Regression:

install.packages("poissonreg") # install necessary packages
library(poissonreg)
install.packages("glmnet")
library(glmnet)

data1 <- vroom("train.csv") # grab training data

# view(data1)

data1 <- data1 %>%
  select(-casual, - registered) # drop casual and registered variables

log_train_set <- data1 %>%
  mutate(count=log(count))

bike_recipe <- recipe(count ~ ., data=log_train_set) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Change weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>% # change weather as factor INSIDE RECIPE
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>% # convert season to factor with levels
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>% # convert holiday to factor
  step_time(datetime, features="hour") %>% # this hourly variable will replace datetime
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>% #make dummy variables7
  step_normalize(all_numeric_predictors()) # Make mean 0, sd=1

prepped_recipe <- prep(bike_recipe) # preprocessing new data
baked_data1 <- bake(prepped_recipe, new_data = log_train_set)

preg_model <- linear_reg(penalty=0.001, mixture=0) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

data_test <- vroom("test.csv") # input test data

bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) %>%
  fit(data = log_train_set) # Fit the workflow

# Get Predictions and Set up for Kaggle:
log_lin_preds <- predict(bike_workflow, new_data = data_test) %>% #This predicts log(count)
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., data_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

vroom_write(log_lin_preds, "bike_predictions_penreg.csv", delim = ",")


################################################################################
# Cross Validation

library(tidyverse)
library(tidymodels)
install.packages("glmnet")
library(glmnet)
library(vroom)

data_train <- vroom("train.csv") # grab training data
# view(data1)

data_train <- data_train %>%
  select(-casual, - registered) # drop casual and registered variables

log_train_set <- data_train %>%
  mutate(count=log(count))

bike_recipe <- recipe(count ~ ., data=log_train_set) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Change weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>% # change weather as factor INSIDE RECIPE
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>% # convert season to factor with levels
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>% # convert holiday to factor
  step_time(datetime, features="hour") %>% # this hourly variable will replace datetime
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>% #make dummy variables7
  step_normalize(all_numeric_predictors()) # Make mean 0, sd=1

prepped_recipe <- prep(bike_recipe) # preprocessing new data
baked_data1 <- bake(prepped_recipe, new_data = log_train_set)

## Penalized regression model
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

## Set Workflow
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model)

## Grid of values to tune over
tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 5) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(data_train, v = 10, repeats=1) # k-fold CV


## Run the CV1
CV_results <- preg_wf %>%
  tune_grid(resamples=folds,
          grid=tuning_grid,
          metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL

## Plot Results (example)
collect_metrics(CV_results) %>% # Gathers metrics into DF
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

## Find Best Tuning Parameters13
bestTune <- CV_results %>%
  select_best("rmse")


## Finalize the Workflow & fit it1
final_wf <- preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=log_train_set)

## Prediction
# final_wf %>%
#   predict(new_data = myNewData)

data_test <- vroom("test.csv") # input test data

final_log_lin_preds <- predict(final_wf, new_data = data_test) %>% #This predicts log(count)
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., data_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

vroom_write(final_log_lin_preds, "bike_predictions_penreg_cv.csv", delim = ",")



################################################################################
# Random Forest Tuning
# library(tidymodels)
# 
# 
# require(xgboost)
# 
# data_train <- vroom("train.csv") # grab training data
# 
# data_test <- vroom("test.csv") # input test data
# 
# pred <- predict(bst, data_test)
# 
# boost_tree









