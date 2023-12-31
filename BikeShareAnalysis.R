library(tidyverse)
#install.packages('tidymodels')
#install.packages('tidyverse')
library(tidymodels)
#install.packages('DataExplorer')
#install.packages("poissonreg")
library(poissonreg)
# install.packages("glmnet")
library(glmnet)
library(patchwork)
# install.packages("rpart")
# install.packages('ranger')
library(ranger)
#install.packages('stacks')
library(stacks)
library(vroom)
library(parsnip)
# install.packages('dbarts')
# library(dbarts)



data1 <- vroom("train.csv") # grab training data
#view(data1)

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
# Regression Tree

my_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

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
  step_rm(datetime)

prepped_recipe <- prep(bike_recipe) # preprocessing new data
baked_data1 <- bake(prepped_recipe, new_data = log_train_set)

## Set Workflow
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(my_mod)

## Grid of values to tune over
tuning_grid <- grid_regular(tree_depth(),
                            cost_complexity(),
                            min_n(),
                            levels = 3) ## L^3 total tuning possibilities

## Split data for CV
folds <- vfold_cv(data_train, v = 10, repeats=1) # k-fold CV


## Run the CV1
CV_results <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL

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

vroom_write(final_log_lin_preds, "bike_predictions_reg_tree.csv", delim = ",")


################################################################################
# Random Forest Tuning

rf_mod <- rand_forest(mtry = tune(),
                      min_n = tune(),
                      trees = 800) %>%
  set_engine('ranger') %>% # What R function to use
  set_mode('regression')

data_train <- vroom("train.csv") # grab training data
# view(data_train)

data_train <- data_train %>%
  select(-casual, - registered) # drop casual and registered variables

log_train_set <- data_train %>%
  mutate(count=log(count))

bike_recipe <- recipe(count ~ ., data=log_train_set) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Change weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>% # change weather as factor INSIDE RECIPE
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>% # convert season to factor with levels
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>% # convert holiday to factor
  step_date(datetime, features = "year") %>%
  step_time(datetime, features="hour") %>% # this hourly variable will replace datetime %>%
  step_rm(datetime) %>%
  step_mutate(datetime_hour=factor(datetime_hour, levels = 0:23, labels = 0:23)) %>%
  step_mutate(datetime_year=factor(datetime_year, levels = 2011:2012, labels = c(2011, 2012)))

prepped_recipe <- prep(bike_recipe) # preprocessing new data
baked_data1 <- bake(prepped_recipe, new_data = log_train_set)

## Set Workflow
rforest_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(rf_mod)

#view(log_train_set)

## Grid of values to tune over
tuning_grid <- grid_regular(mtry(range = c(1, 9)),
                            min_n(),
                            levels = 3) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(data_train, v = 10, repeats=1) # k-fold CV


## Run the CV1
CV_results <- rforest_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL

## Find Best Tuning Parameters13
bestTune <- CV_results %>%
  select_best("rmse")


## Finalize the Workflow & fit it1
final_rforest_wf <- rforest_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=log_train_set)

## Prediction
# final_wf %>%
#   predict(new_data = myNewData)

data_test <- vroom("test.csv") # input test data

final_log_lin_preds <- predict(final_rforest_wf, new_data = data_test) %>% #This predicts log(count)
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., data_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

vroom_write(final_log_lin_preds, "bike_predictions_rforest2.csv", delim = ",")


################################################################################
# Stacked Modeling

data_train <- vroom("train.csv") # grab training data
# view(data_train)

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

# Split data for CV
folds <- vfold_cv(log_train_set, v = 10, repeats = 1)

# Create control grid
untunedModel <- control_stack_grid() # If tuning a model
tunedModel <- control_stack_resamples() # If not tuning a model

###############
# Penalized Regression Model ###
###############

preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

## Set Workflow
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model)

## Grid of values to tune over
preg_tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 5) ## L^2 total tuning possibilities

preg_models <- preg_wf %>%
  tune_grid(resamples = folds,
            grid = preg_tuning_grid,
            metrics = metric_set(rmse, mae, rsq),
            control = untunedModel)
  

########
# Linear Model
########

my_mod <- linear_reg() %>% # Type of Model
  set_engine("lm") # R function to use

lin_reg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(my_mod)

lin_reg_model <- fit_resamples(
  lin_reg_wf,
  resamples = folds,
  metrics = metric_set(rmse, mae, rsq),
  control = tunedModel
)


########
# Random Forest
########

# **flag as control = untuned model

rf_mod <- rand_forest(mtry = tune(),
                      min_n = tune(),
                      trees = 750) %>%
  set_engine('ranger') %>% # What R function to use
  set_mode('regression')

## Set Workflow
rforest_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(rf_mod)

#view(log_train_set)

## Grid of values to tune over
tuning_grid <- grid_regular(mtry(range = c(1, 9)),
                            min_n(),
                            levels = 3) ## L^2 total tuning possibilities

rf_preg_models <- rforest_wf %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(rmse, mae, rsq),
            control = untunedModel)


#######
# Stack the models together

# Base models to include
bike_stack <- stacks() %>%
  add_candidates(preg_models) %>%
  add_candidates(lin_reg_model) %>%
  add_candidates(rf_preg_models)


# Fit the stacked model
fitted_bike_stack <- bike_stack %>%
  blend_predictions() %>%
  fit_members()

data_test <- vroom("test.csv") # input test data

final_stacked_preds <- predict(fitted_bike_stack, new_data = data_test) %>% #This predicts log(count)
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., data_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

vroom_write(final_stacked_preds, "bike_predictions_stack.csv", delim = ",")


################################################################################
# Bayesian Additive Regression Trees (BART)

bart_mod <- parsnip::bart(mode = 'regression',
                 engine = 'dbarts',
                 trees = 30)

## Set Workflow
bart_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(bart_mod) %>%
  fit(data=log_train_set)

data_test <- vroom("test.csv") # input test data

final_bart_preds <- predict(bart_wf, new_data = data_test) %>% #This predicts log(count)
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., data_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

vroom_write(final_bart_preds, "bike_predictions_bart.csv", delim = ",")











