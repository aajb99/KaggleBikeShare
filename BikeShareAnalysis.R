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





