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

vroom_write(bike_predictions, "bike_predictions.csv", delim = ",")
