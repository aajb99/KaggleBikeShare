library(tidyverse)
install.packages('tidymodels')
install.packages('tidyverse')
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

data1 <- data1 %>%
  mutate(across(factor_cols, as.factor)) # convert factor variables

# str(data1)

data1 <- data1 %>%
  select(-registered, -casual) # drop registered and casual

view(data1)

###############################################################################

rFormula <- count ~ .

my_recipe <- recipe(rFormula, data = data1) %>% # set model formula and dataset
  step_time(datetime, features = 'hour') %>% # get hours
  step_dummy(all_nominal_predictors()) # get dummy variables

prepped_recipe <- prep(my_recipe) # preprocessing new data
baked_data1 <- bake(prepped_recipe, new_data = data1)

factor_cols2 <- c(8:14)
baked_data1 <- baked_data1 %>%
  mutate(across(factor_cols2, as.factor)) # Convert dummy variables into factors

view(baked_data1[1:10,])
