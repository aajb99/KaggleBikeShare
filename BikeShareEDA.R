##
## Bike Share EDA Code
##

## Libraries
library(tidyverse)
library(vroom)

## Read in the Data
bike <- vroom("data/train.csv")

#getwd() : gets working directory
#setwd() : set working directory