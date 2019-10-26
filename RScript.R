
## This will be the team's R script

## Load all libraries used in the project
library(readxl)

#load baby data 
babydata<-read_xlsx("babies.xlsx")


## Preliminary Exploration of Data

# Produce a quick visual summary of the data
summary(babydata)

# Our data has:
# 9 numerical variables: 
  # gestation, wt1, parity, Age, ht, dage, wt2, dht, dwt

# 9 Categorical variables
  # race, ed, drace, ded, smoke, time, cigs, marital, inc

prelimlm<-lm()

# Interesting Relationships

# Unusual Data Points


## What relationships are there between the measured variables and the birth weight of babies?
#Build a model, use AIC as at least one of your measures
#build some first-order interactions as part of your pool of models, even if not in final model. 
#Even if these are not in your final model, interpret one fitted interaction effect that you considered a priori logical.

# What are validation datasets, Mean Square Error, hold out a random 20% ?
# What is 5- fold cross validation?

# Check relevant assumptions for whatever final model you settle on.

# Subject your first model to bootstrapping to give empirical confidence intervals for parameters, along with any parametric ones you generate.

# Address practical significance as well as statistical significance
