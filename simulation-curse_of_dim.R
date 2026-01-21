## Curse of dimensionality simulation
## We illustrate how as dimensionality increases, data become sparse

library(tidyverse)
library(tidymodels)

#####
#######  Start with two categorical variables with few levels

n = 100
set.seed(123)
nhd <- sample(c("A", "B", "C"), size = n, replace = TRUE)
house_type <- sample(c("1stHouse", "2stHouse", "3stHouse"), size = n, replace = TRUE)

df <- data.frame(nhd, house_type)

# Use tidymodels recipes to one-hot encode the categorical variables
df_prep <- df %>% recipe(~.) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep() %>%
    bake(new_data = df)

## Find distances of from first observation to others
## Use "manhattan" distance- it's simple

head(df_prep)

# First get where they differ from "1" values in first observation
# You will likely have to adjust this line based on the first observation values!
df_dist1 <- df_prep %>% mutate(dist1 = abs(nhd_B -1))

# First observation values from the other columns are all zero, so we can just sum the other observation values
# to get their contribution
df_dist2 <- df_prep %>% select(-nhd_B) %>% rowwise() %>%  # need rowwise() here!
    mutate(dist2 = sum(across(everything())))

dist <-  df_dist1$dist1 + df_dist2$dist2

df_dist <- df_prep %>% mutate(dist = dist)

## Possible distances are 0,1,2,3
# Find proportion of the data at each distance from the first observation

df_dist %>% summarize(
    mean_dist0 = mean(dist == 0),
    mean_dist1 = mean(dist == 1),
    mean_dist2 = mean(dist == 2),
    mean_dist3 = mean(dist == 3)
)

#####
####  Now add more levels to each categorical variable


n = 100
set.seed(123)
nhd <- sample(c("A", "B", "C", "D", "E", "F", "G"), size = n, replace = TRUE)
house_type <- sample(c("1stHouse", "2stHouse", "3stHouse", "SmCondo", "LgCondo"), size = n, replace = TRUE)

df <- data.frame(nhd, house_type)


df_prep <- df %>% recipe(~.) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep() %>%
    bake(new_data = df)

## Find distances of from first observation to others
## Use "manhattan" distance- it's simple

head(df_prep)

# First get where they differ from "1" values in first observation
df_dist1 <- df_prep %>% mutate(dist1 = abs(nhd_G -1) + abs(house_type_SmCondo -1))

# First observation values from the other columns are all zero, so we can just sum the other observation values
# to get their contribution
df_dist2 <- df_prep %>% select(-nhd_G, -house_type_SmCondo) %>% rowwise() %>%  # need rowwise() here!
    mutate(dist2 = sum(across(everything())))

dist <-  df_dist1$dist1 + df_dist2$dist2

df_dist <- df_prep %>% mutate(dist = dist)

## Possible distances are 0,1,2,3,4
# Find proportion of the data at each distance from the first observation

df_dist %>% summarize(
    mean_dist0 = mean(dist == 0),
    mean_dist1 = mean(dist == 1),
    mean_dist2 = mean(dist == 2),
    mean_dist3 = mean(dist == 3),
    mean_dist4 = mean(dist == 4)
)



