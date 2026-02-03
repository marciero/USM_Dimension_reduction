library(tidyverse)  ## install.packages("tidyverse") if needed
library(faraway)    ## install.packages("faraway") if needed. gives us the data set
library(tidymodels)  ## install.packages("tidymodels") if needed

## "Manual" PCR example using meatspec data from faraway package.
# You can find information on the data set executing the following

?meatspec

## Fit linear model using the 100 light absorbance predictors
fit <- lm(fat ~ ., meatspec)

## Find the rmse and r squared using metric() from tidymodels.
# We skip training and test splits as this is for this illustration.
cbind(preds = predict(fit, meatspec), truth = meatspec$fat) %>% as_tibble %>% metrics(truth, preds)

## Note that we could do this "manually" and is actually simpler for this example.
preds = predict(fit, meatspec)
sqrt(sum((meatspec$fat - preds)^2)/215)

## Scale data

x<- as.matrix(meatspec[, 1:100])   %>% scale()
XX <- t(x) %*% x

## Eigen decomp
XXeig <- eigen(XX)

## condition number. It's very big.
XXeig$values[1]/XXeig$values[100]

## The matrix is positive definite and invertible, but practically speaking, it might be considered singular
## as the smallest eigenvalues are ~10^(-9) very close to zero.

## We find the cumulative percent variation in the components as in class
total_XXvar <- XXeig$values %>% sum()
XXeig$values %>% cumsum()/total_XXvar

## It looks like n = 8 components to get 100% to five decimal places.


## Fit the model with eight components and find the rmse
Z <- x %*% XXeig$vectors
fitZ <- lm(meatspec$fat ~ Z[, 1:8])
preds = predict(fitZ, meatspec)
sqrt(sum((meatspec$fat - preds)^2)/215)

## We see that the rmse is a bit bigger than for the full model. Still not too bad though as the mean for fat
# content is 18.1 with sd of 12.7. And as was noted, one of the drawbacks of PCR is that, as an unsupervised
# method, it will miss components without a lot of variation but that are correlated or predictive of the
# response. Another thing to note is that this illustrates that r-squared  value may not be the best measure
# of predictive performance. It's really a correlation. Generally, rmse will be preferred. Also, we really want to be using training and test splits.

## You can try the above using eigenvalues of the correlation matrix instead.



