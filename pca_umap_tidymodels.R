library(tidyverse)  ## install.packages("tidyverse") if needed
library(tidymodels)  ## install.packages("tidymodels") if needed

## PCA using `prcomp()` and `recipes` package

## We use the UCI ML Wine dataset

wine <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data',
                 header = FALSE)

## add descriptive column names
colnames(wine) <- c('Class_label', 'Alcohol', 'Malic_acid', 'Ash',
                    'Alcalinity_of_ash', 'Magnesium', 'Total_phenols',
                    'Flavanoids', 'Nonflavanoid_phenols', 'Proanthocyanins',
                    'Color_intensity', 'Hue',
                    'OD280_OD315_of_diluted_wines', 'Proline')

head(wine)


## We can quickly get  variances and components using `prcomp()`
# The components are called "rotation" in the output, as the they are
# a rotation of the original axes to form the new PCA axes.

wine_pca <- prcomp(wine %>% select(-Class_label),
                   center = TRUE, scale = TRUE)

wine_pca

## If we wanted to perform PCR, we would simply use the scores as the data in a regression model.

### PCA using recipes


pca_rec <- recipe(Class_label~., data = wine) %>%  ## If we had no y variable, we would use recipe(~., data = ...)
    step_normalize(all_predictors()) %>%
    ## add additional steps here if needed (e.g., create dummies)
    step_pca(all_predictors(), num_comp = 13)  ## keep all components, but you can tune this hyper parameter

## This creates the new features from whatever data set we specify above
pca_prep <- prep(pca_rec)

# Select out the second step. `tidy` just puts in nice data frame format.
tidied_pca <- tidy(pca_prep, 2)
tidied_pca

## This form would be nice for plotting the components. See the slides for example plots.

## To get the scores - the transformed data -  we would have to transform the data set using `bake()`
wine_pca_scores <- bake(pca_prep, wine)
wine_pca_scores

## Important: To avoid data leakage, with training and test splits, and resampling (e.g., cross-validation),
## we would only `prep()` the recipe on the training data, and then `bake()` test data separately.
## In practice, in ML pipelines we dont have to use prep and bake separately, But rather
# we would create a pipeline using the `workflows` package and simply use fit and predict
# Python is similar with the use of the `pipeline` object in `sklearn`.

## We might do something like this for a simple train test split:

set.seed(123)

wine$Class_label <- as.factor(wine$Class_label)  ## tidymodels wants factors for classification-not numbers.

wine_split <- initial_split(wine, prop = 0.7, strata = Class_label)
wine_train <- training(wine_split)
wine_test <- testing(wine_split)


pca_rec2 <- recipe(Class_label ~., data = wine_train) %>%
    step_normalize(all_predictors()) %>%
    step_pca(all_predictors(), num_comp = 5)

wine_flow <- workflow() %>%
    add_recipe(pca_rec2) %>%
    add_model(multinom_reg())
## Fit on training data
wine_fit <- fit(wine_flow, data = wine_train)
## Predict on test data
wine_pred <- predict(wine_fit, new_data = wine_test)

augmented_results <- augment(wine_fit, new_data = wine_test)

metrics(augmented_results, truth = Class_label, estimate = .pred_class)


## UMAP is similar - we simply use `step_umap` recipe step. Note that it does
# require the `embed` package. Try it!

library(embed)

