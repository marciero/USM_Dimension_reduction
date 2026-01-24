

skimr::skim(ames)

ames_prep <- recipe(Sale_Price ~ ., ames) %>%
    step_dummy(all_nominal()) %>%
    prep() %>% bake(ames)

skimr::skim(ames_prep)



hotels <-
    read_csv("https://tidymodels.org/start/case-study/hotels.csv")


hotel_prep <- recipe(children ~ ., hotels) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep() %>% bake(hotels)
skimr::skim(hotel_prep)
