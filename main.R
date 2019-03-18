library(tidyverse)

source("data-loader.R")
source("utils.R")

train = load_data("data/train.csv")
if (nrow(train) != nrow(na.omit(train))) {
    stop("some rows are incomplete!")
}

targets = c("SalePrice", "YrSold")
train_x = train %>% select(-targets)
train_y = train %>% select(targets)

process_regressor = function(regressor, train_x, train_y) {
    fit = regressor$call(train_x, train_y) %>%
        mutate(id=1:nrow(.)) %>%
        gather(key="key", value="fit", -id) %>%
        mutate(predictor = regressor$name)

    truth = train_y %>%
        mutate(id=1:nrow(.)) %>%
        gather(key="key", value="truth", -id)

    left_join(fit, truth, by=c("id", "key")) %>%
        mutate(error = fit - truth)
}

regressor.truth = list(
    name = "truth",
    call = function(train_x, train_y) train_y
)

source("super-lm.R")

regressors = list(
    regressor.truth,
    lm_basic,
    lm_reduced,
    lm_crossed
)

process_regressor(lm_crossed, train_x, train_y)
