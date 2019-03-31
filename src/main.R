library(tidyverse)
library(logging)
basicConfig()
loginfo("Script entry")

source("src/data-loader.R")
source("src/utils.R")
source("src/evaluation.R")
source("src/cross-validation.R")

source("regressors/super-lm.R")
source("regressors/xgboost.R")

train = load_data("data/train.csv")
if (nrow(train) != nrow(na.omit(train))) {
    stop("some rows are incomplete!")
}

targets = c("SalePrice")
train_x = train %>% select(-targets)
train_y = train %>% select(targets)

#' Train a regressor on the training data and evalute it's performance on
#' the test set.
#' @param regressor the regressor to use.
#' @param train_x features (training set)
#' @param train_y targets (training set)
#' @param test_x features (testing set)
#' @param test_y targets (testing set)
process_regressor = function(regressor, train_x, train_y, test_x, test_y) {
    paste("Processing regressor [", regressor$name, "]") %>% loginfo()

    loginfo("Training...")
    reg = regressor$train(train_x, train_y)

    loginfo("Testing...")
    test.fit = reg$predict(test_x) %>%
        mutate(id=1:nrow(.)) %>%
        gather(key="key", value="fit", -id)

    test.truth = test_y %>%
        mutate(id=1:nrow(.)) %>%
        gather(key="key", value="truth", -id)

    test.error = test.fit %>%
        left_join(test.truth, by=c("id", "key")) %>%
        mutate(error = fit - truth) %>%
        mutate(predictor = regressor$name) %>%
        mutate(class = reg$class)

    paste("Completed regressor [", regressor$name, "]") %>% loginfo()
    list(
        model.obj = reg,
        test.error = test.error
    )
}

regressors = list(
    lm_basic,
    xgboost10,
    xgboost50,
    xgboost100,
    xgboost200
)

big.results = n.cross.validate(regressors, 10, 10, train_x, train_y)
big.test.error = lapply(big.results, function(x) x$test.error) %>% bind_rows()

big.results %>%
    lapply(function(x) x$test.error) %>%
    bind_rows() %>%
    rmsle() %>%
    print() %>%
    ggplot(aes(x=predictor, y=rmsle, colour=class, fill=predictor)) +
    geom_col() +
    facet_wrap(~key, scales="free_y")
