library(tidyverse)
library(logging)
basicConfig()
loginfo("Script entry")

source("data-loader.R")
source("utils.R")

source("super-lm.R")

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

#' Perform K-fold cross-validation on regressor.
#' @param regressor the regressor to use.
#' @param K the number of folds.
#' @param xs features.
#' @param ys targets.
cross.validate = function(regressor, K, xs, ys) {
    s = sample(nrow(xs))
    fi = 1:nrow(xs) %% K

    partition = function(k) {
        paste("Partitioning data for fold", k) %>% loginfo()
        # TODO: Fix factor levels being lost
        # this needs to be done smarter; you need at least one example
        # of each factor level present in each fold
        list(
            test_x  = xs[s,][fi == k,],
            test_y  = ys[s,][fi == k,],
            train_x = xs[s,][fi != k,],
            train_y = ys[s,][fi != k,],
            fold = k
        )
    }

    proc = function(p) {
        paste("Beginning processing for fold", p$fold) %>% loginfo()
        res = process_regressor(regressor, p$train_x, p$train_y, p$test_x, p$test_y)
        paste("Fold", p$fold, "complete") %>% loginfo()
        res$test.error$cv.fold = p$fold
        res$test.error
    }

    1:K %>%
    lapply(partition) %>%
    lapply(proc) %>%
    bind_rows()
}

#' Perform N rounds of K-fold cross validation.
#' @param regressor the regressor to cross-validate.
#' @param N the number of rounds to perform.
#' @param K the number of folds to use.
#' @param xs features
#' @param ys targets
n.cross.validate = function(regressor, N, K, xs, ys) {
    paste("Performing", N, "rounds of", K, "fold cross-validation") %>% loginfo()

    proc = function(x) {
        paste("Beginning round", x) %>% loginfo()
        res = cross.validate(regressor, K, xs, ys)
        res$cv.round = x
        paste("Round", x, "complete") %>% loginfo()
        res
    }

    1:N %>%
    lapply(proc)
}

regressors = list(
    lm_basic,
    lm_reduced,
    lm_crossed
)

basic.results = n.cross.validate(lm_basic, 1, 10, train_x, train_y)
big.results = lapply(regressors, n.cross.validate, 10, 10, train_x, train_y)
