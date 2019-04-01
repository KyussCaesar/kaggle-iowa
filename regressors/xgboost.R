library(xgboost)

# Convert factors to one-hot encoding and as.matrix
prep.train = function(train_x) {
    trn = factors.to.one.hot(train_x)
    for (name in colnames(trn)) {
        if (trn[[name]] %>% is.factor()) {
            trn[[name]] = as.numeric(as.logical(trn[[name]]))
        }
    }

    as.matrix(trn)
}

#' Prepare result from xgboost.
xgb.result = function(models) {
    loginfo("Completed fitting an xgboost")
    list(
        model = models,
        class = "xgboost",
        predict = function(test_x) {
            tst = prep.train(test_x)
            named.apply(names(models), function(x) predict(models[[x]], tst))
        }
    )
}

#' Make an xgboost regressor.
#' @param n xgboost nrounds param.
#' @param d xgboost max_depth param.
#' @return regressor
mk.xgb.reg = function(n, d) {
    list(
        name = paste0("xgboost", n, "d", d),
        train = function(train_x, train_y) {
            models = list()
            trn = prep.train(train_x)
            for (name in colnames(train_y)) {
                dtrain = xgb.DMatrix(data=trn, label=train_y[[name]])
                models[[name]] = xgb.train(data=dtrain, nrounds=n, max_depth=d)
            }

            xgb.result(models)
        }
    )
}
