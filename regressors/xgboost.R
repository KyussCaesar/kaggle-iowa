library(xgboost)

prep.train = function(train_x) {
    trn = factors.to.one.hot(train_x)
    for (name in colnames(trn)) {
        if (trn[[name]] %>% is.factor()) {
            trn[[name]] = as.numeric(as.logical(trn[[name]]))
        }
    }

    as.matrix(trn)
}

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

xgboost10 = list(
    name = "xgboost10",
    train = function(train_x, train_y) {
        models = list()
        trn = prep.train(train_x)
        for (name in colnames(train_y)) {
            dtrain = xgb.DMatrix(data=trn, label=train_y[[name]])
            models[[name]] = xgb.train(data=dtrain, nrounds=10)
        }
        
        xgb.result(models)
    }
)

xgboost50 = list(
    name = "xgboost50",
    train = function(train_x, train_y) {
        models = list()
        trn = prep.train(train_x)
        for (name in colnames(train_y)) {
            dtrain = xgb.DMatrix(data=trn, label=train_y[[name]])
            models[[name]] = xgb.train(data=dtrain, nrounds=50)
        }
        
        xgb.result(models)
    }
)

xgboost100 = list(
    name = "xgboost100",
    train = function(train_x, train_y) {
        models = list()
        trn = prep.train(train_x)
        for (name in colnames(train_y)) {
            dtrain = xgb.DMatrix(data=trn, label=train_y[[name]])
            models[[name]] = xgb.train(data=dtrain, nrounds=100)
        }
        
        xgb.result(models)
    }
)

xgboost200 = list(
    name = "xgboost200",
    train = function(train_x, train_y) {
        models = list()
        trn = prep.train(train_x)
        for (name in colnames(train_y)) {
            dtrain = xgb.DMatrix(data=trn, label=train_y[[name]])
            models[[name]] = xgb.train(data=dtrain, nrounds=200)
        }
        
        xgb.result(models)
    }
)
