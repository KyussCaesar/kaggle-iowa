loginfo("Loading evaluation module...")

rms = function(x) {
    sqrt(sum(x*x)/length(x))
}

metrics = function(x, e.test=rms, e.folds=mean, e.rounds=mean) {
    x %>%
    group_by(key, class, predictor, cv.round, cv.fold) %>%
    summarise(obj.f = e.test(error)) %>%
    summarise(m.obj = e.folds(obj.f)) %>%
    summarise(m.obj = e.rounds(m.obj)) %>%
    ungroup()
}

rmsle = function(x) {
    x %>%
    mutate(log.error = log(fit+1) - log(truth+1)) %>%
    group_by(key, class, predictor, cv.round, cv.fold) %>%
    summarise(rmsle.f = rms(log.error)) %>%
    summarise(rmsle = mean(rmsle.f)) %>%
    summarise(rmsle = mean(rmsle)) %>%
    ungroup()
}
