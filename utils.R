loginfo("Loading utils...")

#' Returns a dataframe which is a mapping between the colnames in the
#' input and the number of NAs in the column.
count_na_by_column = function(df) {
    df[!complete.cases(df),] %>%
        gather() %>%
        filter(is.na(value)) %>%
        select(key) %>%
        group_by(key) %>%
        summarise(count = n())
}

named.apply = function(xs, f) {
    result = list()
    for (x in xs) {
        result[[x]] = f(x)
    }

    data.frame(result)
}

assert = function(x, ...) {
    if (!x) {
        msg = paste("Assertion invalid: ", ...)
        stop(msg)
    }
}

assert.complete = function(x, ...) {
    assert(nrow(x) == length(complete.cases(x)), "dataset is not complete", ...)
}
