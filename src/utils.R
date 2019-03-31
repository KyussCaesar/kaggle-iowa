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

#' Where `xs` is some list of names, return the list `l[x] = f(x)`
#' @param xs the names
#' @param f the function to apply
named.apply = function(xs, f) {
    result = list()
    for (x in xs) {
        result[[x]] = f(x)
    }

    data.frame(result)
}

#' Stop execution if `x` is false.
assert = function(x, ...) {
    if (!x) {
        msg = paste("Assertion error: ", ...)
        stop(msg)
    }
}

assert.complete = function(x, ...) {
    assert(nrow(x) == length(complete.cases(x)), "dataset is not complete", ...)
}

assert.all = function(x, pred, ...) {
    assert(all(sapply(x, pred)), ...)
}

assert.any = function(x, pred, ...) {
    assert(any(sapply(x, pred)), ...)
}

assert.eq = function(x, y, ...) {
    assert(x == y, ...)
}

assert.nz = function(x, ...) {
    if (x %>% is.data.frame()) assert(nrow(x) != 0, ...)
    if (length(x) == 1) assert(x != 0, ...)
    assert(length(x) != 0)
}

#' Converts all of the factor columns in `df` to one-hot encoded variants.
#' New columns have names of the format `oldname.level`, so for example the
#' factor `colour` with levels `Red, Green, Blue` would expand to three columns
#' called `colour.Red`, `colour.Green` and `colour.Blue` respectively.
#' 
#' @param df the data frame to apply the transformation to.
factors.to.one.hot = function(df) {

    factor.cols = which(sapply(df, is.factor))
    f.col.names = colnames(df)[ factor.cols]
    n.col.names = colnames(df)[-factor.cols]

    result = list()
    for (name in n.col.names) {
        result[[name]] = df[[name]]
    }

    for (name in f.col.names) {
        for (level in levels(df[[name]])) {
            new.name = paste(name, level, sep=".")
            result[[new.name]] = factor(df[[name]] == level)
        }
    }

    data.frame(result)
}

cardinality = function(x) {
    length(unique(x))
}

count = function(x) {
    length(which(x))
}
