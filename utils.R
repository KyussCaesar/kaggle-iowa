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

cv.folds.sbs = function(df, n.folds) {
    df2 = factors.to.one.hot(df)
    strata.cols =
        df2 %>%
        sapply(is.factor) %>%
        which()

    row = c()
    fold = c()
    for (s in strata.cols) {
        strata = which(df2[,s] %>% as.logical.factor())

        for (f in 1:n.folds) {
            row <- append(row, sample(strata, 1))
            fold <- append(fold, f)
        }
    }

    data.frame(row = row, fold = fold)
}
cv.folds.ss = function(df, n.folds) {
    
    # convert factors to one-hot encoding
    df2 = factors.to.one.hot(df)
    
    # determine which columns are strata
    strata.cols =
        df2 %>%
        sapply(is.factor) %>%
        which()

    # find out which strata have fewer than n.folds members
    # omit them
    csc = c()
    mfd = data.frame(row=0, mfd=T)
    for (s in strata.cols) {
        rows = df2[,s] %>% as.logical() %>% which()
        s.ok = length(rows) > n.folds
        csc <- append(csc, s.ok)
        if (!s.ok) {
            mfd = rbind(mfd, data.frame(row=rows, mfd=T))
        }
    }

    sc2 = strata.cols[csc]
    srf = sc2 %>% lapply(
        function(x) data.frame(
            strata = x,
            row    = which(df2[,x] %>% as.logical())
        )
    ) %>%
    bind_rows()

    srf = srf[sample(nrow(srf)),]
    
    mfd = srf %>%
        group_by(row) %>%
        summarise(mfd = any(strata %in% bad.strata)) %>%
        pull(mfd)

    result = data.frame(
        fold = 1:nrow(df2) %% n.folds,
        strata = 0,
        row = 0
    )

    for (i in 1:nrow(srf)) {
        if (srf[i,"row"] %in% unique(result$row)) next

        strat = srf[i,"strata"]
        rw = srf[i,"row"]
        pref.f = result %>%
            mutate(is.strat = strata == strat) %>%
            group_by(fold) %>%
            summarise(count.strat = length(which(is.strat))) %>%
            arrange(count.strat) %>%
            pull(fold)
        
        for (pf in pref.f) {
            candidates = which(result$fold == pf & result$row == 0)
            
            if (length(candidates != 0)) break
        }

        which.one = min(candidates)
        result[which.one,"row"] = rw
        result[which.one,"strata"] = strat

        # break once all rows have been allocated
        if (!any(result$row == 0)) break
    }

    data.frame(row = result$row, fold = result$fold)
}

cardinality = function(x) {
    length(unique(x))
}

create.folds = function(df) {
    folds = createMultiFolds(df, k=10, times=10)
    stupid.names = data.frame(fold=NULL, round=NULL, row=NULL)
    
    for (i in 1:10) {
        for (j in 1:10) {
            which.one = 10*(i-1) + j
            stupid.names <- rbind(
                stupid.names,
                data.frame(fold=j, round=i, row=folds[[which.one]])
            )
        }
    }
}