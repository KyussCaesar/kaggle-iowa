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

#' Partition df into n.folds folds.
#' @param df the data frame to partition.
#' @param n.folds the number of folds.
#' 
#' This function attempts to partition the data such that
#' each fold has a similar number of examples of each level
#' of each factor in df.
create.folds = function(df, n.folds) {
    paste("Partitioning", nrow(df), "rows into", n.folds, "folds") %>% loginfo()

    clearn = function(df2, n.folds) {
        assert.nz(df2 %>% nrow())
        for (i in 1:ncol(df2)) {
            if (df2[,i] %>% is.factor()) {
                row.select = as.logical(df2[,i])

                if (all(row.select == FALSE)) {
                    return(clearn(df2[,-i], n.folds))
                }

                rows = which(row.select)
                s.ok = length(rows) >= n.folds

                if (!s.ok) {
                    return(clearn(df2[-rows,-i], n.folds))
                }
            }
        }

        return(df2)
    }

    # convert factors to one-hot encoding
    df2 = factors.to.one.hot(df) %>% clearn(n.folds)  

    assert.nz(df2, "all rows were clearned")

    # determine which rows belong to which strata
    srf =
        df2 %>%
        sapply(is.factor) %>%
        which() %>%
        lapply(
            function(x) data.frame(
                strata = x,
                taken  = FALSE,
                row    = which(df2[,x] %>% as.logical())
            )
        ) %>%
        bind_rows()

    # defines how to update a fold:
    # 1. find the strata which the fold has the fewest members of
    # 2. pick a random row from that strata and add it to the fold
    # 3. remove all occurences of that row from srf.
    update = function(fold) {
        fsmap = count.strata.in.fold(fold)
        weakest = fsmap[order(fsmap$count),"strata"]

        candidate.rows = c()
        for (weak in weakest) {
            if (all(srf[srf$strata == weak,"taken"])) next
            candidate.rows = srf[srf$strata == weak & srf$taken != TRUE,"row"]
            if (length(candidate.rows) != 0) break
        }

        if (length(candidate.rows) == 0) {
            logerror("Zero candidates for fold update")
            browser()
        }

        if (length(candidate.rows) == 1) which.row = candidate.rows
        else which.row = sample(candidate.rows, 1)

        if (any(srf[srf$row == which.row,"taken"])) {
            logerror("Tried to add a row which was already taken")
            browser()
        }

        srf[srf$row == which.row,"taken"] <<- TRUE

        fold %>%
        unlist() %>%
        append(which.row) %>%
        list()
    }

    count.strata.in.fold = function(fold) {
        result = c()
        for (row in unlist(fold)) {
            assert(length(row) == 1)
            result <- append(result, srf[srf$row == row,"strata"])
        }

        dd = lapply(unique(srf$strata), function(x) data.frame(strata=x, count=0)) %>% bind_rows()
        for (r in result) {
            which.strata = dd$strata == r
            strata.count = dd[which.strata,"count"]
            dd[which.strata,"count"] = strata.count + 1
        }

        return(dd)
    }

    fi = 0
    folds = lapply(1:n.folds, function(x) list())
    while (count(srf$taken) != nrow(srf)) {
        folds[[fi+1]] = update(folds[[fi+1]])
        fi = (fi + 1) %% n.folds
        n.proc = cardinality(srf[srf$taken,"row"])
        if (n.proc %% 30 == 0) n.proc %>% paste("rows processed") %>% loginfo()
    }

    loginfo("All rows processed, partition complete")

    1:n.folds %>%
    lapply(function(x) data.frame(fold=x, row=unlist(folds[[x]]))) %>%
    bind_rows() %>%
    mutate(row = as.numeric(rownames(df2)[row]))
}

cardinality = function(x) {
    length(unique(x))
}

count = function(x) {
    length(which(x))
}
