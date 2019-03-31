loginfo("Loading cross-validation module...")

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

#' Perform N rounds of K-fold cross validation.
#' @param regressors the regressors to cross-validate.
#' @param N the number of rounds to perform.
#' @param K the number of folds to use.
#' @param xs features
#' @param ys targets
n.cross.validate = function(regressors, N, K, xs, ys) {
    paste("Performing", N, "rounds of", K, "fold cross-validation") %>% loginfo()
    result = list()
    for (n in 1:N) {
        paste("Beginning round", n) %>% loginfo()
        fold.index = create.folds(xs, K)
        for (k in 1:K) {
            paste("Beginning processing for fold", k) %>% loginfo()
            tsti = fold.index[fold.index$fold == k,"row"]
            trni = fold.index[fold.index$fold != k,"row"]
            tst_x = xs[tsti,]
            tst_y = ys[tsti,]
            trn_x = xs[trni,]
            trn_y = ys[trni,]

            for (regressor in regressors) {
                res = process_regressor(regressor, trn_x, trn_y, tst_x, tst_y)
                res$test.error$cv.fold = k
                res$test.error$cv.round = n
                result <- append(result, list(res))
            }

            paste("Fold", k, "complete") %>% loginfo()
        }

        paste("Round", n, "complete") %>% loginfo()
    }
    
    return(result)
}
