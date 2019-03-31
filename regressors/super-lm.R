loginfo("Loading lm regressors")

lm.result = function(models) {
    loginfo("Completed fitting an lm")
    list(
        model = models,
        class = "lm",
        predict = function(test_x)
            named.apply(names(models), function(x) predict(models[[x]], test_x))
    )
}

lm.prep = function(xs, proc=identity) {
    proc(colnames(xs[sapply(xs, cardinality) != 1]))
}

lm_basic = list(
    name = "basic lm",
    train = function(train_x, train_y) {
        models = list()
        nsp = function(x) paste0("`", x, "`")
        cols = train_x %>% lm.prep() %>% sapply(nsp) %>% paste(collapse=" + ")
        for (name in colnames(train_y)) {
            models[[name]] =
                paste(name, "~", cols) %>%
                as.formula() %>%
                lm(data=cbind(train_x, train_y))
        }

        lm.result(models)
    }
)

lm_reduced = list(
    name = "reduced lm",
    train = function(train_x, train_y) {
        train_x.factors = train_x[,sapply(train_x, is.factor)]
        map.factor.levels = train_x.factors %>% sapply(levels)

        map.maker = function(name) {
            if (is.factor(train_x[[name]]))
                data.frame(
                    varnames = paste0(name, map.factor.levels[[name]]),
                    var = name
                )
            else
                data.frame(
                    varnames = name,
                    var = name
                )
        }

        # mapping between colnames in x and varnames as they appear
        # in summary.lm
        map.flvl.factor =
            colnames(train_x) %>%
            lapply(map.maker) %>%
            bind_rows()

        # `n`on `s`yntactic `p`rotect
        # wrap xs in backticks so formula doesn't freak out.
        nsp = function(x) paste0("`", x, "`")

        models = list()
        cols = train_x %>% lm.prep() %>% sapply(nsp) %>% paste(collapse=" + ")
        for (name in colnames(train_y)) {
            sig.cols =
                paste(name, "~", cols) %>%
                as.formula() %>%
                lm(data=cbind(train_x, train_y)) %>%
                summary() %>%
                coef() %>% 
                data.frame() %>%
                mutate(varnames = gsub("`", "", rownames(.))) %>%
                rename(p.value = Pr...t..) %>%
                filter(p.value < 0.01) %>%
                select(p.value, varnames) %>%
                left_join(map.flvl.factor, by="varnames") %>%
                pull(var) %>%
                unique() %>%
                nsp()

            # then fit a new one based on those columns
            models[[name]] =
                paste(nsp(name), "~", paste(sig.cols, collapse = " + ")) %>%
                as.formula() %>%
                lm(data=cbind(train_x, train_y))
        }

        lm.result(models)
    }
)

lm_crossed = list(
    name = "crossed lm",
    train = function(train_x, train_y) {
        train_x.factors = train_x[,sapply(train_x, is.factor)]
        map.factor.levels = train_x.factors %>% sapply(levels)

        map.maker = function(name) {
            if (is.factor(train_x[[name]]))
                data.frame(
                    varnames = paste0(name, map.factor.levels[[name]]),
                    var = name
                )
            else
                data.frame(
                    varnames = name,
                    var = name
                )
        }

        # mapping between colnames in x and varnames as they appear
        # in summary.lm
        map.flvl.factor =
            colnames(train_x) %>%
            lapply(map.maker) %>%
            bind_rows()

        # `n`on `s`yntactic `p`rotect
        # wrap xs in backticks so formula doesn't freak out.
        nsp = function(x) paste0("`", x, "`")

        models = list()
        cols = train_x %>% lm.prep() %>% sapply(nsp) %>% paste(collapse=" + ")
        for (name in colnames(train_y)) {
            # find significant columns
            sig.cols =
                paste(name, "~", cols) %>%
                as.formula() %>%
                lm(data=cbind(train_x, train_y)) %>%
                summary() %>%
                coef() %>% 
                data.frame() %>%
                mutate(varnames = gsub("`", "", rownames(.))) %>%
                rename(p.value = Pr...t..) %>%
                select(p.value, varnames) %>%
                left_join(map.flvl.factor, by="varnames") %>%
                arrange(p.value) %>%
                pull(var) %>%
                unique() %>%
                head(5) %>%
                nsp() %>%
                paste(collapse=" * ")

            # then fit a new one based on those columns
            models[[name]] =
                paste(nsp(name), "~", paste(cols, "+", sig.cols)) %>%
                as.formula() %>%
                lm(data=cbind(train_x, train_y))
        }

        lm.result(models)
    }
)
