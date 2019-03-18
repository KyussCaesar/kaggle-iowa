lm_basic = list(
    name = "basic lm",
    call = function(train_x, train_y) {
        result = list()
        for (name in colnames(train_y)) {
            result[[name]] =
                paste(name, "~ .") %>%
                as.formula() %>%
                lm(data=cbind(train_x, train_y)) %>%
                (function(x) x$fitted.values)
        }

        data.frame(result)
    }
)

lm_reduced = list(
    name = "reduced lm",
    call = function(train_x, train_y) {
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

        result = list()
        for (name in colnames(train_y)) {
            # find significant columns
            sig.cols =
                paste(name, "~ .") %>%
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
            result[[name]] =
                paste(nsp(name), "~", paste(sig.cols, collapse = " + ")) %>%
                as.formula() %>%
                lm(data=cbind(train_x, train_y)) %>%
                (function(x) x$fitted.values)
        }

        data.frame(result)
    }
)

lm_crossed = list(
    name = "crossed lm",
    call = function(train_x, train_y) {
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

        result = list()
        for (name in colnames(train_y)) {
            # find significant columns
            sig.cols =
                paste(name, "~ .") %>%
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
                nsp()

            # then fit a new one based on those columns
            result[[name]] =
                paste(nsp(name), "~", paste(sig.cols, collapse = " * ")) %>%
                as.formula() %>%
                lm(data=cbind(train_x, train_y)) %>%
                (function(x) x$fitted.values)
        }

        data.frame(result)
    }
)
