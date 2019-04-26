list.files(RESULT_DIR) %>%
    sapply(function(x) paste0(RESULT_DIR, "/", x)) %>%
    lapply(readRDS) %>%
    lapply(function(x) x$test.error) %>%
    bind_rows() %>%
    rmsle() %>%
    group_by(class) %>%
    arrange(rmsle) %>%
    ungroup() %>%
    arrange(rmsle) %>%
    View()

log.sometimes = function(msg) {
    if (rnorm(1) > 1.0) {
        loginfo(msg)
    }
}

big.results = list()
for (file in list.files(RESULT_DIR)) {
    filename = paste0(RESULT_DIR, "/", file)
    big.results[[filename]] = filename %>%
        readRDS() %>%
        (function(x) x$test.error)

    paste(object.size(big.results)/1000000, "megabytes loaded") %>% log.sometimes()
}

big.results %>%
    bind_rows() %>%
    rmsle() %>%
    group_by(class) %>%
    arrange(rmsle) %>%
    dplyr::slice(1) %>%
    ungroup() %>%
    arrange(rmsle) %>%
    View()