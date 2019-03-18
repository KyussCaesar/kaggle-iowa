count_na_by_column = function(df) {
    df[!complete.cases(df),] %>%
        gather() %>%
        filter(is.na(value)) %>%
        select(key) %>%
        group_by(key) %>%
        summarise(count = n())
}
