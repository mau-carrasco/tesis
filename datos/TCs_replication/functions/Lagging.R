### this function lags the selected 'vars' (column names) in 'df' by one year
Lagging <- function(df, vars) {
    lagged <- df[, c("country", "year", vars)]
    lagged$year <- lagged$year + 1
    df <- df[, !(names(df) %in% vars)]
    df <- merge(df, lagged, by = c("country", "year"), all.x = TRUE)
    return(df)
}
