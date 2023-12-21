### this function applies several variable transformations needed for the analyses
Transformations <- function(df,
    latentdraws   = TRUE, ## TRUE incorporates the uncertainty in variables derived from measurement models (V-Dem, LPI) by
                          ## creating multiples of the datasets and drawing those variables from the normal distribution with means
                          ## and standard deviations provided for each country-year observation by the measurement models
    on_unit_scale = NULL, ## vector of names of variables to put on unit scale, for comparison of results across outcome measures
    vars_to_log   = c("pop_wdi", "gdppc_const_wdi"), ## for taking logs of certain covariates
    outcome_var   = NULL  ## name of the outcome variable (DV) in the analysis, to preserve it in the dataframe before lagging the IVs (incl. lagged DV)
) {
    if(latentdraws) {
        df$latentmean_fariss <- suppressWarnings(rnorm(n = nrow(df), mean = df$latentmean_fariss, sd = df$latentsd_fariss))
        df$latentsd_fariss <- NULL
        vdem_vars <- c("v2jureform", "v2x_polyarchy", "v2x_liberal", "v2x_partip", "v2cseeorgs",
                       "v2csreprss", "v2csprtcpt", "v2xel_frefair", "v2x_frassoc_thick", "v2cltort", "v2clkill")
        for(var in vdem_vars) {
            df[, var] <- suppressWarnings(as.numeric(rnorm(nrow(df), mean = df[, var], sd = df[, paste(var, "sd", sep = "_")])))
            df[, paste(var, "sd", sep = "_")] <- NULL
        }
    }
    if(!is.null(on_unit_scale)) {
        df[, on_unit_scale] <- lapply(df[, on_unit_scale], function(col) {
            col <- col - min(col, na.rm = TRUE)
            col <- col / max(col, na.rm = TRUE)
            return(col)
        })
    }
    df[, vars_to_log] <- lapply(df[, vars_to_log], log)
    if(!is.null(outcome_var) ) { df$outcome <- df[, outcome_var] }
    df <- df[order(df$country, df$year), ]
    return(df)
}
