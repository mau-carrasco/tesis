### this function counts the numbers of TCs fulfilling the criteria for our explanatory variables
TCnumbers <- function(
    df = tcs,                    ## 'df' must be the initial list of TCs including the 'tcs_sample_first_year' variable
    restricted_to_sample = FALSE ## FALSE for all TCs in the initial list; TRUE for only TCs operating in country-years included in the sample
) {
    condition <- list(NULL)
    if(restricted_to_sample) {
        condition$operation <- IsTrue(df$tc_begin >= df$tcs_sample_first_year | df$tc_complete >= df$tcs_sample_first_year)
        condition$report <- IsTrue(df$tc_report >= df$tcs_sample_first_year)
        condition$public_report <- IsTrue(df$tc_report_public >= df$tcs_sample_first_year)
        condition$prosecution <- IsTrue(df$tc_rec_prosecute >= df$tcs_sample_first_year)
        condition$reform <- IsTrue(df$tc_rec_reform >= df$tcs_sample_first_year)
    } else {
        condition$operation <- !is.na(df$tc_begin)
        condition$report <- !is.na(df$tc_report)
        condition$public_report <- !is.na(df$tc_report_public)
        condition$prosecution <- !is.na(df$tc_rec_prosecute)
        condition$reform <- !is.na(df$tc_rec_reform)
    }
    output <- list(operation = df$country[condition$operation])
    output$report <- df$country[condition$report]
    output$public <- df$country[condition$public_report]
    output$prosecute <- df$country[condition$prosecution & condition$public_report]
    output$reform <- df$country[condition$reform & condition$public_report]
    output$both <- df$country[condition$prosecution & condition$reform & condition$public_report]
    output$either <- df$country[(condition$prosecution | condition$reform) & condition$public_report]
    output <- lapply(output, function(cases) { list(tcs = length(cases), countries = length(unique(cases))) })
    output <- as.data.frame(do.call(rbind, output))
    return(output)
}
