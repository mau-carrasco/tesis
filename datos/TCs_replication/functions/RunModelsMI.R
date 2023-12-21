RunModelsMI <- function(imputations, outcome, models, varstable, estimator,
    add_to_formula = NULL, ## to append to model formula (this is needed because the formulas are based on the
                           ## info in 'varstable' but the "within" models cannot include the first-year values of the DV)
    filenameadd = ""       ## to append to the file name for savings estimates
) {
    names(models) <- models
    results <- lapply(models, function(model) {
        ### setting up formula from info in 'varstable'
        xvars <- c(varstable$var[varstable[, model] %in% c("x", "w")], add_to_formula)
        zvars <- varstable$var[varstable[, model] %in% c("z")]
        wvars <- varstable$var[varstable[, model] %in% c("w")]
        equation <- paste("outcome ~ ", paste(xvars, collapse = " + "), " | . - ", paste(wvars, collapse = " - "), " + ", paste(zvars, collapse = " + "), sep = "")
        cat("\n\nModel \"", model, "\":\n", equation, "\n", sep = "")
        ### estimating models on each multiply imputed dataset and saving info for inclusion in tables/figures
        estMI <- lapply(imputations, function(imp) {
            fit <- plm(formula = equation, data = imp, index = c("country", "year"), model = estimator, effect = "individual")
            print(fit)
            index <- attr(fit$model, "index")
            info <- list(
                nobs = nrow(fit$model),
                ncountries = length(unique(index[, "country"])),
                years = range(as.integer(as.character(index$year))),
                countries = as.character(unique(index[, "country"])),
                rsquared = abs(summary(fit)[["r.squared"]][["adjrsq"]])
            )
            return(list(fit = fit, info = info))
        })
        ### extracting relevant model information
        info <- list(nobs = unique(unlist(lapply(estMI, function(x){x$info$nobs}))))
        info$ncountries <- unique(unlist(lapply(estMI, function(x){x$info$ncountries})))
        info$years <- unlist(unique(lapply(estMI, function(x){ paste(x$info$years, collapse = "-") })))
        info$estimator <- estimator
        ### taking the average of R-squared for MI models to report in regression tables
        info$rsquared <- round(mean(unlist(lapply(estMI, function(x){x$info$rsquared}))), 3)
        est <- lapply(estMI, function(x) coef(x$fit))
        ses <- lapply(estMI, function(x) sqrt(diag(vcov(x$fit))) )
        ### combining estimates and standard errors for multiply imputed dataset
        output <- mi.meld(q = data.frame(est), se = data.frame(ses), byrow = FALSE)
        return(list(estimates = list(est = TableMI(output), info = info), estMI = estMI))
        ### we only save (to file) and use the aggregated 'estimates' for creating figures and tables for publication
        ### but 'estMI' is also returned in case complete regression model fit info is needed
    })
    estimates <- lapply(X = results, FUN = function(x){x[["estimates"]]})
    estMI <- lapply(X = results, FUN = function(x){x[["estMI"]]})
    ### saving estimates to files
    cat("Saving results to files... ")
    if(nchar(filenameadd) > 0) filenameadd <- paste("_", filenameadd, sep = "")
    save(estimates, file = paste("estimates/", outcome, "_estimates", filenameadd, ".RData", sep = ""))
    cat("done.\n\n")
    invisible(estMI)
}
