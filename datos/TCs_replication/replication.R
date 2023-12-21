### Author: Oskar Timo Thoms
### Replication code for "Do Truth Commissions Improve Democracy?"
### (co-authored with Geoff Dancy), forthcoming in Comparative Political Studies.
### Version: Feb 10, 2021



#############
### setup ### this replication code was last tested in R version 4.0.3 running on macOS 11.2
#############

rm(list = ls())
library("Amelia")      ## tested with version 1.7.6
library("fastDummies") ## tested with version 1.6.3
library("plm")         ## tested with version 2.4-0
library("tidyverse")   ## tested with version 1.3.0
library("xtable")      ## tested with version 1.8-4
### loading functions used below
### (these functions are specific to this project, and are not taken from existing R packages)
setwd("C:/Users/mcarrasco/Desktop/git/tesis_magister/datos/TCs_replication")
source("functions/ExtractTables.R")
source("functions/Lagging.R")
source("functions/RunModelsMI.R")
source("functions/TableMI.R")
source("functions/TCnumbers.R")
source("functions/TCsResultsFigure.R")
source("functions/Transformations.R")
IsTrue <- function(x) { !is.na(x) & x }



####################
### loading data ###
####################

### We do not include the extensive code for assembling the datasets,
### but provide the initial dataset and the multiply imputed versions of it.
### As detailed and referenced in the article, the dataset contains variables from or coded based on:
### - the Varieties of Democracy project (version 10)
### - the Uppsala Conflict Data Program (version 20.1)
### - the Boix, Miller, and Rosata Democracy dataset (version 3.0)
### - Chris Fariss' latent human right protection scores (version 3.01)
### - the World Development Indicators (version July 2020)
### - the Transitional Justice Research Collaborative data on truth commissions, provided by Geoff Dancy
### - and a transition type indicator coded by Geoff Dancy

### table of variables in the country-year dataset (with labels) and the models to run:
varstable <- read.csv("data/variables_and_models.csv")
### in this table
### - y indicates dependent variables
### - x indicates independent variables
### - ref indicates the reference category of included categorical variables
### - w indicates the instrumented (endogeneous) variable
### - z indicates the instrument

### country-year dataset:
load("data/dataset_cy.RData", verbose = TRUE)

### multiply imputed versions of the country-year data (imputed with the Amelia package):
load("data/imputed.RData", verbose = TRUE)
### this list contains the imputed dataframes in the 'imputations' list of the Amelia-class object,
### with some additional variables merged in which were ignored in the multiple imputation model
### (for instance the TC variables)

### all of our truth commissions variables in the country-year dataset were coded from this original list:
load("data/commissions.RData", verbose = TRUE)

### list of countries and start years in our sample
load("data/sample_cases.RData", verbose = TRUE)


class(sample)


###################################################################################
### TCs and sample descriptives (intro, Table 2 and Appendix Tables 4, 5 and 7) ###
###################################################################################

HowManyOperated <- function(years) {
    cat("operating (", paste(range(years), collapse = "-"), "): ",
        length(tcs$tc_id[tcs$tc_begin %in% years]), " TCs in ",
        length(unique(tcs$country[tcs$tc_begin %in% years])), " countries\n", sep = ""
    )
}
HowManyMandated <- function(years) {
    cat("mandated (", paste(range(years), collapse = "-"), "): ",
        length(tcs$tc_id[IsTrue(tcs$tc_mandate %in% years)]), " TCs in ",
        length(unique(tcs$country[tcs$tc_mandate %in% years])), " countries, of which ",
        sum(!is.na(tcs$tc_begin[tcs$tc_mandate %in% years])), " had begun operating\n", sep = ""
    )
}
HowManyYearsOperated <- function() {
    df <- tcs[tcs$tc_begin %in% 1970:2020, ]
    df$tc_complete[is.na(df$tc_complete)] <- 2020
    df$years <- df$tc_complete - df$tc_begin + 1
    cat("total operating years (1970-2020): ", sum(df$years, na.rm = TRUE), "\n", sep = "")
    cat("average operating years (1970-2020): ", mean(df$years, na.rm = TRUE), "\n", sep = "")
    cat("ongoing in 2020: ", sum(df$ongoing), "\n", sep = "")
}
### some initial descriptives for article introduction
HowManyOperated(1970:2000)
HowManyMandated(2001:2020)
HowManyOperated(1970:2020)
HowManyYearsOperated()

### these are the countries in the sample that did not have TCs:
unique(sample$country)[!unique(sample$country) %in% unique(tcs$country)]
### these are the countries that had TCs but are not in the sample:
unique(tcs$country)[!unique(tcs$country) %in% unique(sample$country)]

### merging in-sample indicator into list of TCs
sample$tcs_sample <- 1
tcs <- merge(sample, tcs, by = "country", all.y = TRUE)
tcs$tcs_sample[is.na(tcs$tcs_sample)] <- 0
tcs <- tcs[order(tcs$country, tcs$tc_begin), ]
### we only consider TC operations until 2015 in the article, so dropping data after that year:
tc_vars <- c("tc_begin", "tc_complete", "tc_report", "tc_report_public", "tc_rec_prosecute", "tc_rec_reform")
tcs[, tc_vars] <- lapply(tcs[, tc_vars], function(col) {
    col[IsTrue(col > 2015)] <- NA
    return(col)
})

### values for Table 2 in article:
TCnumbers(df = tcs, restricted_to_sample = TRUE)
### (this function summarizes the numbers of TCs meeting the coding criteria for our
### explanatory variables;(output requires more formatting for publication version)

### values for Appendix Table 7:
TCnumbers(df = tcs, restricted_to_sample = FALSE)

### only keeping TCs that have actually begun operations
tcs <- tcs[!is.na(tcs$tc_begin), c("country", "tc_begin", "tc_complete", "tcs_sample_first_year")]

### Appendix Table 4:
nrow(tcs)
tcs

### the next lines set up and format the data for Appendix Table 5
### first, we include only TC-years for country-years in the sample
tcs$tc_begin <- ifelse(IsTrue(tcs$tc_begin < tcs$tcs_sample_first_year & tcs$tc_complete >= tcs$tcs_sample_first_year),
                       tcs$tcs_sample_first_year, tcs$tc_begin)
tcs$tc_begin <- ifelse(IsTrue(tcs$tc_complete < tcs$tcs_sample_first_year), NA, tcs$tc_begin)
tcs$tc_complete <- ifelse(IsTrue(tcs$tc_complete < tcs$tcs_sample_first_year), NA, tcs$tc_complete)
### then some cleaning up and formatting
tcs$tc_begin[tcs$tc_begin == tcs$tc_complete] <- NA
tcs$ongoing_tcs <- paste(tcs$tc_begin, tcs$tc_complete, sep = "-")
tcs$ongoing_tcs <- str_replace(tcs$ongoing_tcs, fixed("NA-"), "")
tcs$ongoing_tcs <- str_replace(tcs$ongoing_tcs, fixed("NA"), "")
tcs <- unique(tcs[tcs$ongoing_tcs != "", c("country", "ongoing_tcs")])
tcs <- tcs %>% group_by(country) %>% mutate(ongoing_tcs = paste(ongoing_tcs, collapse = ", ")) %>% unique() %>% as.data.frame()
### merging in-sample TCs into list of sample country-years
sample <- merge(sample[, c("country", "tcs_sample_first_year", "last_year")], tcs, by = "country", all.x = TRUE)
sample$ongoing_tcs[is.na(sample$ongoing_tcs)] <- ""

### Appendix Table 5 (output requires a bit more formatting in 'ongoing_tcs' column):
nrow(sample)
sample



###############
### helpers ###
###############

### the vectors in this section help set up the analyses and automate repetitive tasks
names(outcome_vars) <- outcome_vars <- c("v2xel_frefair", "v2x_frassoc_thick", "v2x_liberal", "v2jureform", "v2x_partip", "latentmean_fariss")
names(repression_vars) <- repression_vars <- c("v2cltort", "v2clkill", "v2csreprss")
names(tc_vars) <- tc_vars <- c("tc_hashadany", "tc_finalreport", "tc_publicreport", "tc_prosecute", "tc_reform")

### as explained in the article, each type of DV requires a different set of covariates
### this is set up in the following vector:
models_to_run <- list(
    v2xel_frefair     = paste("t", c(1, 10:19), sep = ""),
    v2x_frassoc_thick = paste("t", c(1, 10:19), sep = ""),
    v2x_liberal       = paste("t", c(2, 20:29), sep = ""),
    v2jureform        = paste("t", c(2, 20:29), sep = ""),
    v2x_partip        = paste("t", c(3, 30:39), sep = ""),
    latentmean_fariss = paste("t", c(4, 40:49), sep = ""),
    v2cltort          = paste("t", c(4, 40:49), sep = ""),
    v2clkill          = paste("t", c(4, 40:49), sep = ""),
    v2csreprss        = paste("t", c(4, 40:49), sep = "")
)
### (these numbers refers to the model IDs in 'varstable', which lists all
### variables included in each model, and is used to automate the estimations below

outcome_labels <- c(
    v2xel_frefair     = "Clean\nElections",
    v2x_frassoc_thick = "Freedom of\nAssociation",
    v2x_partip        = "Participatory\nComponent",
    v2x_liberal       = "Liberal\nComponent",
    v2jureform        = "Judicial\nReforms",
    latentmean_fariss = "Physical\nIntegrity",
    v2cltort          = "Torture",
    v2clkill          = "Political\nKillings",
    v2csreprss        = "CSO\nRepression"
)
tc_labels <- c(
    tc_hashadany = "TC Operation",
    tc_finalreport_first = "TC Final Report (first)",
    tc_finalreport = "TC Final Report",
    tc_publicreport_first = "TC Public Report (first)",
    tc_publicreport = "TC Public Report",
    tc_recommendations_first = "TC Recommendations (first)",
    tc_recommendations = "TC Recommendations",
    tc_prosecute_first = "TC Prosecution Recommendation (first)",
    tc_prosecute = "TC Prosecution Recommendation",
    tc_reform_first = "TC Reform Recommendation (first)",
    tc_reform = "TC Reform Recommendation"
)
vars_to_put_on_unit_scale <- c("latentmean_fariss", "v2jureform", "v2cltort", "v2clkill", "v2csreprss")



#################################################
### descriptive statistics (Appendix Table 6) ###
#################################################

### creating copy of dataset and applying variable transformations as in the analyses below, except no draws
### for the variables from measurements models, so that the summary descriptives are based on the country-year means
newdf <- Transformations(df = data, latentdraws = FALSE, on_unit_scale = vars_to_put_on_unit_scale)
### subsetting to include only country-year observations in our sample and relevant variables
exclude_vars <- c("country", "year", "region", "dem_bmr", "demtrans_bmr", "tcs_sample_first_year", "tcs_sample",
                  "transition", "demconfl", "demstate", "v2x_polyarchy", "tc_ongoing_sum", "tc_past_sum",
                  "tc_hashadany_all", "TC_reg_density", names(newdf)[str_detect(names(newdf), "firstyear")])
newdf <- newdf[newdf$tcs_sample == 1, !names(newdf) %in% exclude_vars]

### the remaining code in this section calculates and formats descriptive
### statistics for all variables in 'df' using the variable labels in 'varstable'
dtable <- lapply(newdf, function(col) {
    perc_miss <- sum(is.na(col)) / length(col) * 100
    if(is.factor(col)) {
        col <- as.character(col)
        descriptives <- as.data.frame(table(col))
        names(descriptives) <- c("categories", "frequency")
        descriptives$categories <- as.character(descriptives$categories)
        descriptives <- data.frame(
            categories = paste(descriptives$categories, descriptives$frequency, sep = ": "),
            min        = NA,
            max        = NA,
            mean       = NA,
            median     = NA,
            SD         = NA,
            perc_miss  = perc_miss
        )
    } else {
        descriptives <- data.frame(
            categories = NA,
            min        = min(col, na.rm = TRUE),
            max        = max(col, na.rm = TRUE),
            mean       = mean(col, na.rm = TRUE),
            median     = quantile(col, .5, na.rm = TRUE),
            SD         = sd(col, na.rm = TRUE),
            perc_miss  = perc_miss
        )
    }
    return(descriptives)
})
dtable <- data.frame(do.call(rbind, dtable))
vars_to_round <- c("min", "max", "mean", "median", "SD", "perc_miss")
dtable[, vars_to_round] <- round(dtable[, vars_to_round], 2)
vars_replace_na <- c("categories", "min", "max", "mean", "median", "SD", "perc_miss")
dtable[, vars_replace_na] <- lapply(dtable[, vars_replace_na], function(col) {
    col[is.na(col)] <- ""
    return(col)
})
dtable$var <- rownames(dtable)
dtable$var[str_detect(dtable$var, "internal")] <- "internal"
dtable$var[str_detect(dtable$var, "trans_type")] <- "trans_type"
dtable <- merge(varstable[c("var", "label")], dtable, by = "var", all.y = TRUE)
dtable <- dtable[!str_detect(dtable$label, fixed(" (SD)")), ]

### Appendix Table 6:
dtable[, -1]

### for inserting into LaTeX document (this table was further formatted and the variables reordered manually for publication):
title <- paste("Descriptive statistics (", nrow(newdf), " country-year observations, 1970–2016)", sep = "")
latextable <- xtable(dtable[, -1], align =  c("r", "l", "l", rep("r", 6)), caption = title)
names(latextable) <- c("", "categories", "min", "max", "mean", "median", "SD", "\\% missing")

### printing to .tex file
dir.create("tables")
print(latextable,
      sanitize.text.function = identity,
      include.rownames = FALSE,
      table.placement = "!htbp",
      size = "footnotesize",
      floating.environment = "table",
      caption.placement = "top",
      booktabs = TRUE,
      hline.after = c(-1, 0, nrow(dtable)),
      comment = FALSE,
      file = "tables/descriptives.tex"
)



#####################################
### bivariate (Appendix Figure 3) ###
#####################################

### setting seed for replicability because drawing values of measurement model
### variables from the normal distribution requires randomization
# sample(1:1000, 1)
set.seed(732)

### this section runs simple OLS regressions on the imputed datasets for each combination of
### outcome and (lagged) TC variables and aggregates the results to graphically report
### differences-in-means with 95% confidence intervals (saved as .pdf)
zscore <- qnorm(1 - ((1 - 0.95)/2))
bivariate <- lapply(outcome_vars, function(outcome) {
    cat("\n", outcome, ": ", sep = "")
    ### the five multiply imputed datasets let us incorporate the uncertainty due to
    ### imputing missing values of a few variables
    ### in order to incorporate the uncertainty in the measurement models producing
    ### the V-Dem and latent physical integrity measures, we create further multiples of
    ### those datasets in the next line of code, and in the Transformations function below,
    ### draw those variables from the normal distribution with means and standard deviations
    ### provided for each country-year observation by the measurement models
    ### (5 multiply imputed datasets copied 5 times, for a total of 25 datasets for the analyses)
    imputations <- rep(x = imps, each = 5)
    imputations <- lapply(imputations, function(df) {
        ### coding variable transformations in each dataset
        df <- Transformations(df = df, outcome_var = outcome, on_unit_scale = vars_to_put_on_unit_scale, latentdraws = TRUE)
        ### lagging IVs
        vars <- varstable$var[varstable$lag == 1]
        df <- Lagging(df = df, vars = vars)
        ### subset to sample
        df <- subset(df, subset = (tcs_sample == 1))
        return(df)
    })
    result <- lapply(tc_vars, function(tc_var) {
        cat(tc_var, "... ", sep = "")
        fits <- lapply(imputations, function(df) {
            plm(as.formula(paste("outcome ~", tc_var)), data = df, model = "pooling", index = c("country", "year"))
        })
        est <- lapply(fits, function(x) coef(x))
        ses <- lapply(fits, function(x) sqrt(diag(vcov(x))))
        output <- mi.meld(q = data.frame(est), se = data.frame(ses), byrow = FALSE)
        output <- TableMI(output)
        output$cilower <- output$est - zscore * output$se[2]
        output$ciupper <- output$est + zscore * output$se[2]
        return(output[output$var == tc_var, c("est", "se", "cilower", "ciupper")])
    })
    cat("\n")
    do.call(rbind, result)
})

### Appendix Figure 3:
yrange <- range(unlist(bivariate))
dir.create("figures")
pdf("figures/fig3_diff_in_means.pdf", height = 8.5, width = 6.5)
par(mfrow = c(length(tc_vars), length(outcome_vars)), mar = c(0, 3, 2, 0) + 0.1)
lapply(tc_vars, function(tc_var) {
    ### using the same ylimits across rows for comparison, but if maximum detail is desired, can uncomment the next line
    # yrange <- range(unlist(lapply(outcome_vars, function(outcome) { bivariate[[outcome]][tc_var, ] })))
    lapply(outcome_vars, function(outcome) {
        plot(x = 0, y = bivariate[[outcome]][tc_var, "est"], ylim = yrange, xlab = "", ylab = "", xaxt = 'n',
             pch = 18, main = outcome_labels[outcome], cex = 1, cex.main = 0.75, cex.axis = 0.75)
        title(ylab = tc_labels[tc_var], line = 2, cex.lab = 0.75)
        segments(x0 = 0, y0 = bivariate[[outcome]][tc_var, "cilower"], y1 = bivariate[[outcome]][tc_var, "ciupper"], lwd = 2, lend = "butt")
        abline(h = 0, lty = "solid", lwd = 0.33, col = "black")
    })
    invisible(NULL)
})
dev.off()



#################
## estimation ###
#################

dir.create("estimates")

### setting seed for replicability because drawing values of measurement model
### variables from the normal distribution requires randomization
# sample(1:1000, 1)
set.seed(211)

### the code in this section may take a while to run, depending on computing resources
fits <- lapply(c(outcome_vars, repression_vars), function(outcome) {
    models <- models_to_run[[outcome]]
    ### see the explanation for the next line in the bivariate section above
    imputations <- rep(x = imps, each = 5)
    imputations <- lapply(imputations, function(df) {
        ### coding variable transformations in each dataset
        df <- Transformations(df = df, outcome_var = outcome, on_unit_scale = vars_to_put_on_unit_scale)
        ### lagging IVs
        vars <- varstable$var[varstable$lag == 1]
        df <- Lagging(df = df, vars = vars)
        ### coding dummies from categorical variables
        vars <- varstable$var[varstable$type == "category"]
        df <- fastDummies::dummy_cols(df, select_columns = vars, ignore_na = TRUE, remove_selected_columns = TRUE)
        ### subset to sample
        return(subset(df, subset = (tcs_sample == 1)))
    })
    ### running fixed effects and random effects models
    re <- RunModelsMI(imputations = imputations, outcome = outcome, models = models, varstable = varstable,
                      estimator = "random", filenameadd = "random", add_to_formula = paste(outcome, "firstyear", sep = "_"))
    fe <- RunModelsMI(imputations = imputations, outcome = outcome, models = models, varstable = varstable,
                      estimator = "within", filenameadd = "within")
    ### subset to restricted sample
    imputations <- lapply(imputations, function(df) {
        subset(df, subset = (tc_hashadany == 1))
    })
    RunModelsMI(imputations = imputations, outcome = outcome, models = models[-1], varstable = varstable,
                estimator = "random", filenameadd = "random_qual", add_to_formula = paste(outcome, "firstyear", sep = "_"))
    RunModelsMI(imputations = imputations, outcome = outcome, models = models[-1], varstable = varstable,
                estimator = "within", filenameadd = "within_qual")
    invisible(list(fe = fe, re = re))
})

### loading estimates from files into list for producing tables and figures
results <- lapply(c(full = "", qual = "_qual"), function(suffix) {
    lapply(c(outcome_vars, repression_vars), function(outcome) {
        lapply(c(within = "within", random = "random"), function(estimator) {
            file <- paste(outcome, "_estimates_", estimator, suffix, ".RData", sep = "")
            load(paste("estimates/", file, sep = ""), verbose = FALSE)
            return(estimates)
        })
    })
})



##################################
### appendix regression tables ###
##################################
lookup <- c(
    "(Intercept)" = "Intercept",
    nobs = "n observations",
    ncountries = "n countries",
    years = "years",
    estimator = "estimator",
    rsquared = "R-squared"
)

### turning all estimates from above into regression tables for each outcome variable
### (separate tables for analyses of full and restricted samples)
tables <- lapply(results, function(toplevel) {
    lapply(toplevel, function(outcomelevel) {
        estimates <- lapply(1:2, function(index) {
            names(outcomelevel[[index]]) <- paste(names(outcomelevel[[index]]), index, sep = "_")
            return(outcomelevel[[index]])
        })
        estimates <- do.call(c, estimates)
        tf <- lapply(estimates, function(mod) {
            mod_result <- mod[["est"]]
            mod_result$output <- paste(format(round(mod_result$est, 3), 3), " [",
                                       format(round(abs(mod_result$z), 2), 2), "]", sep = "")
            ### if not printing to LaTeX, then comment out next line
            mod_result$output[abs(mod_result$z) >= zscore] <- paste("\\textbf{", mod_result$output[abs(mod_result$z) >= zscore], "}", sep = "")
            info <- data.frame(output = unlist(mod$info))
            info$var <- names(lookup[rownames(info)])
            mod_result <- rbind(mod_result[, c("var", "output")], info)
            return(mod_result)
        })
        tf <- lapply(names(tf), function(modelname) {
            names(tf[[modelname]])[2] <- modelname
            return(tf[[modelname]])
        })
        tf <- Reduce(f = function(...) merge(..., by = "var", all = TRUE), tf)
        varstable$order <- 1:nrow(varstable)
        tf <- merge(varstable[, c("var", "order", "label")], tf, by = "var", all.y = TRUE)
        tf$label[is.na(tf$label)] <- lookup[tf$var[is.na(tf$label)]]
        tf$order[is.na(tf$order)] <- match(tf$var[is.na(tf$order)], names(lookup)) + max(tf$order, na.rm = TRUE)
        tf <- tf[order(tf$order), names(tf) != "order"]
        tf <- tf[, sort(names(tf))]
        tf[is.na(tf)] <- ""
        return(tf)
    })
})

### the models run above include various robustness checks, but we don't show all of these in the article and appendix
### here we create the regression tables for all the results shown in the figures (produced below)
ExtractTables(tables = tables$full,
    dvs = c(outcome_vars, repression_vars),
    models_to_select = lapply(models_to_run, function(mods) mods[c(1, 3, 5, 7)]),
    file_suffix = "tclast"
)
### (this function extracts specific subsets of the regression tables and prints them to individual files, as LaTeX code)
ExtractTables(tables = tables$full,
    dvs = outcome_vars,
    models_to_select = lapply(models_to_run, function(mods) mods[c(2, 4, 8, 10)]),
    file_suffix = "tcfirst"
)
ExtractTables(tables = tables$full,
    dvs = outcome_vars,
    models_to_select = lapply(models_to_run, function(mods) mods[c(9, 11)]),
    file_suffix = "tcrecs"
)
ExtractTables(tables = tables$qual,
    dvs = outcome_vars,
    models_to_select = lapply(models_to_run, function(mods) mods[c(3, 5, 9, 11)]),
    file_suffix = "tcqual"
)



#######################
### results figures ###
#######################

### calculating confidence intervals for results figures
toplot <- lapply(results, function(toplevel) {
    lapply(toplevel, function(outcomelevel) {
        lapply(outcomelevel, function(estimator) {
            lapply(estimator, function(modelfit) {
                modelfit$est$cilower <- modelfit$est$est - zscore * modelfit$est$se
                modelfit$est$ciupper <- modelfit$est$est + zscore * modelfit$est$se
                output <- modelfit$est[str_detect(modelfit$est$var, fixed("tc_")), c("var", "est", "se", "cilower", "ciupper")]
                return(output[nrow(output), ])
            })
        })
    })
})

### Figure 1:
TCsResultsFigure(estimates = toplot$full,
    outcome_vars = outcome_vars,
    models_to_select = lapply(models_to_run, function(mods) mods[c(1, 3, 5, 7)]),
    filename = "fig1_tclast"
)

### Figure 2:
TCsResultsFigure(estimates = toplot$qual,
    outcome_vars = outcome_vars,
    models_to_select = lapply(models_to_run, function(mods) mods[c(3, 5, 9, 11)]),
    yrange_relative = TRUE,
    filename = "fig2_tcqual"
)

### Appendix Figure 4:
TCsResultsFigure(estimates = toplot$full,
    outcome_vars = outcome_vars,
    models_to_select = lapply(models_to_run, function(mods) mods[c(9, 11)]),
    filename = "fig4_tcrecs",
    figure_height = 4
)

### Appendix Figure 5:
TCsResultsFigure(estimates = toplot$full,
    outcome_vars = outcome_vars,
    models_to_select = lapply(models_to_run, function(mods) mods[c(2, 4, 8, 10)]),
    filename = "fig5_tcfirst"
)

### Appendix Figure 6:
TCsResultsFigure(estimates = toplot$full,
    outcome_vars = repression_vars,
    models_to_select = lapply(models_to_run, function(mods) mods[c(1, 3, 5, 7)]),
    filename = "fig6_repression"
)
