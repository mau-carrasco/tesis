### this function extracts specific subsets of the regression 'tables' for each 'dv'
### and prints them as LaTeX code to individual .tex files
ExtractTables <- function(tables, dvs, models_to_select,
    file_suffix = NULL ## what to append to default filename
) {
    output <- lapply(dvs, function(dv) {
        ### selecting relevant columns
        columns <- sort(c("label", paste(models_to_select[[dv]], 1, sep = "_"), paste(models_to_select[[dv]], 2, sep = "_")))
        tf <- tables[[dv]][, columns]
        ### selecting relevant rows
        rows <- rowSums(tf[, -1] == "") != ncol(tf[, -1])
        tf <- tf[rows, ]
        ### changing column names
        names(tf)[-1] <- unlist(lapply(1:(ncol(tf[, -1])/2), function(num) {
            paste("Model ", num, c("a", "b"), sep = "")
        }))
        names(tf) <- str_replace(names(tf), "label", "")
        ### formatting and printing to file
        latextable <- xtable(tf, align =  c("r", "l", rep("r", ncol(tf) - 1)),
                             caption = "estimates [t-statistic]; significant at 0.05 alpha level in \\textbf{bold}")
        file_suffix <- ifelse(!is.null(file_suffix), paste("_", file_suffix, sep = ""), "")
        filename <- paste("tables/reg_", dv, file_suffix, ".tex", sep = "")
        print(latextable,
              sanitize.text.function = identity,
              include.rownames = FALSE,
              table.placement = "!hb",
              size = "footnotesize",
              floating.environment = "table",
              booktabs = TRUE,
              hline.after = c(-1, 0, nrow(tf) - 5, nrow(tf)),
              comment = FALSE,
              file = filename
        )
        return(tf)
    })
    invisible(output)
}
