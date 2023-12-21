TCsResultsFigure <- function(outcome_vars, estimates, models_to_select, filename,
    figure_height = 8.25,   ## figure dimensions are optimized for inclusion in letter-sized documents
                            ## (default is full page figure) but the figure height can be adjusted here
    yrange_relative = FALSE ## should all sub-figures (across rows) have the same ylimits?
                            ## (within rows, ylimits will always be the same)
) {
    filepath <- paste("figures/", filename, ".pdf", sep = "")
    ### common 'yrange' across all estimates included in the figure (i.e. across rows)
    yrange <- range(unlist(
        lapply(outcome_vars, function(outcome) {
            lapply(estimates[[outcome]], function(estimator) {
                lapply(estimator[models_to_select[[outcome]]], function(model) {
                    model[, c("cilower", "ciupper")]
                })
            })
        })
    ))
    rows <- max(unlist(lapply(outcome_vars, function(outcome) { length(models_to_select[[outcome]]) })))
    offsets <- c(-0.25, 0.25)
    pdf(filepath, height = figure_height, width = 6.5)
    par(mfrow = c(rows, length(outcome_vars)), mar = c(0, 3, 2, 0) + 0.1, oma = c(2, 0, 0, 0) + 0.1)
    nmodels <- unique(unlist(lapply(models_to_select[outcome_vars], length)))
    ### plotting estimates
    lapply(1:nmodels, function(model_number) { # model_number = 1
        if(yrange_relative) {
            ### if TRUE, basing 'yrange' only on within-rows, thereby showing more details
            yrange <- range(unlist(lapply(outcome_vars, function(outcome_var) {
                modelid <- models_to_select[[outcome_var]][model_number]
                lapply(c("random", "within"), function(estimator) {
                    estimates[[outcome_var]][[estimator]][[modelid]][, c("est", "cilower", "ciupper")]
                })
            })))
        }
        adj <- (yrange[2] - yrange[1])/500
        lapply(outcome_vars, function(outcome_var) { # outcome_var = outcome_vars[1]
            modelid <- models_to_select[[outcome_var]][model_number]
            random <- estimates[[outcome_var]][["random"]][[modelid]]
            within <- estimates[[outcome_var]][["within"]][[modelid]]
            plot(x = offsets[1], y = within$est, ylim = yrange, xlim = c(-0.5, 0.5), xlab = "", ylab = "", xaxt = 'n',
                 pch = 18, main = outcome_labels[outcome_var], cex = 0.75, cex.main = 0.75, cex.axis = 0.75)
            title(ylab = tc_labels[within$var], line = 2, cex.lab = 0.75)
            points(x = offsets[2], y = random$est, pch = 18, cex = 0.75)
            segments(x0 = offsets[1], y0 = within$cilower, y1 = within$ciupper, lwd = 2, lend = "butt")
            segments(x0 = offsets[2], y0 = random$cilower, y1 = random$ciupper, lwd = 2, lend = "butt")
            segments(x0 = offsets[2], y0 = random$cilower + adj, y1 = random$ciupper - adj, lwd = 0.75, lend = "butt", col = "white")
            abline(h = 0, lty = "solid", lwd = 0.25, col = "black")
        })
    })
    ### legend
    legendx <- c(0.1, 0.55)
    par(fig = c(0, 1, 0, par("omd")[3]), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    frame()
    plot(legendx, c(0.5, 0.5), pch = 18, ylim = c(0, 1), xlim = c(0, 1), cex = 0.75, bty = "n", xaxt = "n", yaxt = "n")
    segments(x0 = legendx, y0 = c(0.25, 0.25), y1 = c(0.75, 0.75), lwd = 2, lend = "butt")
    segments(x0 = legendx[2], y0 = 0.25 + 1/50, y1 = 0.75 - 1/50, lwd = 0.75, lend = "butt", col = "white")
    text(x = legendx[1], y = 0.5, pos = 4, labels = "estimate and 95% confidence interval from fixed effects model", cex = 0.75)
    text(x = legendx[2], y = 0.5, pos = 4, labels = "estimate and 95% confidence interval from random effects model", cex = 0.75)
    dev.off()
    invisible(NULL)
}
