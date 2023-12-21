### this convenience function extracts the output from Amelia's 'mi.meld()' function
TableMI <- function(mi.estimates) {
    tab <- data.frame(est = t(mi.estimates$q.mi), se = t(mi.estimates$se.mi))
    tab$var <- row.names(tab)
    row.names(tab) <- 1:nrow(tab)
    tab$z <- tab$est/tab$se
    return(tab[, c("var", "est", "se", "z")])
}
