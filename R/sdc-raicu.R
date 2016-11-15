raicu.breakdown <- function(r) {
    if (ncol(r) != 1)
        stop("r has to be 1-column data.frame.")

    rname <- names(r)
    r <- as.character(r[[rname]])
    cols <- strsplit(r, '[.]')
    cols <- lapply(cols, 
           function(x) {
               x <- tryCatch(as.numeric(x), 
                             warning=function(w) {
                                 return(NA)
                             })
               if (length(x) < 5)
                   x <- c(x, rep(NA, 5-length(x)))
               if (length(x) > 5)
                   x <- x[1:5]
               x
           })
    
    cols <- t(data.frame(cols))
    rownames(cols) <- seq(nrow(cols))
    colnames(cols) <- paste(rname, as.roman(seq(5)), sep=".")
    
    cols

}


#' @export raicu.to.category
raicu.to.category <- function(demg, keyvars) {
    demg <- data.frame(demg)
    raicu_vars <- c("RAICU1", "RAICU2", "URAICU")
    keyvars <- sapply(strsplit(keyvars, "[.]"), function(x) x[1])
    vars <- raicu_vars[raicu_vars %in% keyvars]

    for (i in vars) {
        demg <- cbind(demg, raicu.breakdown(demg[i]))
        demg[i] <- NULL
    }
    
    return(demg)
}
