#' @export raicu.breakdown
raicu.breakdown <- function(r, digits=3) {

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
    cols <- cols[, 1:digits]

    combine <- function(x) {
        x <- x[!is.na(x)]
        paste(x, collapse=".")
    }
    if (class(cols) == "numeric") 
        return(combine(cols))
    else
        return(apply(cols, 1, combine))
}
