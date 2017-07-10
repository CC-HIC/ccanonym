#' @export convert.numeric.datetime
convert.numeric.datetime<- function(data, items=NULL) {
    if (is.null(items))
        items <- names(data)
    
    hic.code <- stname2code(items)

    for (i in items) {
        dtype <- cleanEHR:::ITEM_REF[[stname2code(i)]]$Datatype
        if (!is.null(dtype)) {
            if (dtype == "date") 
                data[[i]] <- as.numeric(as.POSIXct(data[[i]], format="%Y-%m-%d"))
            if (dtype == "time") 
                data[[i]] <- as.numeric(as.POSIXct(data[[i]], format="%H:%M"))
            if (dtype == "date/time") 
                data[[i]] <- as.numeric(as.POSIXct(data[[i]],
                                                   format="%Y-%m-%dT%H:%M:%S")) 
        }
    }
    data
}

#' @export convert.back.datetime
convert.back.datetime <- function(data, items=NULL) {
    if (is.null(items))
        items <- names(data)
    hic.code <- stname2code(items)

    for (i in items) {
        dtype <- cleanEHR:::ITEM_REF[[stname2code(i)]]$Datatype
        numdate <- as.numeric(data[[i]])

        if (!is.null(dtype)) {
            if (dtype == "date") {
                data[[i]] <- as.POSIXct(numdate, origin="1970-01-01")
                data[[i]] <- as.character(data[[i]])
            }

            if (dtype == "time") {
                data[[i]] <- as.POSIXct(numdate, origin="1970-01-01")
                data[[i]] <- strftime(data[[i]], format="%H:%M")
            }

            if (dtype == "date/time") {
                data[[i]] <- as.POSIXct(numdate, origin="1970-01-01")
                data[[i]] <- strftime(data[[i]], format="%Y-%m-%dT%H:%M:%S")
            }
        }
    }
    data
}
