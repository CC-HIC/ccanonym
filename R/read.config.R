#' read.config
#' @param ccd the identifiable ccRecord object
#' @param config yaml file location
#' @export create.sdc
#' @import data.table
create.sdc <- function(ccd, path, remove.alive=T, verbose=F) {
    demg <- data.table(suppressWarnings(sql.demographic.table(ccd)))

    if (remove.alive)
        demg <- demg[DIS=="D"]

    conf <- yaml.load_file(path)
    keyv <- sapply(conf$keyVars, names)
    numv <- sapply(conf$numVars, names)
    datetimev <- sapply(conf$keyDateTime, names)

    numv <- c(numv, datetimev)
    demg <- data.frame(convert.numeric.datetime(demg, datetimev))
    
    # check the data type and avoid processing on the all NA columns.
    new.numv <- vector()
    for (i in seq(numv)) {
        if (!is.numeric(demg[[numv[i]]])) {
            stop(paste(numv[i], "has to be in numeric type."))
        }
        if (!all(is.na(demg[[numv[i]]])) & 
            length(unique(demg[[numv[[i]]]]) != 1))
            new.numv <- c(new.numv, numv[i])
    }
    numv <- new.numv
    for (i in numv)
    print(demg[[i]])
    print(numv)

    return(createSdcObj(demg, 
                 keyVars=keyv,
                 numVars=numv[1:3],
                 sensibleVar=conf$sensibleVar))
}



get.age <- function(demg) {

}


#' @export remove.direct
remove.direct <- function(data, path) {
    conf <- yaml.load_file(path)
    dirvars <- sapply(conf$directVars, names)
    for (i in dirvars) 
        data[[i]] <- NULL
    data 
}


#' @export convert.numeric.datetime
convert.numeric.datetime<- function(data, items=NULL) {
    if (is.null(items))
        items <- names(data)

    hic.code <- stname2code(items)

    for (i in items) {
        dtype <- ccdata:::ITEM_REF[[stname2code(i)]]$Datatype
        if (dtype == "date") 
            data[[i]] <- as.numeric(as.POSIXct(data[[i]], format="%Y-%m-%d"))
        if (dtype == "time") 
            data[[i]] <- as.numeric(as.POSIXct(data[[i]], format="%H:%M"))
        if (dtype == "date/time") 
            data[[i]] <- as.numeric(as.POSIXct(data[[i]], format="%Y-%m-%d %H:%M")) 
    }
    data
}
     
#' @export convert.back.datetime
convert.back.datetime <- function(data, items=NULL) {

    if (is.null(items))
        items <- names(data)
    hic.code <- stname2code(items)

    for (i in items) {
        dtype <- ccdata:::ITEM_REF[[stname2code(i)]]$Datatype
        if (dtype == "date") {
            data[[i]] <- as.POSIXct(data[[i]], origin="1970-01-01")
            data[[i]] <- as.character(data[[i]])
        }

        if (dtype == "time") {
            data[[i]] <- as.POSIXct(data[[i]], origin="1970-01-01")
            data[[i]] <- strftime(data[[i]], format="%H:%M")
        }

        if (dtype == "date/time") {
            data[[i]] <- as.POSIXct(data[[i]], origin="1970-01-01")
            data[[i]] <- strftime(data[[i]], format="%Y-%m-%d %H:%M")
        }

    }
    data
}


#' @export apply.dataset 
apply.dataset <- function(ccd, demgsdc) {


}


security.check <- function(ccd) {

}



anonymisation <- function(ccd) {

    security.check(ccd)

}
