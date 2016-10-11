#' read.config
#' @param data the input confidential data in data.frame or data.table
#' @param config yaml file location
#' @export create.sdc
create.sdc <- function(data, path) {

# NOTE:  configuration security check is required. 

    conf <- yaml.load_file(path)
    keyv <- sapply(conf$keyVars, names)
    numv <- sapply(conf$numVars, names)

    if (length(numv) == 0 ) numv <- NULL
    return(createSdcObj(data, 
                 keyVars=keyv,
                 numVars=numv,
                 sensibleVar=conf$sensibleVar))
}



date2numeric <- function(dt, datecol) {
    for (i in datecol)
        dt[[i]] <- dt
        

}


remove.direct <- function(data, dirvar) {}



convert.numeric.datetime<- function(data) {
    items <- names(data)
    hic.code <- stname2code(items)

    for (i in items) {
        dtype <- ccdata:::ITEM_REF[[stname2code(i)]]$Datatype
        if (dtype == "date") {
            data[[i]] <- as.numeric(as.POSIXct(data[[i]], format="%Y-%m-%d",
                                               origin="1970-01-01"))
        }

        if (dtype == "time") {
            data[[i]] <- as.numeric(as.POSIXct(data[[i]], format="%H:%M:%S", 
                                               origin="1970-01-01"))
        }

        if (dtype == "date/time") {
            data[[i]] <- as.numeric(as.POSIXct(data[[i]]), 
                                    origin="1970-01-01")
        }
    }
    data
}
     
    
convert.back.datetime <- function(data) {


}

apply.dataset <- function() {
}

#as.POSIXct(
#           as.numeric(
#                      as.POSIXct(
#                                 
#    c('07:51:00', '01:00:00', "NULL", ""),
#    format='%H:%M:% origin="1970-01-01")
