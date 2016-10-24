#' read.config
#' @param ccd the identifiable ccRecord object
#' @param config yaml file location
#' @export do.sdc
#' @import data.table
do.sdc <- function(ccd, path, remove.alive=T, verbose=F) {
    demg <- data.table(suppressWarnings(sql.demographic.table(ccd)))

    if (remove.alive)
        demg <- demg[DIS=="D"]

    conf <- yaml.load_file(path)

    keyv <- conf$keyVars
    numv <- names(conf$numVars)
    datetimev <- names(conf$datetimeVars)
    sensv <- conf$sensibleVar

    numv <- c(numv, datetimev)
    demg <- data.frame(convert.numeric.datetime(demg, datetimev))
    numv <- non.unique.columns(demg, numv)

    sdc <- createSdcObj(demg, keyVars=keyv, numVars=numv,
                        sensibleVar=conf$sensibleVar)

    sdc <- localSuppression(sdc)
    sdc <- sdc.numvar(sdc, conf, numv)

    if (verbose)
        print(sdc)

    demg[, numv] <- sdc@manipNumVars
    demg[, keyv] <- sdc@manipKeyVars
    demg <- convert.back.datetime(demg, numv)

    return(list(data=demg, sdc=sdc))
}



sdc.numvar <- function(sdc, conf, numv) {
    numconf <- append(conf$numVars, conf$datetimeVars)
    for (item in numv) {
        print(item)
        operations  <- names(numconf[[item]])
        for (op in operations) {
            FUN <- eval(parse(text=op))
            args <- 
                append(numconf[[item]][[op]], list(obj=sdc, variables=item))
            sdc <- do.call(FUN, args)
        }
    }
    sdc
}


non.unique.columns <- function(data, numv) {
    # check the data type and avoid processing on the 
    # all NA columns.
    new.numv <- vector()
    for (i in seq(numv)) {
        if (!is.numeric(data[[numv[i]]])) {
            stop(paste(numv[i], class(data[[numv[i]]]), "has to be a numeric vector."))
        }
        unidata <- unique(data[[numv[i]]])
        if (NA %in% unidata)
            unidata <- unidata[-which(is.na(unidata))]
        if (length(unidata) > 1)
            new.numv <- c(new.numv, numv[i])
    }

    if (length(new.numv) == 0)
        return(NULL)
    else
        return(new.numv)
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
