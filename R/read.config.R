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

    keyv <- conf$keyVars
    numv <- names(conf$numVars)
    datetimev <- names(conf$datetimeVars)
    sensv <- conf$sensibleVar

    numv <- c(numv, datetimev)
    demg <- data.frame(convert.numeric.datetime(demg, datetimev))
    numv <- remove.all.null.numvar(demg, numv)
#    demg <- fill.na.numvar(demg, numv)
    print(demg[, numv])

    sdc <- createSdcObj(demg, keyVars=keyv, numVars=numv,
                        sensibleVar=conf$sensibleVar)

    sdc <- localSuppression(sdc)
    sdc <- sdc.numvar(sdc, conf, numv)

    if (verbose)
        print(sdc)

    demg[, numv] <- sdc@manipNumVars
    demg[, keyv] <- sdc@manipKeyVars
    demg <- convert.back.datetime(demg, numv)

    return(demg)
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

#' @description
#' sdcMicro package failed to create the sdc object with numerical vector where NAs are 
#' presented. To overcome this problem, we fill the NAs
#' with the mean value of the vector and restore NAs by the end of the whole
#' process. 
#' @param numdata is a data.frame 
#' @return a data.frame where the NAs in the columns are filled with the mean
#' value. 
fill.na.numvar <- function(data, numv) {
    data[, numv]<- 
        apply(data[, numv], 2, 
              function(x) {
                  x[is.na(x)] <- mean(x, na.rm=T)
                  return(x)
              })
    data
}



na.index <- function(data){
    apply(data, 2,
          function(x) {
              x[!is.na(x)] <- 1
              x
          })
}
remove.all.null.numvar <- function(data, numv) {
    # check the data type and avoid processing on the 
    # all NA columns.
    new.numv <- vector()
    for (i in seq(numv)) {
        if (!is.numeric(data[[numv[i]]])) {
            stop(paste(numv[i], "has to be in numeric type."))
        }
        if (!all(is.na(data[[numv[i]]])) & 
            length(unique(data[[numv[[i]]]]) != 1))
            new.numv <- c(new.numv, numv[i])
    }
    new.numv
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
