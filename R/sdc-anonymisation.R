#' Anonymise the critical care dataset
#'
#' This function is the sf
#' 
#'
#' 
#' @param ccd identifiable data set in ccRecord format (see. ccdata R package)
#' @param conf yaml configuration which either characters of the path of yaml
#'        file or the parsed list.
#' @param remove.alive logical value determines whether removing all alive
#'        episode.
#' @param verbose
#' @return ccRecord 
#'
#' @export
anonymisation <- function(ccd, conf, remove.alive=T, verbose=F, ...) {
    nv <- variables.name(conf)
    ccd <- deltaTime(ccd, ...)
    sdc <- do.sdc(ccd, conf, remove.alive, verbose)
    newccd <- create.anonym.ccd(ccd, sdc$data)
    security.check(newccd, nv$dirv)
    newccd
}

#' do.sdc
#' 
#' @param ccd the identifiable ccRecord object
#' @param config yaml file location
#' @export do.sdc
#' @import data.table
do.sdc <- function(ccd, conf, remove.alive=T, verbose=F) {
    

    if (is.character(conf))
        conf <- yaml.load_file(conf)

    demg <- data.table(suppressWarnings(sql.demographic.table(ccd)))
    demg <- append.age(demg)
    demg$index <- seq(nrow(demg))

    # Remove dead episodes 
    if (remove.alive)
        demg <- demg[DIS=="D"]

    # Read configuration 
    vn <- variables.name(conf)
    vn$numv <- c(vn$numv, vn$datetimev)
    demg <- data.frame(convert.numeric.datetime(demg, vn$datetimev))
    vn$numv <- non.unique.columns(demg, vn$numv)


    # Remove direct identifiable variables 
    demg <- remove.direct.vars(demg, vn$dirv)


    sdc <- createSdcObj(demg, keyVars=vn$keyv, numVars=vn$numv,
                        sensibleVar=vn$sensv)

    # Do the SDC processes. 
    sdc <- localSuppression(sdc)
    sdc <- do.sdc.numvar(sdc, conf, vn$numv)

    if (verbose)
        print(sdc)

    demg[, vn$numv] <- sdc@manipNumVars
    demg[, vn$keyv] <- sdc@manipKeyVars
    demg <- convert.back.datetime(demg, vn$datetimev)

    return(list(data=demg, sdc=sdc))
}


remove.direct.vars <- function(data, dirvs) {
    for (i in dirvs) 
        data[[i]] <- NULL
    data 
}

append.age <- function(demg) {
    format.dob <- "%Y-%m-%d"
    format.dah <- "%Y-%m-%d"
    demg$AGE <- 
        floor(as.numeric(difftime(as.POSIXct(demg$DAH, format=format.dah),
                                  as.POSIXct(demg$DOB, format=format.dob), 
                                  units="days"))/365)
    return(demg)
}

create.anonym.ccd <- function(ccd, sdc.data) {
    new.record <- ccRecord()

    for (i in seq(nrow(sdc.data))) {
        cdl <- clinic.data.list(ccd, sdc.data$index[i])
        sdcl <- sdc.row2list(sdc.data[i, ])
        new.record <- new.record + new.episode(append(cdl, sdcl))
    }
    new.record
}

do.sdc.numvar <- function(sdc, conf, numv) {
    numconf <- append(conf$numVars, conf$datetimeVars)
    for (item in numv) {
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

#' Parse the confiuration file and check the variable names. 
variables.name <- function(conf) {
    if (is.character(conf))
        conf <- yaml.load_file(conf)

    dirv <- conf$directVars 
    keyv <- conf$keyVars
    numv <- names(conf$numVars)
    datetimev <- names(conf$datetimeVars)
    sensv <- conf$sensVar
    nonidv <- conf$nonidentifyVars
    all.vars <- c(keyv, numv, datetimev, sensv)

    # check the correctness of the configuration file. 
    if (any(all.vars %in% nonidv)) {
        print(all.vars[all.vars %in% nonidv])
        stop("identifiable variables appeared in non-identifiable slot, check the yaml file!" )
    
    }

    confvars <- c(all.vars, nonidv)
    vars.in.index <- code2stname(names(ccdata:::ITEM_REF)) %in% confvars
    if (!all(vars.in.index)){
print(
short2longname((as.character(code2stname(names(ccdata:::ITEM_REF))[!vars.in.index]))))
    
    }
    

    return(list(dirv=dirv, keyv=keyv, numv=numv, 
                datetimev=datetimev, sensv=sensv, 
                all.vars=all.vars))
}

sdc.row2list <- function(sdcrow) {
    lst <- as.list(sdcrow)
    lst <- lapply(lst, as.character) 
    lst <- lapply(lst, 
                  function(x) {
                      if (is.na(x)) 
                          return("NULL")
                      else
                          return(x)
                  })
    lst[['index']] <- NULL
    names(lst) <- stname2code(names(lst))
    for (i in names(lst)) {
        if (lst[[i]] == "NULL")
        lst[[i]] <- NULL
    }
    lst
}

clinic.data.list <- function(ccd, index) {
    cdl  <- lapply(ccd@episodes[[index]]@data, 
                           function(x) {
                               if (length(x) > 1)
                                   return(x)
                               else 
                                   return(NULL)
                           })
    for (i in names(cdl)) {
        if (is.null(cdl[[i]]))
            cdl[[i]] <- NULL
    }
    cdl
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

security.check <- function(ccd, dirv) {
    ccdata:::for_each_episode(ccd, 
        function(x) {
            if (any(dirv %in% names(x@data)))
                stop("direct identifiable items appeared in the final data!!!")
        })

}
