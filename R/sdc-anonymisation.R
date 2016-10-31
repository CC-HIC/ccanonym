#' Anonymise the critical care dataset
#'
#'The following process will be followed for each data release.
#'    1.  The data request will be reviewed and approved as per the Data Access
#'request SOP. The request must include a list of fields, and a time period for
#'the data.
#'
#'    2.  The fields will be compared to the master list of direct identifiers,
#'key variables, and sensitive variables (see Appendix [b] List of fields and
#'anonymisation approach [Page 14]).
#'
#'    3.  A k-anonymity threshold of at least twenty will be applied as an
#'initial default (meaning that the smallest group that could be re-identified
#'using the key variables to 'triangulate' would contain twenty individuals).*1*
#'
#'    4.  An anonymisation configuration file will be submitted that describes
#'the desired data fidelity (e.g. that age is reported rounded to the nearest
#'five years, that the day of hospital admission is known but the month and year
#'suppressed etc.).
#'
#'    5.  The data requested will be processed as per Section [3] Anonymisation
#'methodology - page [6].
#'
#'    6.  If the k-anonymity threshold is not met (either directly or because of
#'inadequate l-diversity in sensitive fields) after processing then those seeking
#'access to the data will be invited to discuss their configuration requests.
#'Guidance will be offered as to which fields might be further aggregated or
#'suppressed.
#'
#'    7.  Once the k-anonymity and l-diversity criteria have been met then the
#'data can be made available for release. 
#'
#'    8.  An audit trial of the data release will be created containing the
#'following information.  • Date and time of data processing • Unique reference
#'to the source data • Code reference of anonymisation package (git commit ID) •
#'Code reference of the configuration file for the anonymisation • Personal
#'details of the data user • Personal details of the data controller (or
#'designated nominee) who is approving the data release *1*This is the mininum
#'size, but the typical group size would be larger. Now consider two
#'re-identification scenarios: the prosecutor and the journalist. In the former,
#'an intruder seeks to re-identifiy a specific individual. Here the average group
#'size is more relevant. In the latter, an intruder (the journalist) simply wants
#'to show that the anonymisation has failed and they have re-identified an
#'arbitrary individual. Here the minimum group size is more important.  
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
anonymisation <- function(ccd, conf, remove.alive=T, verbose=F, 
                          k.anon=20, l.div=10, ...) {
    vn <- anony.var(conf)
    ccd <- deltaTime(ccd, ...)
    sdc <- sdc.trial(ccd, conf, remove.alive, verbose, k=k.anon, l=l.div)
    newccd <- create.anonym.ccd(ccd, sdc$data)
    security.check(newccd, vn$dirv)
    newccd
}

#' sdc.trial
#' 
#' @param ccd the identifiable ccRecord object
#' @param config yaml file location
#' @import data.table
#' @export
sdc.trial <- function(ccd, conf, remove.alive=T, verbose=F, k.anon=20, l.div=10) {

    if (is.character(conf))
        conf <- yaml.load_file(conf)

    vn <- anony.var(conf)

    if (verbose)  cat("parsing the ccdata object ...\n")

    demg <- data.table(suppressWarnings(sql.demographic.table(ccd)))


    demg <- append.age(demg)
    demg$index <- seq(nrow(demg))

    # Remove dead episodes 
    if (remove.alive)
        demg <- demg[DIS=="D"]

    demg <- custom.operation(demg, conf)
    # Remove direct identifiable variables 
    demg <- remove.direct.vars(demg, vn$dirv)
    demg <- microaggregation.numvar(demg, conf)

    sdc <- createSdcObj(demg, keyVars=c(vn$ctgrv, vn$numv, vn$datetimev))
    sdc <- localSuppression(sdc, k=k.anon)

    demg[, sdc@keyVars] <- sdc@manipKeyVars

    if (verbose) cat("adding noise ...\n")
    demg <- addnoise.numvar(demg, conf)

    if (verbose)   cat("measuring l-diversity...\n")
    if (verbose)   cat("=====================\n")
    for (i in vn$sensv) {
        ld <- ldiversity(sdc, i)@risk$ldiversity
        if (max(ld) < l.div) { 
            cat("[x] ", i,"=", max(ld), 
                " does not comply with the l-diversity ", l.div, "\n")
        }
    }

    if (verbose)   cat("=====================\n")
    if (verbose) print(sdc)

    return(list(data=demg, sdc=sdc))
}


custom.operation <- function(demg, conf) {
    ops <- conf$Operations
    for (item in names(ops)) {
        func <- eval(parse(text=ops[item]))
        demg[[item]] <- func(demg[[item]])
    }
    return(demg)
}




addnoise.numvar <- function(demg, conf) {
    vn <- anony.var(conf)
    vn$numv <- c(vn$numv, vn$datetimev)
    demg <- data.frame(convert.numeric.datetime(demg, vn$datetimev))
    numconf <- append(conf$numVars, conf$datetimeVars)
    numv <- non.unique.columns(demg, vn$numv)
    for (item in numv) {
        if (!is.null(numconf[[item]][['noise']])) {
            noise.level <- numconf[[item]][['noise']]
            demg[, item] <- 
                addNoise(data.frame(demg[, item]), 
                         noise=noise.level)$mx
        }
    }
    demg <- convert.back.datetime(demg, vn$datetimev)
    demg
}

microaggregation.numvar <- function(demg, conf) {

    vn <- anony.var(conf)
    vn$numv <- c(vn$numv, vn$datetimev)
    demg <- data.frame(convert.numeric.datetime(demg, vn$datetimev))

    numconf <- append(conf$numVars, conf$datetimeVars)
    numv <- non.unique.columns(demg, vn$numv)

    for (item in numv) {
        if (!is.null(numconf[[item]][['aggr']])) {
            agg.level <- numconf[[item]][['aggr']]
            demg[, item] <- 
                microaggregation(data.frame(demg[, item]), 
                                 aggr=agg.level)$mx
        }
    }
    demg <- convert.back.datetime(demg, vn$datetimev)
    demg
}

remove.direct.vars <- function(data, dirvs) {
    for (i in dirvs) 
        data[[i]] <- NULL
    data 
}

append.age <- function(demg) {
    format.dob <- "%Y-%m-%d"
    format.dah <- "%Y-%m-%d"
    demg$AGE <- difftime(as.POSIXct(demg$DAH, format=format.dah),
                         as.POSIXct(demg$DOB, format=format.dob), 
                         units="days")/365
    demg$AGE <- as.numeric(floor(as.numeric(demg$AGE)))
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



#' Parse YAML configuration file
#' 
#' Parse the YAML configuration file and check the anony.var. 
#' @param conf either the path of YAML configuration of a list
#' @return a list contrains all the anony.var broken down in each
#' category.
#' @export 
anony.var <- function(conf) {
    if (is.character(conf))
        conf <- yaml.load_file(conf)

    dirv <- conf$directVars 
    ctgrv <- conf$categoryVars
    numv <- names(conf$numVars)
    datetimev <- names(conf$datetimeVars)
    sensv <- conf$sensVar
    nonidv <- conf$nonidentifyVars
    all.vars <- c(ctgrv, numv, datetimev, sensv)

    all.ccd.stname <- c(code2stname(names(ccdata:::ITEM_REF)), "AGE")

    # check the correctness of the configuration file. 
    index <- c(all.vars, nonidv) %in% all.ccd.stname
    if (!all(index)) {
        print(c(all.vars, nonidv)[!index])
        stop("Items in configuration file do not appear in ccdata item list.")
    }

    if (any(all.vars %in% nonidv)) {
        print(all.vars[all.vars %in% nonidv])
        stop("identifiable variables appeared in non-identifiable slot, check the yaml file!" )
    }

    confvars <- c(dirv, all.vars, nonidv)
    index <- all.ccd.stname %in% confvars
    if (!all(index)){
        ss <- as.character(all.ccd.stname[!index])
        cat(paste("-", ss, "# ", short2longname(ss), "\n"))
        stop("Missing items in the configuration.")
    }

    return(list(dirv=dirv, ctgrv=ctgrv, numv=numv, 
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
