#' Anonymise the critical care dataset
#'
#' This is the main function of the ccanonym package. It creates the anonymised
#' data abstract from the identifiable dataset through the anonymisation
#' process. 
#'
#' @details
#'1. Remove the alive episode if \code{remove.alive} variable is TRUE.
#'
#'2. Calculate the age based on the date of birth (DOB) and date of ICU admission
#'(DAICU) and date of admission will be removed subsequently. The Removal of
#'DAICU should be specified in the configuration file. 
#'
#'3. All demographic time stamps will be converted based on their difference
#'between the date of admission and date of admission will be converted to an
#'arbitrary time 1970-01-01. i.e. admission date: 2014-01-01 -> 1970-01-01;
#'discharge date: 2014-01-03 -> 1970-01-03. With this process, all the time
#'information will be hidden from the users. However the cadence of such of length
#'of stay will still be preserved. 
#'
#'4. Remove VIPs from a list file which has the identifiers of VIPs. The
#' identifiers can be NHS number or PAS number or site episode id combination
#' (Q70:000001). 
#'
#'5. Remove episodes which stays longer than a certain period of time. One can
#'specify it in the configuration file, e.g. maxStay: 30. 
#'
#'6. Micro-aggregate the numerical/date variables specified in the configuration
#'file. 
#'
#'7. Special aggregation, such as we suppress the post code in such a way that
#'NW1 1AA -> NW1. The function can be written in the configuration file. 
#'
#'8. Suppress the key variables where the k-anonymity is violated. 
#'
#'9. Suppress the sensitive variables where the l-diversity is violated. 
#'
#'10. Adding noise to the selected data. 
#' 
#'11. Combine and create the new ccRecord object and convert all the 2d date
#'time stamps to the hour difference to the admission time. 
#' 
#' @param ccd identifiable data set in ccRecord format (see. cleanEHR R package)
#' @param conf YAML configuration which can be either path of the YAML
#'        file or a configuration list equivalent to the YAML configuration. 
#' @param remove.alive logical value determines whether to remove all alive
#'        episodes.
#' @param verbose logical 
#' @param k.anon integer the K-anonymity
#' @param l.div integer the minimum L-diversity of the data extract. For
#'        entries where the L-diversity is above this threshold will be suppressed. 
#' @return ccRecord 
#'
#' @examples
#' \dontrun{
#' # We assume the original dataset is called `ccd`
#' # Create a template configuration file, modify it if necessary. 
#' template.conf("test.yaml")
#' 
#' # Trial: adjust K-anonymity and L-diversity. 
#' sdc-trial(ccd, "test.yaml", k.anon=10, l.div=2)
#' 
#' # Create the data extract after the k.anon and l.div is decided. 
#' ccd.anon <- anonymisation(ccd, "test.yaml", k.anon=10)
#' }
#' @import sdcMicro
#' @import yaml
#' @import cleanEHR
#' @export
anonymisation <- function(ccd, conf, remove.alive=T, verbose=F, 
                          k.anon=5, l.div=NULL, ...) {
    vn <- parse.conf(conf)
    ccd <- deltaTime(ccd, ...)
    sdc <- sdc.trial(ccd, conf, remove.alive, verbose, k=k.anon, l=l.div)
    newccd <- create.anonym.ccd(ccd, sdc)
    security.check(newccd, vn$dirv)
    newccd
}

#' Performe statstical disclosure control process on demgraphic data.
#'
#' The sdc.trial is a function called by \code{anonymisation()}. However it is
#' also a very useful function for tuning the variables, such as k and l, when the
#' data is new. This function will return the anonymised demographic data and a
#' SDC object which tells more about the entire anonymisation process, such as
#' individual risks, information lost and so on. The user should balance the
#' security and usefulness based on data anonymisation SOP. 
#'
#' @details 
#' 
#'1. Remove the alive episode if \code{remove.alive} variable is TRUE.
#'
#'2. Calculate the age based on the date of birth (DOB) and date of ICU admission
#'(DAICU) and date of admission will be removed subsequently. The Removal of
#'DAICU should be specified in the configuration file. 
#'
#'3. All demographic time stamps will be converted based on their difference
#'between the date of admission and date of admission will be converted to an
#'arbitrary time 1970-01-01. i.e. admission date: 2014-01-01 -> 1970-01-01;
#'discharge date: 2014-01-03 -> 1970-01-03. With this process, all the time
#'information will be hidden from the users. However the cadence of such of length
#'of stay will still be preserved. 
#'
#'4. Remove episodes which stays longer than a certain period of time. One can
#'specify it in the configuration file, e.g. maxStay: 30. 
#'
#'5. Micro-aggregate the numerical/date variables specified in the configuration
#'file. 
#'
#'6. Special aggregation, such as we suppress the post code in such a way that
#'NW1 1AA -> NW1. The function can be written in the configuration file. 
#'
#'7. Suppress the key variables where the k-anonymity is violated. 
#'
#'8. Suppress the sensitive variables where the l-diversity is violated. 
#'
#'9. Adding noise to the selected data. 
#' @param ccd the identifiable ccRecord object
#' @param conf the YAML file location or a list equivalent to the YAML file. 
#' @param remove.alive logical whether remove all non-dead episodes. 
#' @param verbose logical showing more or less information. 
#' @param k.anon minimum K-anonymity threshold.
#' @param l.div minimum l-diversity threshold.
#' @return sdc a list contains the parsed data and the sdcMicro object where
#'         the detailed individual risks can be checked.
#' @examples
#' \dontrun{
#' # We assume the original dataset is called `ccd`
#'
#' # Create a template configuration file, modify it if necessary. 
#' template.conf("test.yaml")
#' 
#' # Trial: adjust K-anonymity and L-diversity. 
#' sdc <- sdc-trial(ccd, "test.yaml", k.anon=10, l.div=2)
#'
#' # To access SDC object
#' sdc$sdc
#' 
#' # Create the data extract after the k.anon and l.div is decided. 
#' ccd.anon <- anonymisation(ccd, "test.yaml", k.anon=10)
#' }
#' @import data.table
#' @export
sdc.trial <- function(ccd, conf, remove.alive=T, verbose=F, k.anon=5,
                      l.div=NULL) {

    if (is.character(conf))
        conf <- yaml.load_file(conf)

    vn <- parse.conf(conf)

    if (verbose)  cat("parsing the cleanEHR object ...\n")

    demg <- data.table(suppressWarnings(demographic.patient.spell(ccd)))
    demg <- remove.patients(demg, conf$removelist)
    demg <- append.age(demg)

    # Remove dead episodes 
    if (remove.alive)
        demg <- demg[DIS=="D"]

    demg <- deltaTime1d(demg, conf$deltaTime, conf$maxStay)
    
    demg <- custom.operation(demg, conf)
    # Remove direct identifiable variables 
    demg <- remove.direct.vars(demg, vn$dirv)
    demg <- microaggregation.numvar(demg, conf)

    sdc <- createSdcObj(demg, keyVars=c(vn$ctgrv, vn$numv))
    sdc <- localSuppression(sdc, k=k.anon)


    if (!is.null(l.div)) {
        manipSensVars <- suppress.ldiversity(sdc, vn$sensv, verbose, l.div)
        demg[, vn$sensv] <- manipSensVars[, vn$sensv]
    }

    demg[, sdc@keyVars] <- sdc@manipKeyVars

    demg <- addnoise.numvar(demg, conf)

    if (verbose)
        print(sdc)

    return(list(data=demg, sdc=sdc, conf=vn))
}


custom.operation <- function(demg, conf) {
    ops <- conf$Operations
    for (item in names(ops)) {
        func <- eval(parse(text=ops[item]))
        demg[[item]] <- func(demg[[item]])
    }
    return(demg)
}


suppress.ldiversity <- function(sdc, sensv, verbose, l.div) {
    manipSensVars <- list()
    for (i in sensv) {
        ld <- ldiversity(sdc, i)@risk$ldiversity
        manipVar <- sdc@origData[[i]]
        manipVar[ld[1:nrow(ld), 2] < l.div] <- NA
        manipSensVars[[i]] <- manipVar    
    }
    as.data.frame(manipSensVars)
}

#' adding noise to all numerical variables. 
#' 
#' The numerical variables includes datetime which will be
#' treated as numeric by converting them to numeric. It will be converted back
#' to datetime format at them end. 
addnoise.numvar <- function(demg, conf) {
    vn <- parse.conf(conf)
    demg <- data.frame(convert.numeric.datetime(demg))
    numconf <- conf$numVars
    numv <- non.unique.columns(demg, vn$numv)

    for (item in numv) {
        if (!is.null(numconf[[item]][['noise']])) {
            noise.level <- numconf[[item]][['noise']]
            demg[, item] <- addNoise(data.frame(demg[, item]), noise=noise.level)$xm
        }
    }
    demg <- convert.back.datetime(demg)
    demg
}

#' microaggregation on numeric variables
#' 
#' @param demg data.table/data.frame 
#' @param conf configuration list
#' @return demg data.frame
microaggregation.numvar <- function(demg, conf) {
    vn <- parse.conf(conf)
    # It is acceptable to have wrong formatted date time which generate warnings.
    demg <- suppressWarnings(data.frame(convert.numeric.datetime(demg)))

    numconf <- conf$numVars
    numv <- non.unique.columns(demg, vn$numv)

    for (item in numv) {
        if (!is.null(numconf[[item]][['aggr']])) {
            agg.level <- numconf[[item]][['aggr']]
            demg[, item] <- 
                microaggregation(data.frame(demg[, item]), 
                                 aggr=agg.level)$mx
        }
    }
    demg <- suppressWarnings(convert.back.datetime(demg))
    demg
}

#' Remove direct identifiers specified
#' 
#' @param data data.frame/data.table
#' @param dirvs direct variables name 
#' @return data data.frame/data/table 
remove.direct.vars <- function(data, dirvs) {
    for (i in dirvs) 
        data[[i]] <- NULL
    data 
}


#' Append AGE column to the demographic table 
append.age <- function(demg) {
    format.dob <- "%Y-%m-%d"
    format.dah <- "%Y-%m-%d"
    demg$AGE <- difftime(as.POSIXct(demg$DAH, format=format.dah),
                         as.POSIXct(demg$DOB, format=format.dob), 
                         units="days")/365
    demg$AGE <- as.numeric(floor(as.numeric(demg$AGE)))
    demg$index <- seq(nrow(demg))
    return(demg)
}


#' Convert all the date-time columns to delta time, but still convert them back
#' with an origin of 1970-01-01. 
deltaTime1d <- function(demg, items, maxstay) {
    if (!"DAICU" %in% items)
        stop("DAICU must in deltaTime. ")

    demg <- data.frame(demg)
    demg <- convert.numeric.datetime(demg, items)
    demg[, items] <- demg[, items] - demg$DAICU

    mxind <- apply(demg[, items], 1, function(x) max(x, na.rm=T)) > maxstay * 3600 * 24
    
    if (any(mxind))
        demg <- demg[-which(mxind), ]

    demg <- convert.back.datetime(demg, items)
    demg
}


clinic.data.list <- function(ccd, index, nidentify) {
    cdl <- list()
    for (i in stname2code(nidentify)) {
        cdl[[i]] <- ccd@episodes[[index]]@data[[i]]
    }

#
#
#    cdl  <- lapply(ccd@episodes[[index]]@data, 
#                   function(x) {
#                       for (i in stname2code(nidentify))
#
#                       if (length(x) > 1)
#                           return(x)
#                       else 
#                           return(NULL)
#                   })
#    for (i in names(cdl)) {
#        if (is.null(cdl[[i]]))
#            cdl[[i]] <- NULL
#    }
    cdl
}

create.anonym.ccd <- function(ccd, sdc) {
    sdc$data$ADNO <- seq(nrow(sdc$data))
    sdc$data$ICNNO <- "pseudo_site"

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




    new.record <- ccRecord()

    for (i in seq(nrow(sdc$data))) {
        cdl <- clinic.data.list(ccd, sdc$data$index[i], sdc$conf$nidentify)
        sdcl <- sdc.row2list(sdc$data[i, ])
        new.record <- new.record + new.episode(append(cdl, sdcl))
    }
    new.record@infotb[, pid:=sdc$data$pid]
    new.record@infotb[, spell:=sdc$data$spell]
    new.record
}



non.unique.columns <- function(data, numv) {
    # check the data type and avoid processing on the 
    # all NA columns.
    new.numv <- vector()
    for (i in seq(numv)) {
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
    cleanEHR:::for_each_episode(ccd, 
                              function(x) {
                                  if (any(dirv %in% names(x@data)))
                                      stop("direct identifiable items appeared in the final data!!!")
                              })
}

remove.patients <- function(demg, rmfile) {
    if (is.null(rmfile))
        return(demg)
    rmpatients <- as.character(read.csv(rmfile, header=F)[[1]])
    demg <- demg[!NHSNO %in% rmpatients]
    demg <- demg[!pasno %in% rmpatients]
    demg <- demg[!paste(ICNNO, ADNO, sep=":") %in% rmpatients]
   
    demg
}
