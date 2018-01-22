#' Parse YAML configuration file
#' 
#' Parse the YAML configuration file and check the parse.conf. 
#' @param conf either the path of YAML configuration of a list
#' @return a list contrains all the parse.conf broken down in each
#' category.
#' @export 
parse.conf <- function(conf) {
    if (is.character(conf))
        conf <- yaml.load_file(conf)

    dirv <- conf$directVars 
    ctgrv <- conf$categoryVars
    numv <- names(conf$numVars)
    sensv <- conf$sensVar

    confvars_origin <-c(dirv, ctgrv, numv, sensv)
    confvars <- sapply(strsplit(confvars_origin, "[.]"), function(x) x[1])

    nidentify <- conf$nonidentifyVars
    all.vars <- c(ctgrv, numv, sensv)

    # all variables in CCHIC data + AGE, the derived variable. 
    ccd.vars <- c(code2stname(names(cleanEHR:::ITEM_REF)), "AGE")

    dirv <- c(dirv, ccd.vars[!confvars %in% ccd.vars])


    if (!all(confvars %in% ccd.vars)) {
        print(ccd.vars[!(confvars %in% ccd.vars)])
        stop("Items in configuration cannot be found in standart CCHIC item list.")
    }
    
#    all.ccd.stname <- c(code2stname(names(cleanEHR:::ITEM_REF)), "AGE")
#
#    # check the correctness of the configuration file.
#    # reduce derived items such as RAICU1.IV to its origin RAICU1
#    ccdvars <- sapply(strsplit(c(all.vars, nonidv), "[.]"), function(x) x[1])
#    index <- ccdvars %in% all.ccd.stname
#    if (!all(index)) {
#        print(ccdvars[!index])
#        stop("Items in configuration file do not appear in cleanEHR item list.")
#    }
#
#    if (any(all.vars %in% nonidv)) {
#        print(all.vars[all.vars %in% nonidv])
#        stop("identifiable variables appeared in non-identifiable slot, check the yaml file!" )
#    }
#
#    confvars <- c(dirv, all.vars, nonidv)
#    # reduce derived items such as RAICU1.IV to its origin RAICU1
#    confvars <- sapply(strsplit(confvars, "[.]"), function(x) x[1])
#    index <- all.ccd.stname %in% confvars
#    if (!all(index)){
#        ss <- as.character(all.ccd.stname[!index])
#        cat(paste("-", ss, "# ", short2longname(ss), "\n"))
#        stop("Missing items in the configuration.")
#    }

    return(list(dirv=dirv, ctgrv=ctgrv, numv=numv, sensv=sensv, 
                all.vars=all.vars, nidentify=nidentify))
}
