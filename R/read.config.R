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


