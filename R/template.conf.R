#' create a YAML configuration template
#' 
#' This is a function to create a YAML configuration example which can be used
#' as a template for users.
#' @param outfile the file name of YAML configuration template.
#' @export 
template.conf <- function(outfile="template.yaml") {
    conffile <- paste(path.package('ccanonym'), "/inst/test.yaml", sep="")
    conffile <- yaml.load_file(conffile)
    write(file=outfile, as.yaml(conffile))
}
