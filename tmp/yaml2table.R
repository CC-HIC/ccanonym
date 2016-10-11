library(yaml)
library(purrr)
library(data.table)
anon.dict <- yaml::yaml.load_file("../data/ANON_REF.yaml")
anon.dict <- anon.dict %>% map(function(x) list(
          Anonymisation = x$Anonymisation,
          dataItem = x$dataItem,
          NHICcode = x$NHICcode
          ))
ll <- length(anon.dict)
dt <- data.table(matrix(unlist(anon.dict), nrow=ll, byrow=T),stringsAsFactors=FALSE)
names(dt) <- c("Anonymisation", "dataItem", "NHICcode")
dt[, Anonymisation := factor(Anonymisation,
    levels = c("Direct identifier", "Key variable", "Sensitive", "Non-identifying"),
    ordered=TRUE)]
setcolorder(dt, c("Anonymisation", "NHICcode", "dataItem"))
dt <- dt[order(Anonymisation, NHICcode)]
dt
fwrite(dt, file="../data/ANON_REF.csv")