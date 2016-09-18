library(ccdata)
an <- read.csv("data/ANON_REF.csv")
newan <- paste(code2stname(as.character(an$NHICcode)), an$dataItem, sep=" # ")
newlist <- list()

newlist[["direct_var"]] <- newan[an$Anonymisation == "Direct identifier"]
newlist[["key_var"]] <- newan[an$Anonymisation == "Key variable"]
newlist[["nonidentify_var"]]<- newan[an$Anonymisation == "Non-identifying"]
yml <- as.yaml(newlist)
write(yml, file="newconf.yaml")
