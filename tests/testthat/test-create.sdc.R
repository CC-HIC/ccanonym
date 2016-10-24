context("create sdc object from the yaml configuration file")

test_that("create sdc object", {
    ccd <- xml2Data("../data/test_data_anonym.xml")
    demg <- suppressWarnings(sql.demographic.table(ccd))
    demg <<- data.frame(demg)
    conf <- yaml.load_file('../../data/test.yaml')


    keyvar <- conf$keyVars

    keydt <- names(conf$datetimeVars)
    numvar <- names(conf$numVars)

    var <- c(keydt, keyvar, numvar)

    print("")
#    print(var)
#    print(demg[, var, with=F])

    demg2 <<- create.sdc(ccd, '../../data/test.yaml', verbose=T)
#    print(sdc)
#    print(sdc[, var])
})


test_that("numeric variables sdc control", {
#    conf <- yaml.load_file('../../data/test.yaml')
#    num.vars.sdc(demg, conf$datetimeVars)
})
