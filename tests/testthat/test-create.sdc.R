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

    sdc <<- do.sdc(ccd, '../../data/test.yaml', verbose=T)
#    print(sdc)
#    print(sdc[, var])
})


test_that("remove unique columns", {
    tdata <- data.frame(a=rep(1, 10))
    expect_true(is.null(non.unique.columns(tdata, "a")))
    
    tdata <- data.frame(a=seq(1, 10))
    expect_equal(non.unique.columns(tdata, "a"), "a")


    tdata <- data.frame(a=c(1, 2, 3 ,NA, NA))
    expect_equal(non.unique.columns(tdata, "a"), "a")

    tdata <- data.frame(a=c(1, 1, 1 ,NA, NA))
    expect_true(is.null(non.unique.columns(tdata, "a")))

    tdata <- data.frame(a=as.numeric(c(NA ,NA, NA)))
    expect_true(is.null(non.unique.columns(tdata, "a")))

    tdata <- data.frame(a=c("x", "y"))
    expect_error(non.unique.columns(tdata, "a"))

})
