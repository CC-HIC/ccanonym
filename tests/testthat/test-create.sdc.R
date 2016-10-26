context("create sdc object from the yaml configuration file")

ccd <<- xml2Data("../data/test_data_anonym.xml")
demg <- suppressWarnings(sql.demographic.table(ccd))
conf <- yaml.load_file('../../data/test.yaml')
keyvar <- conf$keyVars
keydt <- names(conf$datetimeVars)
numvar <- names(conf$numVars)
var <- c(keydt, keyvar, numvar)

test_that("create sdc object", {
    demg <- data.frame(demg)

    sdc <<- do.sdc(ccd, '../../data/test.yaml')
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


test_that("get all the 2d data from a selected episodes but not the demographic
          data", 
{
    cl <- clinic.data.list(ccd, 2)
    expect_false(all(stname2code(var) %in% names(cl)))

    cl <- clinic.data.list(ccd, 1)
    expect_false(all(stname2code(var) %in% names(cl)))
})



test_that("anonymisation from ccdata", {
    (anonymisation(ccd, conf))
})


test_that("appending age column", {
    age <- append.age(demg)$AGE
    expect_true(is.numeric(age))
    expect_identical(age, c(25, 33, 44, 37, 59))
})
