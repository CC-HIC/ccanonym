context("create sdc object from the yaml configuration file")

#xml2Data()
test_that("create sdc object", {
    ccd <<- xml2Data("../data/test_data_anonym.xml")
    sdc <- create.sdc(ccd, '../../data/test.yaml', verbose=T)
    
})
