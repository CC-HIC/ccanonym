context("test the yaml configuration file")

test_that("create sdc object", {
    sdc2 <- suppressWarnings(sdc.trial(ccd, "../data/standard.yaml"))

})

