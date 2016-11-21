context("Process RAICU (type) data")


test_that("", {
    expect_equivalent(as.numeric(raicu.breakdown(data.frame(x="1.2.3.4.5"))), 
                      seq(5))
    expect_equivalent(as.numeric(raicu.breakdown(data.frame(x="1.2.3.4.5.6"))), 
                      seq(5))
    expect_equivalent(as.numeric(raicu.breakdown(data.frame(x="1.2.3.4"))), 
                      c(seq(4), NA))
    expect_equivalent(as.numeric(raicu.breakdown(data.frame(x="crazy.stuff.with.points"))),
                      as.numeric(rep(NA, 5)))


    demg <- suppressWarnings(sql.demographic.table(ccd))
    expect_equivalent(raicu.to.category(demg, "RAICU1")$RAICU1, NULL)

})
