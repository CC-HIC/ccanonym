context("Process RAICU (type) data")


test_that("", {
    expect_equivalent(raicu.breakdown("1.2.3.4.5"), "1.2.3")
    expect_equivalent(raicu.breakdown("1.2.3.4.5", 4), "1.2.3.4")

    expect_equivalent(raicu.breakdown("10.2.3.4.5"), "10.2.3")
    expect_equivalent(raicu.breakdown("01.2.3.4.5"), "1.2.3")
    
    expect_equivalent(raicu.breakdown("1.2.3"), "1.2.3")
    expect_equivalent(raicu.breakdown("1.2"), "1.2")
    expect_equivalent(raicu.breakdown("1"), "1")
    
    expect_equivalent(raicu.breakdown(c("1", "1.2.3.4")), c("1", "1.2.3"))
})
