context("Date/time to numeric conversion")

test_that("convert to numeric", {
    dtype <- sapply(ccdata:::ITEM_REF, function(x) x$Datatype)
    tdate <- dtype[dtype == "date"]
    ttime <- dtype[dtype == "time"]
    tdatetime <- dtype[dtype == "date/time"]


    create.test.data <- function(hic_code, val) {
        stname <- code2stname(names(hic_code))
        rep.data <- data.frame(t(rep(val, length(stname))), stringsAsFactors=F)
        names(rep.data) <- stname
        return(rep.data)
    }

    test_data <- create.test.data(tdate, "2010-01-01")
    result <- convert.numeric.datetime(test_data)
    expect_equal(length(unique(t(result))), 1)
    expect_equal(as.POSIXct("2010-01-01"), as.POSIXct(as.numeric(result[1]), origin="1970-01-01"))
    expect_equivalent(convert.back.datetime(result), test_data)

    test_data <- create.test.data(ttime, "00:00:00")
    result <- convert.numeric.datetime(test_data)
    expect_equal(length(unique(t(result))), 1)
    expect_true(!NA %in% result)

   expect_equivalent(convert.back.datetime(result), test_data)


    test_data <- create.test.data(tdatetime, "2010-01-01 00:00")
    result <- convert.numeric.datetime(test_data)
    expect_equal(length(unique(t(result))), 1)
    expect_true(!NA %in% result)
    expect_equivalent(convert.back.datetime(result), test_data)

})
