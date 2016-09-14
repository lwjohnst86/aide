context("Functions work")

## TODO: Add more tests

test_that("basic stats work", {
    expect_identical(min_max(1:10), "1.0 to 10.0")
    expect_identical(min_max(1:10, digits = 2), "1.00 to 10.00")

    expect_identical(average(1:10), "5.5")
    expect_identical(average(1:10, digits = 2), "5.50")

    expect_identical(stddev(c(1, 1)), "0.0")
    expect_identical(stddev(c(1, 1), digits = 2), "0.00")

    expect_identical(ave_sd(c(1, 1)), "1.0 (0.0)")
    expect_identical(ave_sd(c(1, 1), digits = 2), "1.00 (0.00)")

    expect_identical(med(1:10), "5.5")
    expect_identical(med(1:10, digits = 2), "5.50")

    expect_identical(iqr(c(1, 1, 2, 2)), "1.0 to 2.0")
    expect_identical(iqr(c(1, 1, 2, 2), digits = 2), "1.00 to 2.00")

    expect_identical(med_iqr(c(1, 1, 2, 2)), "1.5 (1.0 to 2.0)")
    expect_identical(med_iqr(c(1, 1, 2, 2), digits = 2), "1.50 (1.00 to 2.00)")
})

test_that("more complicated stats work", {
    expect_identical(levels(tertile(1:9)), c("[1,3.66]", "(3.66,6.33]", "(6.33,9]"))

    expect_identical(correlate(1:10, 1:10), "1.00")
    expect_identical(correlate(1:10, 1:10, digits = 2), "1.00")
    expect_identical(correlate(1:10, 1:10, digits = 2, method = "spearman"), "1.00")
    expect_identical(correlate(1:10, 10:1), "-1.00")
})

test_that("character manipulation utilities work", {
    expect_identical(trim_ws(" whitespace trimmed "), "whitespace trimmed")
})