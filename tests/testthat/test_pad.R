library(gerard)
context("padding and dfing")

test_that("pad_length works", {
    x <- c(1, 2, 3)
    n <- 11
    y <- pad_length(x, n)
    expect_equal(length(y), n)
    expect_true(all(is.na(y[(length(x) + 1):n])))
    expect_equal(y[1:length(x)], x)
}
)


test_that("cbind_list works", {
    lst <- list(1:3, 1:5, "a")

    dfout <- cbind_list(lst)

    expect_true(is.data.frame(dfout))
    expect_equal(class(dfout[, 1]), "integer")
    expect_equal(class(dfout[, 2]), "integer")
    expect_equal(class(dfout[, 3]), "character")
}
)
