# Tests for approx_zadoks
test_that("approx_zadoks interpolates correctly for target within range", {
    zadoks <- c(51, 53, 56, 59)
    date <- as.Date(c("2025-08-01", "2025-08-05", "2025-08-10", "2025-08-15"))
    target <- 55
    result <- approx_zadoks(zadoks, date, target)
    expect_true(inherits(result, "Date"))
    expect_equal(round(result), as.Date("2025-08-08"))
})

test_that("approx_zadoks returns exact date for exact match", {
    zadoks <- c(51, 53, 55, 59)
    date <- as.Date(c("2025-08-01", "2025-08-05", "2025-08-10", "2025-08-15"))
    target <- 55
    result <- approx_zadoks(zadoks, date, target)
    expect_true(inherits(result, "Date"))
    expect_equal(round(result), as.Date("2025-08-10"))
})

test_that("approx_zadoks returns NA for target out of range", {
    zadoks <- c(56, 59)
    date <- as.Date(c("2025-08-10", "2025-08-15"))
    target <- 55
    result <- approx_zadoks(zadoks, date, target)
    expect_equal(result, NA)
})


test_that("approx_zadoks errors for invalid input types", {
    zadoks <- c("a", "b", "c")
    date <- as.Date(c("2025-08-01", "2025-08-05", "2025-08-10"))
    target <- 20
    expect_error(approx_zadoks(zadoks, date, target))
    zadoks <- c(10, 20, 30)
    date <- c("2025-08-01", "2025-08-05", "2025-08-10")
    expect_error(approx_zadoks(zadoks, date, target))
    date <- as.Date(c("2025-08-01", "2025-08-05", "2025-08-10"))
    target <- c(20, 25)
    expect_error(approx_zadoks(zadoks, date, target))
})


