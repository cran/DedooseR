# Test 1:
test_that("view_excerpts() returns datatable with labelled codes", {
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("tidyr")
  data <- dplyr::tibble(
    excerpt = c("Support felt throughout.", "Personal transformation noted.", "Unused excerpt."),
    c_support = c(TRUE, FALSE, FALSE),
    c_growth = c(FALSE, TRUE, FALSE)
  )
  attr(data$c_support, "label") <- "Support Received"
  attr(data$c_growth, "label") <- "Personal Growth"
  capture.output(widget <- view_excerpts(data))
  expect_s3_class(widget, c("datatables", "htmlwidget"))
  expect_equal(attr(widget$x, "colnames"), c("code", "excerpt"))
  expect_equal(widget$x$data$code, c("Support Received", "Personal Growth"))
  expect_equal(widget$x$data$excerpt, data$excerpt[1:2])
})

# Test 2: 
test_that("view_excerpts() falls back to variable names without labels", {
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("tidyr")
  data <- dplyr::tibble(
    excerpt = c("Code applied.", "Code skipped."),
    c_access = c(TRUE, FALSE)
  )
  capture.output(widget <- view_excerpts(data))
  expect_equal(widget$x$data$code, "c_access")
  expect_equal(nrow(widget$x$data), 1L)
})

# Test 3: 
test_that("view_excerpts() validates input requirements", {
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("tidyr")
  expect_error(
    view_excerpts(dplyr::tibble(c_access = TRUE)),
    "\"excerpt\" %in% names(data) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    view_excerpts(dplyr::tibble(excerpt = c("Text A", "Text B"))),
    "No code columns found (must start with 'c_').",
    fixed = TRUE
  )
})

