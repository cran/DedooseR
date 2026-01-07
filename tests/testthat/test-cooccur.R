# Test 1
test_that("cooccur() returns count matrix and plot", {
  testthat::skip_if_not_installed("ggraph")
  testthat::skip_if_not_installed("igraph")
  excerpts <- dplyr::tibble(
    media_title = c("Doc1", "Doc1", "Doc2", "Doc3"),
    c_hope = c(1, 0, 1, 0),
    c_family = c(1, 1, 0, 1),
    c_school = c(0, 1, 1, 1)
  )
  result <- cooccur(
    excerpts = excerpts,
    output = "tibble",
    scale = "count",
    plot = TRUE,
    edge_min = 1
  )
  expect_s3_class(result$matrix, "tbl_df")
  expect_s3_class(result$plot, "ggplot")
  matrix_tbl <- result$matrix
  hope_row <- matrix_tbl[matrix_tbl$code == "c_hope", ]
  expect_equal(as.numeric(hope_row[["c_family"]]), 1)
  expect_equal(as.numeric(hope_row[["c_school"]]), 2)
})

# Test 2
test_that("cooccur() outputs column-wise proportions", {
  excerpts <- dplyr::tibble(
    media_title = c("Doc1", "Doc1", "Doc2", "Doc3"),
    c_hope = c(1, 0, 1, 0),
    c_family = c(1, 1, 0, 1),
    c_school = c(0, 1, 1, 1)
  )
  result <- cooccur(
    excerpts = excerpts,
    output = "data.frame",
    scale = "prop",
    plot = FALSE
  )
  matrix_df <- result$matrix
  expect_true(is.data.frame(matrix_df))
  expect_equal(rownames(matrix_df), colnames(matrix_df))
  expect_equal(matrix_df["c_hope", "c_school"], 0.667)
  expect_equal(matrix_df["c_school", "c_hope"], 1)
})

# Test 3
test_that("cooccur() returns kable output with bold formatting", {
  testthat::skip_if_not_installed("kableExtra")
  excerpts <- dplyr::tibble(
    media_title = c("Doc1", "Doc2"),
    c_support = c(1, 1),
    c_growth = c(1, 0)
  )
  kable_result <- cooccur(
    excerpts = excerpts,
    output = "kable",
    scale = "count",
    min_bold = 1,
    plot = FALSE
  )
  expect_s3_class(kable_result$matrix, "knitr_kable")
  html_table <- as.character(kable_result$matrix)
  expect_true(grepl("font-weight: bold", html_table, fixed = TRUE))
})

# Test 4
test_that("cooccur() formats proportional kable output", {
  testthat::skip_if_not_installed("kableExtra")

  excerpts <- dplyr::tibble(
    media_title = c("Doc1", "Doc1", "Doc2"),
    c_support = c(1, 0, 1),
    c_growth = c(0, 1, 1)
  )

  result <- cooccur(
    excerpts = excerpts,
    output = "kable",
    scale = "prop",
    min_bold = 0.5,
    plot = FALSE
  )

  expect_s3_class(result$matrix, "knitr_kable")
  html_table <- as.character(result$matrix)
  expect_true(grepl("1.000", html_table, fixed = TRUE))
})

# Test 5
test_that("cooccur() applies codebook labels when requested", {
  testthat::skip_if_not_installed("kableExtra")
  excerpts <- dplyr::tibble(
    media_title = c("Doc1", "Doc2"),
    c_support = c(1, 1),
    c_growth = c(1, 0)
  )
  codebook <- dplyr::tibble(
    variable = c("c_support", "c_growth"),
    label = c("Support Received", "Personal Growth")
  )
  result <- cooccur(
    excerpts = excerpts,
    output = "tibble",
    use_labels = TRUE,
    codebook = codebook,
    plot = FALSE
  )
  expect_true(all(result$matrix$code %in% codebook$label))
})

# Test 6
test_that("cooccur() validates inputs", {
  expect_error(cooccur(), "You must provide `excerpts`.", fixed = TRUE)
  expect_error(
    cooccur(dplyr::tibble(c_support = c(1, 0))),
    "`excerpts` must contain a `media_title` column.",
    fixed = TRUE
  )
  expect_error(
    cooccur(dplyr::tibble(media_title = "Doc1")),
    "No code columns found (columns must start with 'c_').",
    fixed = TRUE
  )
  expect_error(
    cooccur(
      excerpts = dplyr::tibble(
        media_title = c("Doc1", "Doc2"),
        c_support = c(1, 0)
      ),
      use_labels = TRUE
    ),
    "You must provide a `codebook` dataframe when `use_labels = TRUE`.",
    fixed = TRUE
  )
})

# Test 7
test_that("cooccur() errors when codebook lacks required columns", {
  expect_error(
    cooccur(
      excerpts = dplyr::tibble(
        media_title = c("Doc1", "Doc2"),
        c_support = c(1, 0)
      ),
      use_labels = TRUE,
      codebook = dplyr::tibble(variable = "c_support")
    ),
    "`codebook` must have columns named `variable` and `label`.",
    fixed = TRUE
  )
})

# Test 8
test_that("cooccur() handles high edge_min thresholds gracefully", {
  testthat::skip_if_not_installed("ggraph")
  testthat::skip_if_not_installed("igraph")

  excerpts <- dplyr::tibble(
    media_title = c("Doc1", "Doc2", "Doc3"),
    c_support = c(1, 1, 0),
    c_growth = c(0, 1, 1)
  )

  result <- cooccur(
    excerpts = excerpts,
    output = "tibble",
    scale = "count",
    plot = TRUE,
    edge_min = 5
  )

  expect_s3_class(result$plot, "ggplot")
  expect_equal(nrow(result$plot$data), 0)
})
