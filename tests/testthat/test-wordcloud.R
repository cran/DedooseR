# Test 1:
test_that("wordcloud() returns htmlwidget with expected words", {
  testthat::skip_if_not_installed("wordcloud2")
  testthat::skip_if_not_installed("tidytext")

  data <- dplyr::tibble(
    excerpt = c("Support peers helped", "Peers offered strong support"),
    c_support = c(TRUE, TRUE),
    c_growth = c(FALSE, TRUE)
  )

  capture.output(widget <- wordcloud(data, "c_support", max_words = 10))

  expect_s3_class(widget, c("wordcloud2", "htmlwidget"))
  expect_setequal(widget$x$word, c("support", "peers", "helped", "offered", "strong"))
  expect_true(all(widget$x$freq > 0))
})

# Test 2:
test_that("wordcloud() respects custom stopwords and max_words", {
  testthat::skip_if_not_installed("wordcloud2")
  testthat::skip_if_not_installed("tidytext")

  data <- dplyr::tibble(
    excerpt = c("Belonging at school matters", "School community builds belonging"),
    c_belonging = c(TRUE, TRUE)
  )

  capture.output(widget <- wordcloud(
    data,
    "c_belonging",
    max_words = 2,
    custom_stopwords = "school"
  ))

  expect_lte(length(widget$x$word), 2)
  expect_false("school" %in% widget$x$word)
})

# Test 3: 
test_that("wordcloud() validates inputs and edge cases", {
  testthat::skip_if_not_installed("wordcloud2")
  testthat::skip_if_not_installed("tidytext")

  expect_error(
    wordcloud(dplyr::tibble(c_support = TRUE), "c_support"),
    "\"excerpt\" %in% names(data) is not TRUE",
    fixed = TRUE
  )

  expect_error(
    wordcloud(dplyr::tibble(excerpt = "Text", c_support = TRUE), "c_missing"),
    "code %in% names(data) is not TRUE",
    fixed = TRUE
  )

  expect_error(
    wordcloud(
      dplyr::tibble(excerpt = c("No code here"), c_support = FALSE),
      "c_support"
    ),
    "No excerpts found for code: c_support",
    fixed = TRUE
  )

  expect_error(
    wordcloud(
      dplyr::tibble(excerpt = c("The and it"), c_support = TRUE),
      "c_support"
    ),
    "No valid words found for code: c_support",
    fixed = TRUE
  )
})
