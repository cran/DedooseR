# Test 1
test_that("compare_saturation() flags codes meeting thresholds", {
  code_summary <- dplyr::tibble(
    code = c("Belonging", "Resilience", "Stress"),
    count = c(10L, 5L, 2L),
    n_media_titles = c(8L, 4L, 2L),
    prop_media_titles = c(0.8, 0.4, 0.2)
  )
  thresholds <- list(
    Liberal = list(code_count = 4, prop_media_title = 0.3),
    Strict = list(code_count = 8, prop_media_title = 0.7)
  )

  result <- compare_saturation(code_summary, thresholds)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("Liberal", "Strict") %in% names(result)))
  expect_equal(result$Liberal, c(TRUE, TRUE, FALSE))
  expect_equal(result$Strict, c(TRUE, FALSE, FALSE))
})

# Test 2
test_that("compare_saturation() returns kable output when requested", {
  code_summary <- dplyr::tibble(
    code = c("Belonging", "Resilience"),
    count = c(10L, 5L),
    n_media_titles = c(8L, 4L),
    prop_media_titles = c(0.8, 0.4)
  )
  thresholds <- list(
    Default = list(code_count = 5, prop_media_title = 0.3)
  )

  kable_output <- compare_saturation(
    code_summary,
    thresholds,
    output_type = "kable"
  )

  expect_s3_class(kable_output, "knitr_kable")
})

test_that("compare_saturation() produces count plots with meeting codes", {
  code_summary <- dplyr::tibble(
    code = c("Belonging", "Resilience", "Stress"),
    count = c(10L, 5L, 2L),
    n_media_titles = c(8L, 4L, 1L),
    prop_media_titles = c(0.8, 0.4, 0.2)
  )
  thresholds <- list(
    Liberal = list(code_count = 4, prop_media_title = 0.3),
    Strict = list(code_count = 8, prop_media_title = 0.7)
  )

  plot_result <- compare_saturation(
    code_summary,
    thresholds,
    plot = TRUE,
    plot_metric = "count"
  )

  expect_type(plot_result, "list")
  expect_named(plot_result, c("results", "plot"))
  expect_s3_class(plot_result$results, "tbl_df")
  expect_s3_class(plot_result$plot, "ggplot")
  expect_setequal(unique(plot_result$plot$data$Set), c("Liberal", "Strict"))
  expect_true(all(plot_result$plot$data$count >= 5))
})

# Test 3
test_that("compare_saturation() respects plot_metric = 'prop' and 'both'", {
  code_summary <- dplyr::tibble(
    code = c("Belonging", "Resilience"),
    count = c(10L, 5L),
    n_media_titles = c(8L, 4L),
    prop_media_titles = c(0.8, 0.4)
  )
  thresholds <- list(
    Liberal = list(code_count = 4, prop_media_title = 0.3)
  )

  prop_plot <- compare_saturation(
    code_summary,
    thresholds,
    plot = TRUE,
    plot_metric = "prop"
  )
  expect_equal(prop_plot$plot$labels$y, "Proportion of Media Titles")
  expect_setequal(prop_plot$plot$data$prop_media_titles, c(0.8, 0.4))

  both_plot <- compare_saturation(
    code_summary,
    thresholds,
    plot = TRUE,
    plot_metric = "both"
  )
  expect_equal(both_plot$plot$labels$title,
               "Code Saturation Comparison by Threshold Set (Counts + Proportions)")
  expect_equal(colnames(both_plot$plot$data), c("code", "count", "prop_media_titles", "Set", "Meets"))
})

# Test 4
test_that("compare_saturation() validates required columns", {
  bad_summary <- dplyr::tibble(
    code = c("Belonging", "Resilience"),
    count = c(10L, 5L)
  )

  expect_error(
    compare_saturation(bad_summary, list(Default = list(code_count = 3, prop_media_title = 0.2))),
    "`code_summary` must contain the following columns",
    fixed = TRUE
  )
})

# Test 5
test_that("compare_saturation() supports kable output when plotting", {
  code_summary <- dplyr::tibble(
    code = c("Belonging", "Resilience"),
    count = c(10L, 5L),
    n_media_titles = c(8L, 4L),
    prop_media_titles = c(0.8, 0.4)
  )
  thresholds <- list(
    Liberal = list(code_count = 4, prop_media_title = 0.3)
  )

  result <- compare_saturation(
    code_summary,
    thresholds,
    output_type = "kable",
    plot = TRUE,
    plot_metric = "count"
  )

  expect_s3_class(result$results, "knitr_kable")
  expect_s3_class(result$plot, "ggplot")
})

# Test 6
test_that("compare_saturation() handles threshold sets with no matches", {
  code_summary <- dplyr::tibble(
    code = c("Belonging", "Resilience"),
    count = c(2L, 1L),
    n_media_titles = c(1L, 1L),
    prop_media_titles = c(0.2, 0.1)
  )
  thresholds <- list(
    Strict = list(code_count = 5, prop_media_title = 0.5)
  )

  result <- compare_saturation(
    code_summary,
    thresholds,
    plot = TRUE,
    plot_metric = "count"
  )

  expect_true(all(!result$results$Strict))
  expect_equal(nrow(result$plot$data), 0)
})
