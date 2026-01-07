# Test 1
test_that("set_saturation() computes proportions and orders by count", {
  code_counts <- dplyr::tibble(
    code = c("Belonging", "Resilience", "Stress"),
    count = c(10L, 5L, 3L),
    n_media_titles = c(8L, 4L, 2L)
  )
  result <- set_saturation(code_counts, total_media_titles = 10)
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("code", "count", "prop_media_titles"))
  expect_equal(result$code, c("Belonging", "Resilience", "Stress"))
  expect_equal(result$prop_media_titles, c(0.8, 0.4, 0.2))
})

# Test 2
test_that("set_saturation() respects table_min_count and table_min_prop", {
  code_counts <- dplyr::tibble(
    code = c("Belonging", "Resilience", "Stress"),
    count = c(10L, 5L, 3L),
    n_media_titles = c(8L, 4L, 2L)
  )
  filtered <- set_saturation(
    code_counts,
    total_media_titles = 10,
    table_min_count = 5,
    table_min_prop = 0.5
  )
  expect_equal(nrow(filtered), 1L)
  expect_equal(filtered$code, "Belonging")
  expect_true(all(filtered$count >= 5))
  expect_true(all(filtered$prop_media_titles >= 0.5))
})

# Test 3

test_that("set_saturation() accepts list input and returns kable output", {
  code_counts <- dplyr::tibble(
    code = c("Belonging", "Resilience"),
    count = c(10L, 5L),
    n_media_titles = c(8L, 4L)
  )
  list_input <- list(table = code_counts)
  kable_out <- set_saturation(
    list_input,
    total_media_titles = 10,
    output_type = "kable"
  )
  expect_s3_class(kable_out, "knitr_kable")
})

# Test 4
test_that("set_saturation() respects output_type choices", {
  code_counts <- dplyr::tibble(
    code = c("Belonging", "Resilience"),
    count = c(10L, 5L),
    n_media_titles = c(8L, 4L)
  )

  tibble_out <- set_saturation(code_counts, output_type = "tibble")
  expect_s3_class(tibble_out, "tbl_df")

  kable_out <- set_saturation(code_counts, output_type = "kable")
  expect_s3_class(kable_out, "knitr_kable")
})

# Test 5
test_that("set_saturation() returns plot output when requested", {
  code_counts <- dplyr::tibble(
    code = c("Belonging", "Resilience", "Stress"),
    count = c(10L, 5L, 3L),
    n_media_titles = c(8L, 4L, 2L)
  )
  plot_result <- set_saturation(
    code_counts,
    total_media_titles = 10,
    plot = TRUE,
    plot_metric = "count",
    fill_color = "tomato"
  )
  expect_type(plot_result, "list")
  expect_named(plot_result, c("table", "plot"))
  expect_s3_class(plot_result$table, "tbl_df")
  expect_s3_class(plot_result$plot, "ggplot")
  expect_equal(plot_result$plot$layers[[1]]$aes_params$fill, "tomato")
})

# Test 6
test_that("set_saturation() uses default fill color when not provided", {
  code_counts <- dplyr::tibble(
    code = c("Belonging", "Resilience", "Stress"),
    count = c(10L, 5L, 3L),
    n_media_titles = c(8L, 4L, 2L)
  )

  default_plot <- set_saturation(
    code_counts,
    total_media_titles = 10,
    plot = TRUE,
    plot_metric = "count"
  )

  expect_equal(default_plot$plot$layers[[1]]$aes_params$fill, "steelblue")
})

# Test 7
test_that("set_saturation() plot filtering respects plot_min thresholds", {
  code_counts <- dplyr::tibble(
    code = c("Belonging", "Resilience", "Stress"),
    count = c(10L, 5L, 3L),
    n_media_titles = c(8L, 4L, 2L)
  )
  plot_filtered <- set_saturation(
    code_counts,
    total_media_titles = 10,
    plot = TRUE,
    plot_metric = "prop",
    plot_min_count = 5,
    plot_min_prop = 0.5
  )
  expect_true("Stress" %in% plot_filtered$table$code)
  expect_false("Stress" %in% plot_filtered$plot$data$code)
  expect_true(all(plot_filtered$plot$data$count >= 5))
  expect_true(all(plot_filtered$plot$data$prop_media_titles >= 0.5))
})

# Test 8
test_that("set_saturation() infers total_media_titles when omitted", {
  code_counts <- dplyr::tibble(
    code = c("Belonging", "Resilience"),
    count = c(12L, 6L),
    n_media_titles = c(9L, 3L)
  )

  tbl <- set_saturation(code_counts)

  expect_equal(tbl$prop_media_titles, c(1.00, 0.33))
  expect_equal(tbl$count, c(12L, 6L))
})

# Test 9
test_that("set_saturation() validates code_counts input", {
  expect_error(
    set_saturation("not a data frame"),
    "`code_counts` must be a tibble or data frame",
    fixed = TRUE
  )

  bad_df <- dplyr::tibble(code = "Belonging", count = 3L)
  expect_error(
    set_saturation(bad_df),
    "`code_counts` must contain columns `code`, `count`, and `n_media_titles`.",
    fixed = TRUE
  )
})

# Test 10
test_that("set_saturation() with plot_metric = 'both' includes counts and proportions", {
  code_counts <- dplyr::tibble(
    code = c("Belonging", "Resilience"),
    count = c(10L, 5L),
    n_media_titles = c(8L, 4L)
  )

  plot_obj <- set_saturation(
    code_counts,
    plot = TRUE,
    plot_metric = "both"
  )

  expect_equal(plot_obj$plot$labels$title, "Code Saturation")
  expect_equal(
    colnames(plot_obj$plot$data),
    c("code", "count", "prop_media_titles")
  )
  expect_equal(plot_obj$plot$data$count, c(5, 10))
  expect_equal(plot_obj$plot$data$prop_media_titles, c(0.5, 1.0))
})
