# Test 1: the output contains a table and if plot = TRUE, it also contains a plot
test_that("create_code_summary() builds counts table for c_ code columns", {
  excerpts <- dplyr::tibble(
    media_title = c("Media One", "Media One", "Media Two", "Media Three", "Media Three"),
    c_access = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    c_support = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    c_advocacy = c(FALSE, FALSE, TRUE, FALSE, TRUE),
    notes = c("alpha", "beta", "gamma", "delta", "epsilon")
  )
  capture.output(summary_tbl <- create_code_summary(
    excerpts,
    output_type = "tibble",
    plot = FALSE
  ))
  expect_s3_class(summary_tbl, "tbl_df")
  expect_equal(
    names(summary_tbl),
    c("code", "count", "n_media_titles", "prop_media_titles")
  )
  expect_true(all(grepl("^c_", summary_tbl$code)))
  expected <- dplyr::tibble(
    code = c("c_access", "c_advocacy", "c_support"),
    count = c(3L, 2L, 1L),
    n_media_titles = c(2L, 2L, 1L)
  ) %>%
    dplyr::mutate(prop_media_titles = round(n_media_titles / max(n_media_titles), 2)) %>%
    dplyr::arrange(code)
  summary_tbl_sorted <- dplyr::arrange(summary_tbl, code)
  expect_equal(summary_tbl_sorted, expected)
})

# Test 2: the table contains a column named code_count and prop_media_titles, with c_ cols in rows and their variable names in the output
  # the code_counts should be a count of each time a code = TRUE in the dataset
  # prop_media_titles is the number of distinct media titles the code appears in
test_that("create_code_summary() exposes expected columns and values", {
  excerpts <- dplyr::tibble(
    media_title = c("Media One", "Media One", "Media Two", "Media Three", "Media Three"),
    c_access   = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    c_support  = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    c_advocacy = c(FALSE, FALSE, TRUE, FALSE, TRUE)
  )

  capture.output(tbl <- create_code_summary(excerpts, plot = FALSE))

  expect_equal(colnames(tbl), c("code", "count", "n_media_titles", "prop_media_titles"))
  expect_true(all(grepl("^c_", tbl$code)))

  # lock in the numeric values
  expected <- dplyr::tibble(
    code = c("c_access", "c_advocacy", "c_support"),
    count = c(3L, 2L, 1L),
    n_media_titles = c(2L, 2L, 1L),
    prop_media_titles = c(1, 1, 0.5)
  )
  expect_equal(tbl[order(tbl$code), ], expected)
})

# Test 3: if table_min_count or table_min_prop are set, the table counts and props do not include anything above those values
test_that("create_code_summary() enforces table_min_count and table_min_prop thresholds", {
  excerpts <- dplyr::tibble(
    media_title = c("Media One", "Media One", "Media Two", "Media Three", "Media Three"),
    c_access = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    c_support = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    c_advocacy = c(FALSE, FALSE, TRUE, FALSE, TRUE)
  )

  capture.output(count_filtered <- create_code_summary(
    excerpts,
    table_min_count = 3,
    plot = FALSE
  ))
  expect_equal(nrow(count_filtered), 1L)
  expect_equal(count_filtered$code, "c_access")
  expect_true(all(count_filtered$count >= 3))

  capture.output(prop_filtered <- create_code_summary(
    excerpts,
    table_min_prop = 0.75,
    plot = FALSE
  ))
  expect_equal(nrow(prop_filtered), 1L)
  expect_equal(prop_filtered$code, "c_access")
  expect_true(all(prop_filtered$prop_media_titles >= 0.75))
})

# Test 4: if output_type is set, the output is appropriate
test_that("create_code_summary() returns the requested output_type", {
  excerpts <- dplyr::tibble(
    media_title = c("Media One", "Media One", "Media Two"),
    c_access = c(TRUE, TRUE, FALSE),
    c_support = c(FALSE, TRUE, FALSE)
  )
  capture.output(tbl <- create_code_summary(
    excerpts,
    output_type = "tibble",
    plot = FALSE
  ))
  expect_s3_class(tbl, "tbl_df")
  capture.output(kab <- create_code_summary(
    excerpts,
    output_type = "kable",
    plot = FALSE
  ))
  expect_s3_class(kab, "knitr_kable")
  testthat::skip_if_not_installed("DT")
  capture.output(dt <- create_code_summary(
    excerpts,
    output_type = "datatable",
    plot = FALSE
  ))
  expect_s3_class(dt, "datatables")
  expect_s3_class(dt, "htmlwidget")
})


# Test 5: if exclude codes are set, the table and plot do not include these codes
test_that("create_code_summary() honors excluded codes in table and plot", {
  excerpts <- dplyr::tibble(
    media_title = c("Media One", "Media One", "Media Two", "Media Three"),
    c_access = c(TRUE, TRUE, FALSE, TRUE),
    c_support = c(FALSE, TRUE, FALSE, FALSE),
    c_advocacy = c(FALSE, FALSE, TRUE, FALSE)
  )
  capture.output(filtered_tbl <- create_code_summary(
    excerpts,
    exclude = "c_support",
    plot = FALSE
  ))
  expect_false("c_support" %in% filtered_tbl$code)
  capture.output(filtered_plot <- create_code_summary(
    excerpts,
    exclude = "c_support",
    plot = TRUE
  ))
  expect_false("c_support" %in% filtered_plot$table$code)
  expect_false("c_support" %in% filtered_plot$plot$data$code)
})

# Test 6: use codebook labels in table and plot
test_that("create_code_summary() applies codebook labels when requested", {
  excerpts <- dplyr::tibble(
    media_title = c("Media One", "Media Two", "Media Two"),
    c_access = c(TRUE, FALSE, TRUE),
    c_advocacy = c(FALSE, TRUE, TRUE)
  )
  codebook <- dplyr::tibble(
    variable = c("c_access", "c_advocacy"),
    label = c("Access to Care", "Advocacy Efforts")
  )
  capture.output(summary_lbl <- create_code_summary(
    excerpts,
    use_labels = TRUE,
    codebook = codebook,
    plot = FALSE
  ))
  expect_true(all(summary_lbl$code %in% codebook$label))
  expect_false("c_access" %in% summary_lbl$code)
  expect_false("c_advocacy" %in% summary_lbl$code)
  expect_warning(
    capture.output(summary_warn <- create_code_summary(
      excerpts,
      use_labels = TRUE,
      codebook = codebook[-1, , drop = FALSE],
      plot = FALSE
    )),
    "Some codes missing"
  )
  expect_true("c_access" %in% summary_warn$code)
  expect_true("Advocacy Efforts" %in% summary_warn$code)
})

# Test 7: plot_metric is respected
test_that("create_code_summary() respects plot_metric options", {
  excerpts <- dplyr::tibble(
    media_title = c("Media One", "Media One", "Media Two", "Media Three", "Media Three"),
    c_access = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    c_support = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    c_advocacy = c(FALSE, FALSE, TRUE, FALSE, TRUE)
  )
  capture.output(count_plot <- create_code_summary(
    excerpts,
    plot = TRUE,
    plot_metric = "count"
  ))
  count_layer <- ggplot2::layer_data(count_plot$plot)
  expect_equal(sort(count_layer$y), sort(c(3, 2, 1)))
  expect_equal(count_plot$plot$labels$y, "Excerpt Frequency")
  expect_equal(count_plot$plot$labels$title, "Code Counts")
  capture.output(prop_plot <- create_code_summary(
    excerpts,
    plot = TRUE,
    plot_metric = "prop"
  ))
  prop_layer <- ggplot2::layer_data(prop_plot$plot)
  expect_equal(sort(prop_layer$y), sort(c(1, 1, 0.5)))
  expect_equal(prop_plot$plot$labels$y, "Proportion of Media Titles")
  expect_equal(prop_plot$plot$labels$title, "Code Frequencies by Media Title Coverage")
  capture.output(both_plot <- create_code_summary(
    excerpts,
    plot = TRUE,
    plot_metric = "both"
  ))
  both_layer <- ggplot2::layer_data(both_plot$plot)
  expect_equal(sort(both_layer$y), sort(c(3, 2, 1)))
  expect_equal(both_plot$plot$labels$title, "Code Frequencies: Counts and Proportions")
})

# Test 8: fill color option
test_that("create_code_summary() applies fill_color to plots", {
  excerpts <- dplyr::tibble(
    media_title = c("Media One", "Media One", "Media Two"),
    c_access = c(TRUE, TRUE, FALSE),
    c_support = c(FALSE, TRUE, FALSE)
  )
  capture.output(default_plot <- create_code_summary(
    excerpts,
    plot = TRUE
  ))
  expect_equal(default_plot$plot$layers[[1]]$aes_params$fill, "steelblue")
  capture.output(custom_plot <- create_code_summary(
    excerpts,
    plot = TRUE,
    fill_color = "tomato"
  ))
  expect_equal(custom_plot$plot$layers[[1]]$aes_params$fill, "tomato")
})

# Test 9: plot_min_count and plot_min_prop filters for plots
test_that("create_code_summary() applies plot_min_count and plot_min_prop filters", {
  excerpts <- dplyr::tibble(
    media_title = c("Media One", "Media One", "Media Two", "Media Three"),
    c_access = c(TRUE, TRUE, FALSE, TRUE),
    c_support = c(FALSE, TRUE, FALSE, FALSE),
    c_advocacy = c(FALSE, FALSE, TRUE, FALSE)
  )

  capture.output(filtered_plot <- create_code_summary(
    excerpts,
    plot = TRUE,
    plot_min_count = 2,
    plot_min_prop = 0.75
  ))

  expect_true("c_support" %in% filtered_plot$table$code)
  expect_false("c_support" %in% filtered_plot$plot$data$code)
  expect_true(all(filtered_plot$plot$data$count >= 2))
  expect_true(all(filtered_plot$plot$data$prop_media_titles >= 0.75))
})

# Test 10: numeric and labelled codes are coerced to logical
test_that("create_code_summary() coerces numeric and labelled codes to logical", {
  excerpts <- dplyr::tibble(
    media_title = c("Doc1", "Doc2", "Doc3"),
    c_numeric = c(1, 0, 1),
    c_labelled = haven::labelled(
      c(1, 0, 1),
      labels = c("No" = 0, "Yes" = 1)
    )
  )

  capture.output(summary_tbl <- create_code_summary(
    excerpts,
    output_type = "tibble",
    plot = FALSE
  ))

  expect_equal(
    summary_tbl$count[summary_tbl$code == "c_numeric"],
    2L
  )
  expect_equal(
    summary_tbl$count[summary_tbl$code == "c_labelled"],
    2L
  )
  expect_true(all(summary_tbl$n_media_titles[summary_tbl$code %in% c("c_numeric", "c_labelled")] == 2L))
})

# Test 11: input validation errors are raised
test_that("create_code_summary() validates inputs and codebook requirements", {
  expect_error(
    create_code_summary("not a data frame"),
    "`excerpts` must be a data frame.",
    fixed = TRUE
  )

  expect_error(
    create_code_summary(dplyr::tibble(c_code = c(TRUE, FALSE))),
    "`excerpts` must contain a `media_title` column.",
    fixed = TRUE
  )

  excerpts <- dplyr::tibble(
    media_title = c("Doc1", "Doc2"),
    c_code = c(TRUE, FALSE)
  )

  expect_error(
    create_code_summary(excerpts, use_labels = TRUE),
    "You must provide a `codebook` dataframe when `use_labels = TRUE`.",
    fixed = TRUE
  )

  bad_codebook <- dplyr::tibble(variable = "c_code")
  expect_error(
    create_code_summary(excerpts, use_labels = TRUE, codebook = bad_codebook),
    "`codebook` must contain columns named `variable` and `label`.",
    fixed = TRUE
  )
})
