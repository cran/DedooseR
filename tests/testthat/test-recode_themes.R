# Test 1: recoded variables + untouched codes remain
test_that("recode_themes() combines logical codes and preserves labels", {
  raw <- dplyr::tibble(
    excerpt_copy = c("Text A", "Text B", "Text C"),
    c_positive_impact = c(TRUE, FALSE, TRUE),
    c_joy = c(TRUE, FALSE, TRUE),
    c_hope = c(TRUE, FALSE, TRUE),
    c_negative_impact = c(FALSE, TRUE, FALSE),
    excerpt_creator = c("Coder A", "Coder B", "Coder C"),
    media_title = c("Transcript A", "Transcript B", "Transcript C")
  )

  recode_plan <- list(
    c_positive = c("c_positive_impact", "c_joy", "c_hope")
  )
  relabels <- list(
    c_positive = "Positive affect"
  )

  result <- recode_themes(raw, recode_plan, relabel_vars = relabels)
  data_recode <- result[["data_recode"]]
  codebook_recode <- result[["codebook_recode"]]
  expect_identical(data_recode, result[["data_merged"]])
  expect_identical(codebook_recode, result[["codebook_merged"]])

  # 1. Result keeps original excerpt metadata and adds recoded logical codes
  expect_setequal(
    names(data_recode),
    c("excerpt_copy", "excerpt_creator", "media_title", "c_positive", "c_negative_impact")
  )
  expect_equal(nrow(data_recode), nrow(raw))

  # 2. Logical OR is applied across the requested source variables
  expect_equal(
    data_recode[["c_positive"]],
    c(TRUE, FALSE, TRUE),
    ignore_attr = TRUE
  )

  # 3. Codes not recoded remain unchanged (both values and lack of label)
  expect_equal(
    data_recode[["c_negative_impact"]],
    raw[["c_negative_impact"]],
    ignore_attr = TRUE
  )
  expect_null(labelled::var_label(data_recode[["c_negative_impact"]]))

  # 4. Labels update: custom label applied when supplied
  expect_identical(labelled::var_label(data_recode[["c_positive"]]), "Positive affect")

  # 5. Codebook reflects updated labels and types
  codebook_positive <- dplyr::filter(codebook_recode, variable == "c_positive")
  expect_equal(codebook_positive$label, "Positive affect")
  expect_equal(codebook_positive$type, "logical")

  codebook_negative <- dplyr::filter(codebook_recode, variable == "c_negative_impact")
  expect_equal(codebook_negative$label, "c_negative_impact")
  expect_equal(codebook_negative$type, "logical")
})

# Test 2: recode_themes() drops source variables but preserves overlapping names
test_that("recode_themes() drops source variables but preserves overlapping names", {
  raw <- dplyr::tibble(
    excerpt_copy = c("Text A", "Text B"),
    c_positive = c(TRUE, FALSE),
    c_joy = c(TRUE, TRUE)
  )
  recode_plan <- list(
    c_positive = c("c_positive", "c_joy")
  )
  result <- recode_themes(raw, recode_plan)
  data_recode <- result[["data_recode"]]

  # 1. Original source column `c_joy` is dropped after recoding
  expect_false("c_joy" %in% names(data_recode))

  # 2. Overlapping name `c_positive` remains with combined logical values
  expect_true("c_positive" %in% names(data_recode))
  expect_equal(
    data_recode[["c_positive"]],
    c(TRUE, TRUE),
    ignore_attr = TRUE
  )

  # 3. Default label falls back to the variable name
  expect_identical(labelled::var_label(data_recode[["c_positive"]]), "c_positive")
})

test_that("recode_themes() errors when source variables are missing", {
  raw <- dplyr::tibble(
    c_positive = c(TRUE, FALSE)
  )
  recode_plan <- list(
    c_positive = c("c_positive", "c_joy")
  )

  expect_error(
    recode_themes(raw, recode_plan),
    "Some variables for c_positive not found in dataset",
    fixed = TRUE
  )
})

test_that("recode_themes() method dispatch works with tibble inputs when dplyr is attached", {
  needs_detach <- !("package:dplyr" %in% search())
  if (needs_detach) {
    library(dplyr)
    on.exit(detach("package:dplyr", character.only = TRUE), add = TRUE)
  }

  raw <- tibble::tibble(
    excerpt_copy = c("A", "B"),
    c_positive_impact = c(TRUE, FALSE),
    c_joy = c(TRUE, TRUE)
  )

  result <- recode_themes(
    data = raw,
    recodes = list(c_positive = c("c_positive_impact", "c_joy")),
    relabel_vars = list(c_positive = "Positive affect")
  )

  expect_s3_class(result$data_recode, "tbl_df")
  expect_true("c_positive" %in% names(result$data_recode))
  expect_equal(
    result$data_recode[["c_positive"]],
    c(TRUE, TRUE),
    ignore_attr = TRUE
  )
  expect_identical(
    labelled::var_label(result$data_recode[["c_positive"]]),
    "Positive affect"
  )
})

test_that("recode_themes() handles haven labelled columns (e.g., from .dta files)", {
  raw <- tibble::tibble(
    c_emotional_regulation = haven::labelled(
      c(1, 0, 1),
      labels = c(`not coded` = 0, coded = 1)
    ),
    c_emotion_identification = haven::labelled(
      c(0, 1, 1),
      labels = c(`not coded` = 0, coded = 1)
    )
  )

  result <- recode_themes(
    data = raw,
    recodes = list(
      c_emotional_growth = c("c_emotional_regulation", "c_emotion_identification")
    )
  )

  expect_identical(names(result$data_recode), "c_emotional_growth")
  expect_s3_class(result$data_recode, "tbl_df")
  expect_equal(
    result$data_recode[["c_emotional_growth"]],
    c(TRUE, TRUE, TRUE),
    ignore_attr = TRUE
  )
  expect_identical(
    labelled::var_label(result$data_recode[["c_emotional_growth"]]),
    "c_emotional_growth"
  )
})
