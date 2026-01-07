# Test 1: min functionality - clean variables in dataset and codebook
test_that("clean_data() names to lowercase,
          no spaces in columns,
          object with dataset and codebook,
          assigns coder_rank", {
  raw <- dplyr::tibble(
    "Excerpt Copy" = c("Text A", "Text B", "Text C"),
    "Some Range" = 1:3,
    "Code Theme Applied" = c(TRUE, FALSE, TRUE),
    "Excerpt Creator" = c("Coder A", "Coder B", "Coder C"),
    "Media Title" = c("Transcript A", "Transcript B", "Transcript C")
  )

  preferred_coders <- c("Coder A", "Coder B", "Coder C")

  result <- clean_data(
    excerpts = raw,
    preferred_coders = preferred_coders
  )

  # 1. Object has cleaned dataset and codebook
  expect_type(result, "list")
  expect_true("data" %in% names(result))
  expect_true("codebook" %in% names(result))
            
  # 2. All column names in `data` are lowercase and contain no spaces
  expect_true(all(names(result$data) == tolower(names(result$data))))
  expect_false(any(grepl(" ", names(result$data))))
            
  # 3. There is an 'excerpt' column in dataset and codebook has apt columns
  expect_true("excerpt" %in% names(result$data))
  expect_equal(ncol(result$codebook), 3)
  expect_equal(names(result$codebook), c("variable", "label", "type"))
            
  # 4. coder_rank column exists and is integer
  expect_true("coder_rank" %in% names(result$data))
  expect_type(result$data$coder_rank, "integer")
            
  # 5. range and weight columns are dropped
  expect_false(any(grepl("range", names(result$data))))
  expect_false(any(grepl("weight", names(result$data))))
          })

          
# Test 2: rename_vars and rename labels options
test_that("rename_vars and relabel_vars correctly rename variables and update labels", {
  raw <- dplyr::tibble(
    "Excerpt Copy" = c("Text A", "Text B", "Text C"),
    "Some Range" = 1:3,
    "Code Theme Applied" = c(TRUE, FALSE, TRUE),
    "Excerpt Creator" = c("Coder A", "Coder B", "Coder C"),
    "Media Title" = c("Transcript A", "Transcript B", "Transcript C")
  )

  preferred_coders <- c("Coder A", "Coder B", "Coder C")

  # Rename `media_title` -> `source_doc` and relabel it
  result <- clean_data(
    excerpts = raw,
    preferred_coders = preferred_coders,
    rename_vars = list(source_doc = "media_title",
                       coder = "excerpt_creator"),
    relabel_vars = list(source_doc = "source document",
                        coder = "name of coder")
  )

  # 1. Data has the renamed column
  expect_true("source_doc" %in% names(result$data))
  expect_false("media_title" %in% names(result$data))  # original should be gone

  # 2. Codebook uses the renamed variable name
  expect_true("source_doc" %in% result$codebook$variable)

  # 3. Codebook label matches relabel_vars input
  label_value <- result$codebook$label[result$codebook$variable == "source_doc"]
  expect_equal(label_value, "source document")
})

# Test 3: output_type option
test_that("output_type writes requested file formats", {
  testthat::skip_if_not_installed("openxlsx")
  testthat::skip_if_not_installed("haven")
  raw <- dplyr::tibble(
    "Excerpt Copy" = c("Text A", "Text B"),
    "Code Theme Applied" = c(TRUE, FALSE),
    "Excerpt Creator" = c("Coder A", "Coder B"),
    "Media Title" = c("Transcript A", "Transcript B")
  )
  preferred_coders <- c("Coder A", "Coder B")
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  tmp_dta <- tempfile(fileext = ".dta")
  on.exit(unlink(c(tmp_xlsx, tmp_dta)), add = TRUE)
  invisible(clean_data(
    excerpts = raw,
    preferred_coders = preferred_coders,
    output_path = tmp_xlsx,
    output_type = "xlsx"
  ))
  expect_true(file.exists(tmp_xlsx))
  expect_gt(file.info(tmp_xlsx)$size, 0)
  invisible(clean_data(
    excerpts = raw,
    preferred_coders = preferred_coders,
    output_path = tmp_dta,
    output_type = "dta"
  ))
  expect_true(file.exists(tmp_dta))
  expect_gt(file.info(tmp_dta)$size, 0)
})


