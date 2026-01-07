#' Create a Summary Table and Plot of Code Frequencies
#'
#' @description
#' Summarizes how often each qualitative code (represented by logical 0/1 variables)
#' appears across excerpts or media titles. Optionally produces a frequency table
#' and visualization of code distributions.
#'
#' This function automatically handles Stata-labelled (`haven_labelled`) or numeric 0/1
#' variables by converting them to logicals. You can also pass in a custom codebook to
#' apply human-readable code labels.
#'
#' @param excerpts A data frame containing at least one logical or 0/1 variable
#'   representing a code, and a column named `media_title` that identifies the source
#'   document or excerpt.
#' @param table_min_count Minimum number of excerpts required for a code to appear
#'   in the summary table. Default is 1.
#' @param table_min_prop Optional proportion threshold (relative to the maximum count)
#'   for including codes in the table. Default is `NULL`.
#' @param plot Logical; whether to generate a plot visualizing code frequencies.
#'   Default is `FALSE`.
#' @param plot_min_count Minimum number of excerpts required for a code to appear
#'   in the plot. Defaults to `table_min_count`.
#' @param plot_min_prop Optional proportion threshold (relative to the maximum count)
#'   for including codes in the plot. Defaults to `table_min_prop`.
#' @param output_type The format for the output table. One of `"tibble"`,
#'   `"kable"`, or `"datatable"`. Default is `"tibble"`.
#' @param exclude Optional character vector of variable names to exclude from analysis.
#' @param plot_metric The metric to visualize. One of `"count"`, `"prop"`, or `"both"`.
#'   Default is `"count"`.
#' @param fill_color Color for plot bars. Default is `"steelblue"`.
#' @param use_labels Logical; if `TRUE`, uses a supplied `codebook` to display
#'   descriptive labels for codes instead of variable names. Default is `FALSE`.
#' @param codebook Optional data frame with two columns:
#'   - `variable`: the variable names in the dataset
#'   - `label`: the corresponding human-readable label for each code.
#'   Required when `use_labels = TRUE`.
#'
#' @details
#' The function first identifies all logical (or 0/1 numeric) columns in `excerpts`
#' and calculates:
#' - `count`: total number of excerpts where the code is applied
#' - `n_media_titles`: number of distinct media titles containing the code
#' - `prop_media_titles`: proportion of media titles containing the code (relative to max)
#'
#' The table can be output as a tibble, formatted table (`knitr::kable`), or
#' interactive data table (`DT::datatable`).
#'
#' When `plot = TRUE`, the function generates a ggplot2 bar chart showing either
#' code counts, proportions, or both (dual-axis view).
#'
#' @return
#' If `plot = FALSE`, returns a table in the selected `output_type` format.
#' If `plot = TRUE`, invisibly returns a list with two elements:
#' \describe{
#'   \item{table}{A table of summarized code frequencies.}
#'   \item{plot}{A `ggplot` object visualizing the results.}
#' }
#'
#' @examples
#' # Example 1: Basic usage without a codebook
#' df <- data.frame(
#'   media_title = c("Doc1", "Doc2", "Doc3", "Doc4"),
#'   code_a = c(TRUE, FALSE, TRUE, TRUE),
#'   code_b = c(FALSE, TRUE, TRUE, FALSE)
#' )
#'
#' create_code_summary(df, plot = TRUE)
#'
#' # Example 2: Using a codebook for readable labels
#' codebook <- data.frame(
#'   variable = c("code_a", "code_b"),
#'   label = c("Community Engagement", "Policy Support")
#' )
#'
#' create_code_summary(
#'   df,
#'   use_labels = TRUE,
#'   codebook = codebook,
#'   plot = TRUE,
#'   plot_metric = "both"
#' )
#'
#' # Example 3: Excluding a code and outputting as datatable
#' create_code_summary(
#'   df,
#'   exclude = "code_b",
#'   output_type = "datatable"
#' )
#'
#' @importFrom dplyr select all_of filter group_by summarise mutate arrange desc n n_distinct
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal scale_y_continuous sec_axis
#' @importFrom knitr kable
#' @importFrom DT datatable
#' @importFrom purrr map_chr
#' @export
create_code_summary <- function(
    excerpts,
    table_min_count = 1,
    table_min_prop = NULL,
    plot = FALSE,
    plot_min_count = NULL,
    plot_min_prop = NULL,
    output_type = c("tibble", "kable", "datatable"),
    exclude = NULL,
    plot_metric = c("count", "prop", "both"),
    fill_color = "steelblue",
    use_labels = FALSE,
    codebook = NULL # dataframe with columns: variable, label
) {
  output_type <- match.arg(output_type)
  plot_metric <- match.arg(plot_metric)

  # --- Validate inputs ---
  if (!is.data.frame(excerpts)) stop("`excerpts` must be a data frame.")
  if (!"media_title" %in% names(excerpts)) stop("`excerpts` must contain a `media_title` column.")

  # --- Validate codebook if labels are to be used ---
  if (use_labels) {
    if (is.null(codebook)) stop("You must provide a `codebook` dataframe when `use_labels = TRUE`.")
    required_cols <- c("variable", "label")
    if (!all(required_cols %in% names(codebook))) {
      stop("`codebook` must contain columns named `variable` and `label`.")
    }
  }

  # --- Safeguard: Convert 0/1 numerics or labelled to logical ---
  for (col in names(excerpts)) {
    x <- excerpts[[col]]
    if (inherits(x, "haven_labelled")) {
      vals <- unique(na.omit(as.numeric(x)))
      if (all(vals %in% c(0, 1))) excerpts[[col]] <- as.logical(as.numeric(x))
    } else if (is.numeric(x) && all(na.omit(x) %in% c(0, 1))) {
      excerpts[[col]] <- as.logical(x)
    }
  }

  # --- Identify logical columns (codes) ---
  code_columns <- colnames(excerpts)[vapply(excerpts, is.logical, logical(1))]

  # --- Apply exclusions ---
  if (!is.null(exclude)) {
    exclude <- intersect(exclude, code_columns)
    code_columns <- setdiff(code_columns, exclude)
  }
  if (length(code_columns) == 0) stop("No logical (code) columns found after exclusions.")

  # --- Create name → label lookup ---
  if (use_labels) {
    # Merge from codebook
    label_lookup <- setNames(codebook$label, codebook$variable)
    label_lookup <- label_lookup[names(label_lookup) %in% code_columns]
    # fallback: variables not in codebook use their names
    missing_codes <- setdiff(code_columns, names(label_lookup))
    if (length(missing_codes) > 0) {
      warning("Some codes missing from codebook: ", paste(missing_codes, collapse = ", "))
      label_lookup[missing_codes] <- missing_codes
    }
  } else {
    # fallback to haven labels if available
    label_lookup <- purrr::map_chr(code_columns, function(x) {
      lbl <- attr(excerpts[[x]], "label")
      if (is.null(lbl) || is.na(lbl) || lbl == "") x else lbl
    })
    names(label_lookup) <- code_columns
  }

  # --- Summarize code frequencies ---
  total_counts <- excerpts %>%
    dplyr::select("media_title", dplyr::all_of(code_columns)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(code_columns),
      names_to = "code",
      values_to = "applied"
    ) %>%
    dplyr::filter(applied == TRUE) %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(
      count = dplyr::n(),
      n_media_titles = dplyr::n_distinct(media_title),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      prop_media_titles = round(n_media_titles / max(n_media_titles, na.rm = TRUE), 2),
      code_label = dplyr::recode(code, !!!label_lookup)
    ) %>%
    dplyr::select(
      code_label,
      count,
      n_media_titles,
      prop_media_titles
    ) %>%
    dplyr::rename(code = code_label)

  # --- Apply table filters ---
  if (!is.null(table_min_prop)) {
    max_val <- max(total_counts$count, na.rm = TRUE)
    total_counts <- dplyr::filter(total_counts, count >= table_min_prop * max_val)
  }
  total_counts <- dplyr::filter(total_counts, count >= table_min_count)

  # --- Output table ---
  caption_text <- paste("Total Code Counts (min_count =", table_min_count, ")")
  table_out <- switch(
    output_type,
    tibble = total_counts,
    kable = knitr::kable(total_counts, caption = caption_text),
    datatable = DT::datatable(
      total_counts,
      caption = caption_text,
      options = list(pageLength = 30, autoWidth = TRUE)
    )
  )

  # --- Plot section ---
  if (plot) {
    if (is.null(plot_min_count)) plot_min_count <- table_min_count
    if (is.null(plot_min_prop)) plot_min_prop <- table_min_prop

    plot_df <- dplyr::filter(total_counts, count >= plot_min_count)
    if (!is.null(plot_min_prop)) {
      max_val <- max(plot_df$count, na.rm = TRUE)
      plot_df <- dplyr::filter(plot_df, count >= plot_min_prop * max_val)
    }
    plot_df <- dplyr::arrange(plot_df, dplyr::desc(count))

    if (plot_metric == "count") {
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(code, count),
        y = count
      )) +
        ggplot2::geom_col(fill = fill_color) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = "Code",
          y = "Excerpt Frequency",
          title = "Code Counts"
        ) +
        ggplot2::theme_minimal()

    } else if (plot_metric == "prop") {
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(code, prop_media_titles),
        y = prop_media_titles
      )) +
        ggplot2::geom_col(fill = fill_color) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = "Code",
          y = "Proportion of Media Titles",
          title = "Code Frequencies by Media Title Coverage"
        ) +
        ggplot2::theme_minimal()

    } else if (plot_metric == "both") {
      scale_factor <- max(plot_df$count, na.rm = TRUE) /
        max(plot_df$prop_media_titles, na.rm = TRUE)

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(code, count),
        y = count
      )) +
        ggplot2::geom_col(fill = fill_color) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
          name = "Excerpt Frequency",
          sec.axis = ggplot2::sec_axis(~ . / scale_factor,
                                       name = "Proportion of Media Titles")
        ) +
        ggplot2::labs(
          x = "Code",
          title = "Code Frequencies: Counts and Proportions"
        ) +
        ggplot2::theme_minimal()
    }

    print(table_out)
    return(invisible(list(table = table_out, plot = p)))
  }

  print(table_out)
  return(invisible(table_out))
}
