#' View Qualitative Excerpts by Code
#'
#' @description
#' Displays qualitative excerpts interactively in a searchable, filterable data table.
#' Each row represents an excerpt associated with one or more qualitative codes.
#' Code columns are automatically detected as those starting with `"c_"`, and their
#' variable labels (if available) are used as readable code names.
#'
#' This function is primarily designed for exploring and reviewing coded qualitative data,
#' allowing users to filter by code and quickly browse the corresponding excerpts.
#'
#' @param data A data frame containing at least one text column named `excerpt` and
#'   one or more logical code columns prefixed with `"c_"`. Each logical column
#'   represents whether a code was applied (`TRUE`/`FALSE`).
#'
#' @return
#' A [`DT::datatable()`][DT::datatable] object that displays:
#' - **Code:** readable code label or variable name
#' - **Excerpt:** associated qualitative text
#'
#' The output table includes:
#' - A dropdown filter for selecting specific codes
#' - Search boxes for column-wise filtering
#' - Responsive column widths and formatted text wrapping
#'
#' @details
#' - Variable labels are extracted from the `"label"` attribute of each code column
#'   (e.g., assigned via `haven::labelled` or `attr(x, "label") <- "Label"`).
#' - Only excerpts where a code is marked as `TRUE` are displayed.
#' - The table uses custom styling with a purple header and automatic text wrapping.
#'
#' @examples
#' library(dplyr)
#'
#' df <- tibble::tibble(
#'   excerpt = c(
#'     "I felt supported by my peers.",
#'     "Teachers really listened to us.",
#'     "I learned a lot about myself."
#'   ),
#'   c_support = c(TRUE, TRUE, FALSE),
#'   c_growth = c(FALSE, FALSE, TRUE)
#' )
#' attr(df$c_support, "label") <- "Peer/Teacher Support"
#' attr(df$c_growth, "label") <- "Personal Growth"
#'
#' # View excerpts interactively
#' if (interactive()) view_excerpts(df)
#'
#' @importFrom purrr map_chr
#' @importFrom dplyr select filter mutate all_of
#' @importFrom tidyr pivot_longer
#' @importFrom DT datatable formatStyle JS
#' @export
view_excerpts <- function(data) {
  stopifnot(requireNamespace("DT", quietly = TRUE))
  stopifnot(requireNamespace("tidyr", quietly = TRUE))
  stopifnot("excerpt" %in% names(data))

  # detect code columns
  code_cols <- grep("^c_", names(data), value = TRUE)
  if (length(code_cols) == 0) stop("No code columns found (must start with 'c_').")

  # name → label lookup
  label_lookup <- purrr::map_chr(code_cols, function(x) {
    lbl <- attr(data[[x]], "label")
    if (is.null(lbl) || lbl == "") x else lbl
  })
  names(label_lookup) <- code_cols

  # reshape to long: one row per excerpt per code (TRUE only)
  long_data <- tidyr::pivot_longer(
    data,
    cols = dplyr::all_of(code_cols),
    names_to = "code",
    values_to = "applied"
  ) %>%
    dplyr::filter(.data$applied == TRUE) %>%
    dplyr::mutate(code = unname(label_lookup[as.character(.data$code)])) %>%
    dplyr::select(code, excerpt)

  # interactive datatable
  DT::datatable(
    long_data,
    escape = FALSE,
    filter = "top",
    options = list(
      pageLength = 10,
      autoWidth = FALSE,
      columnDefs = list(
        list(width = '150px', targets = 0),
        list(width = '600px', targets = 1)
      ),
      initComplete = DT::JS(
        "function(settings, json) {",
        "  this.api().columns([0]).every(function() {",
        "    var column = this;",
        "    var select = $('<select><option value=\"\"></option></select>')",
        "      .css('margin-top', '5px')",
        "      .appendTo($(column.header()))",
        "      .on('change', function() {",
        "        var val = $.fn.dataTable.util.escapeRegex($(this).val());",
        "        column.search(val ? '^' + val + '$' : '', true, false).draw();",
        "      });",
        "    column.data().unique().sort().each(function(d, j) {",
        "      select.append('<option value=\"' + d + '\">' + d + '</option>');",
        "    });",
        "  });",
        "}"
      ),
      headerCallback = DT::JS('function(thead, data, start, end, display) {
        $(thead).find("th").css("background-color", "#330662").css("color", "white");
      }')
    ),
    rownames = FALSE,
    colnames = c("Code", "Excerpt")
  ) %>%
    DT::formatStyle(
      'code',
      whiteSpace = 'nowrap'
    ) %>%
    DT::formatStyle(
      'excerpt',
      whiteSpace = 'normal',
      wordBreak = 'break-word'
    )
}
