#' Recode logical code variables and optionally relabel them
#'
#' @description
#' recode_themes() combines multiple logical (TRUE/FALSE) code variables into
#' new composite variables. For each new variable, the function computes a
#' logical OR across the specified source variables—meaning the new variable is
#' TRUE when any source variable is TRUE. Optionally, descriptive labels
#' can be supplied for the newly created variables, and a codebook summarizing
#' the resulting dataset is returned.
#'
#' @param data A data frame, tibble, or haven-labelled data frame (for example,
#' the output from clean_data() or a dataset read from a .dta file)
#' containing logical code variables.
#' @param recodes A named list where each name is a new variable to create and
#' each value is a character vector of existing variable names to combine.
#' For example:
#' list(c_help = c("c_support", "c_assist"), c_stress = c("c_anxiety", "c_pressure"))
#' @param relabel_vars Optional named list of variable labels for the new
#' composite variables in the format
#' list(new_var = "New variable label"). If omitted, the new variable names
#' are used as default labels.
#'
#' @return A list with four elements:
#' \describe{
#' \item{data_recode}{A data frame containing the updated dataset with recoded
#' logical code variables.}
#' \item{codebook_recode}{A data frame summarizing variable names, labels (if
#' available), and data types.}
#' \item{data_merged}{Alias for \code{data_recode} retained for backward compatibility.}
#' \item{codebook_merged}{Alias for \code{codebook_recode} retained for backward compatibility.}
#' }
#'
#' @details
#' The function first verifies that the specified source variables exist in the
#' dataset. It then creates the new logical variables defined by recodes,
#' assigns user-specified or default labels, removes the original source
#' variables (unless one overlaps with a new variable name), and builds a
#' codebook summarizing the recoded dataset.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#' c_support = c(TRUE, FALSE, TRUE),
#' c_assist = c(FALSE, TRUE, TRUE),
#' c_anxiety = c(TRUE, FALSE, FALSE),
#' c_pressure = c(FALSE, TRUE, FALSE)
#' )
#'
#' # Define recodes
#' recode_plan <- list(
#' c_help = c("c_support", "c_assist"),
#' c_stress = c("c_anxiety", "c_pressure")
#' )
#'
#' # Run recode_themes() with new labels
#' result <- recode_themes(
#' data = df,
#' recodes = recode_plan,
#' relabel_vars = list(
#' c_help = "Mentions of helping or supporting others",
#' c_stress = "Mentions of stress or pressure"
#' )
#' )
#'
#' # Extract recoded data and codebook
#' data_recode <- result$data_recode
#' codebook_recode <- result$codebook_recode
#'
#' # View recoded dataset
#' head(data_recode)
#'
#' # View codebook
#' head(codebook_recode)
#'
#' @importFrom labelled var_label
#' @export
recode_themes <- function(data, recodes, relabel_vars = NULL) {
  if (!inherits(data, c("data.frame", "tbl_df", "tbl"))) {
    stop("`recode_themes()` only supports data frames or tibbles.", call. = FALSE)
  }
  recode_themes_data_frame_impl(
    data = data,
    recodes = recodes,
    relabel_vars = relabel_vars
  )
}

recode_themes_data_frame_impl <- function(data, recodes, relabel_vars = NULL) {
if (missing(recodes)) {
stop("recodes must be provided when recoding data-frame objects.", call. = FALSE)
}

if (!is.list(recodes) || is.null(names(recodes)) || any(names(recodes) == "")) {
stop("recodes must be a named list of variables to combine.", call. = FALSE)
}

all_from_vars <- c()

for (new_var in names(recodes)) {
from_vars <- recodes[[new_var]]

if (!all(from_vars %in% names(data))) {
  stop(paste("Some variables for", new_var, "not found in dataset"))
}

data[[new_var]] <- apply(data[from_vars], 1, function(x) any(x == TRUE, na.rm = TRUE))
all_from_vars <- c(all_from_vars, setdiff(from_vars, new_var))

if (!is.null(relabel_vars) && new_var %in% names(relabel_vars)) {
  labelled::var_label(data[[new_var]]) <- relabel_vars[[new_var]]
} else {
  labelled::var_label(data[[new_var]]) <- new_var
}
}

data_recode <- data[, !names(data) %in% all_from_vars, drop = FALSE]

codebook_recode <- data.frame(
variable = names(data_recode),
label = sapply(names(data_recode), function(col) {
lbl <- labelled::var_label(data_recode[[col]])
if (is.null(lbl) || lbl == "") col else lbl
}),
type = sapply(data_recode, function(x) class(x)[1]),
stringsAsFactors = FALSE
)

list(
data_recode = data_recode,
codebook_recode = codebook_recode,
data_merged = data_recode,
codebook_merged = codebook_recode
)
}
