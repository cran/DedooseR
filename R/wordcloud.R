#' Generate a word cloud for excerpts by code
#'
#' Creates a word cloud of words from all excerpts where a given code is applied.
#' Common English stop words, user-supplied stop words, and punctuation are removed.
#'
#' @param data A data.frame or tibble containing at least one \code{excerpt} column
#'   and one or more code columns starting with \code{"c_"}.
#' @param code A string giving the name of the code column to filter on (e.g. "c_belonging").
#' @param max_words Maximum number of words to display in the word cloud (default = 100).
#' @param custom_stopwords A character vector of additional stop words to remove
#'   (default = \code{NULL}).
#'
#' @return An interactive word cloud (from \pkg{wordcloud2}).
#'
#' @examples
#' library(dplyr)
#' df <- tibble::tibble(
#'   excerpt = c(
#'     "I felt connected to peers and friends.",
#'     "We should normalize conversations about mental health.",
#'     "My teachers helped me belong at school.",
#'     "I am comfortable talking about suicide prevention."
#'   ),
#'   c_belonging = c(TRUE, FALSE, TRUE, FALSE),
#'   c_destigmatization = c(FALSE, TRUE, FALSE, FALSE)
#' )
#'
#' # Word cloud for belonging excerpts
#' wordcloud(df, "c_belonging")
#'
#' # With custom stop words
#' wordcloud(df, "c_belonging", custom_stopwords = c("connected", "school"))
#'
#' @importFrom tidytext unnest_tokens
#' @importFrom wordcloud2 wordcloud2
#' @export
wordcloud <- function(data, code, max_words = 100, custom_stopwords = NULL) {
  stopifnot("excerpt" %in% names(data))
  stopifnot(code %in% names(data))

  # filter excerpts for selected code
  excerpts <- data[data[[code]] == TRUE, "excerpt", drop = TRUE]
  if (length(excerpts) == 0) {
    stop("No excerpts found for code: ", code)
  }

  # build stopword list
  all_stopwords <- tidytext::stop_words$word
  if (!is.null(custom_stopwords)) {
    all_stopwords <- unique(c(all_stopwords, custom_stopwords))
  }

  # tokenize, remove stop words and punctuation
  tokens <- tibble::tibble(text = excerpts) %>%
    tidytext::unnest_tokens(word, text) %>%
    dplyr::filter(!word %in% all_stopwords) %>%
    dplyr::filter(!grepl("^[[:punct:]]+$", word)) %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::slice_head(n = max_words)

  if (nrow(tokens) == 0) {
    stop("No valid words found for code: ", code)
  }

  # make word cloud
  wordcloud2::wordcloud2(tokens)
}
