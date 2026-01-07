#' Topic modeling on excerpts for a given code
#'
#' Runs a Latent Dirichlet Allocation (LDA) topic model on excerpts filtered
#' by a selected code column. Users can specify the number of topics, the
#' number of top terms per topic, and optionally provide custom stop words
#' in addition to the standard English stop words. The function outputs both
#' the top terms per topic in wide format and posterior probabilities of topic
#' membership for each excerpt.
#'
#' @param data A data.frame or tibble containing at least one column named
#'   \code{excerpt} and one or more code columns starting with \code{"c_"}.
#' @param code A string giving the name of the code column to filter on (e.g., \code{"c_belonging"}).
#' @param n_topics Number of topics to extract (default = 2).
#' @param n_terms Number of top terms to return per topic (default = 10).
#' @param custom_stopwords A character vector of additional stop words to remove
#'   (default = \code{NULL}).
#' @param sparse_threshold Proportion of sparsity for term removal (passed to
#'   \code{\link[tm]{removeSparseTerms}}, default = 0.99).
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{top_terms}: A tibble where each column corresponds to a topic
#'   (\code{topic_1}, \code{topic_2}, ...) and rows give the top terms by rank.
#'   \item \code{posterior}: A tibble containing the original excerpts with
#'   their posterior topic probabilities.
#' }
#'
#' @examples
#' library(tibble)
#'
#' # Example dataset
#' df <- tibble(
#'   excerpt = c(
#'     "I felt connected to peers and friends.",
#'     "We should normalize conversations about mental health.",
#'     "My teachers helped me belong at school.",
#'     "I am comfortable asking for help if I need it."
#'   ),
#'   c_belonging = c(TRUE, FALSE, TRUE, FALSE),
#'   c_destigmatization = c(FALSE, TRUE, FALSE, TRUE)
#' )
#'
#' # Run topic model on excerpts coded as belonging
#' tm <- topicmodel(df, code = "c_belonging", n_topics = 2, n_terms = 5)
#'
#' # View top terms
#' tm$top_terms
#'
#' # View posterior probabilities
#' tm$posterior
#'
#' # Run with custom stopwords (e.g. exclude 'school')
#' tm2 <- topicmodel(df, code = "c_belonging", n_topics = 2,
#'                   n_terms = 5, custom_stopwords = c("school"))
#'
#' @importFrom tidytext stop_words
#' @importFrom topicmodels LDA terms posterior
#' @importFrom tm VCorpus VectorSource DocumentTermMatrix removeSparseTerms
#' @importFrom tibble tibble
#' @export
