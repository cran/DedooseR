#' Create a Code Co-occurrence Matrix and Network Plot
#'
#' @description
#' Builds a co-occurrence matrix showing how often qualitative codes appear together
#' within the same unit (e.g., transcript, document, or media title). The function
#' expects a coded dataset (`excerpts`) and returns both a formatted matrix and
#' (optionally) a network visualization. The returned matrix can be displayed as
#' raw counts or column-wise proportions, whereas the network plot always reflects
#' the underlying raw counts.
#'
#' @details
#' The function identifies columns beginning with `"c_"` as code variables.
#' It computes co-occurrences by summing pairwise intersections of codes across
#' all unique `media_title` units. The diagonal represents the marginal frequencies
#' (the number of transcripts where each code appears).
#'
#' The resulting matrix can be output as a tibble, a simple data frame, or a
#' formatted HTML table via `knitr::kable`. If `plot = TRUE`, the function also
#' returns a network visualization of code co-occurrences using `ggraph` and `igraph`.
#' Edges are filtered via the `edge_min` threshold, and nodes without any remaining
#' connections are removed from the plot.
#'
#' @param excerpts Data frame containing coded excerpts, with a column named
#'   `media_title` and code columns prefixed with `"c_"`.
#' @param min_bold Minimum value for bold highlighting in HTML table output (if
#'   `output = "kable"`). Default is `10`.
#' @param scale Whether to display raw counts (`"count"`) or column-wise conditional
#'   proportions (`"prop"`) in the returned matrix. The network plot always uses
#'   raw counts. Default is `"count"`.
#' @param output The format of the co-occurrence matrix output. One of `"kable"`,
#'   `"tibble"`, or `"data.frame"`. Default is `"kable"`.
#' @param plot Logical; whether to produce a network visualization. Default is `TRUE`.
#' @param edge_min Minimum edge weight (in counts) for displaying connections in the plot.
#'   Default is `10`.
#' @param layout Graph layout for network visualization (passed to `ggraph::ggraph`).
#'   Common options include `"circle"`, `"fr"`, or `"kk"`. Default is `"circle"`.
#' @param edge_color_low,edge_color_high Color gradient for edge weights in the plot.
#'   Default is `"lightgray"` to `"purple"`.
#' @param node_color Color for node points in the network plot. Default is `"lightblue"`.
#' @param use_labels Logical; if `TRUE`, replaces code variable names with descriptive
#'   labels from a provided `codebook`. Default is `FALSE`.
#' @param codebook Optional data frame with columns:
#'   - `variable`: the code variable name (e.g., `"c_family"`)
#'   - `label`: the descriptive name for the code (e.g., `"Family Connectedness"`).
#'   Required when `use_labels = TRUE`.
#'
#' @return
#' A named list with two elements:
#' \describe{
#'   \item{matrix}{A tibble, data frame, or formatted HTML table of the co-occurrence matrix.}
#'   \item{plot}{A `ggplot` object visualizing the co-occurrence network (if `plot = TRUE`).}
#' }
#'
#' @examples
#' # Example 1: Basic co-occurrence matrix from excerpts
#' df <- data.frame(
#'   media_title = c("Doc1", "Doc2", "Doc3"),
#'   c_hope = c(1, 0, 1),
#'   c_family = c(1, 1, 0),
#'   c_school = c(0, 1, 1)
#' )
#'
#' result <- cooccur(
#'   excerpts = df,
#'   scale = "count",
#'   output = "tibble",
#'   plot = TRUE
#' )
#'
#' result$matrix  # Co-occurrence matrix
#' result$plot    # Network plot
#'
#' # Example 2: Use descriptive labels from a codebook and proportions in the table
#' codebook <- data.frame(
#'   variable = c("c_hope", "c_family", "c_school"),
#'   label = c("Hope & Optimism", "Family Connectedness", "School Belonging")
#' )
#'
#' labeled_result <- cooccur(
#'   excerpts = df,
#'   use_labels = TRUE,
#'   codebook = codebook,
#'   scale = "prop",
#'   output = "kable",
#'   plot = TRUE
#' )
#'
#' labeled_result$matrix
#' labeled_result$plot
#'
#' @importFrom dplyr group_by summarise across mutate everything
#' @importFrom tibble as_tibble
#' @importFrom kableExtra cell_spec kable_styling
#' @importFrom knitr kable
#' @importFrom igraph graph_from_adjacency_matrix delete_edges delete_vertices E V degree
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text scale_edge_width
#'   scale_edge_color_gradient
#' @importFrom ggplot2 theme_void
#' @export
cooccur <- function(
  excerpts = NULL,
  min_bold = 10,
  scale = c("count", "prop"),
  output = c("kable", "tibble", "data.frame"),
  plot = TRUE,
  edge_min = 10,
  layout = "circle",
  edge_color_low = "lightgray",
  edge_color_high = "purple",
  node_color = "lightblue",
  use_labels = FALSE,
  codebook = NULL
) {
  if (is.null(excerpts)) {
    stop("You must provide `excerpts`.")
  }
  scale <- match.arg(scale)
  output <- match.arg(output)

  if (!"media_title" %in% names(excerpts)) {
    stop("`excerpts` must contain a `media_title` column.")
  }

  code_columns <- grep("^c_", names(excerpts), value = TRUE)
  if (length(code_columns) == 0) {
    stop("No code columns found (columns must start with 'c_').")
  }

  # Collapse to transcript-level presence
  code_by_transcript <- excerpts %>%
    dplyr::group_by(media_title) %>%
    dplyr::summarise(
      across(all_of(code_columns), ~ as.integer(any(. == 1))),
      .groups = "drop"
    )

  # Build co-occurrence matrix (counts)
  code_matrix <- as.matrix(code_by_transcript[, -1, drop = FALSE])
  coccur_counts <- t(code_matrix) %*% code_matrix

  # Drop all-zero rows/cols
  keep <- which(rowSums(coccur_counts) > 0 | colSums(coccur_counts) > 0)
  coccur_counts <- coccur_counts[keep, keep, drop = FALSE]

  # Prepare matrix for output formatting
  if (scale == "prop") {
    marginals <- diag(coccur_counts)
    matrix_values <- sweep(coccur_counts, 2, marginals, "/")
    matrix_values <- round(matrix_values, 3)
    matrix_values[!is.finite(matrix_values)] <- 0
  } else {
    matrix_values <- coccur_counts
  }

  plot_matrix <- coccur_counts

  # Apply code labels if requested
  if (use_labels) {
    if (is.null(codebook)) {
      stop("You must provide a `codebook` dataframe when `use_labels = TRUE`.")
    }
    if (!all(c("variable", "label") %in% names(codebook))) {
      stop("`codebook` must have columns named `variable` and `label`.")
    }

    label_lookup <- setNames(codebook$label, codebook$variable)
    relabel_matrix <- function(mat) {
      matched_rows <- rownames(mat) %in% names(label_lookup)
      matched_cols <- colnames(mat) %in% names(label_lookup)
      rownames(mat)[matched_rows] <- label_lookup[rownames(mat)[matched_rows]]
      colnames(mat)[matched_cols] <- label_lookup[colnames(mat)[matched_cols]]
      mat
    }

    matrix_values <- relabel_matrix(matrix_values)
    plot_matrix <- relabel_matrix(plot_matrix)
  }

  # Convert to data frame
  coccur_df <- as.data.frame(matrix_values)
  rownames(coccur_df) <- rownames(matrix_values)

  # Format output
  if (output == "tibble") {
    matrix_out <- tibble::as_tibble(coccur_df, rownames = "code")
  } else if (output == "data.frame") {
    matrix_out <- coccur_df
  } else {
    if (scale == "count") {
      coccur_df_fmt <- coccur_df %>%
        dplyr::mutate(across(
          everything(),
          ~ ifelse(. >= min_bold,
                   kableExtra::cell_spec(., bold = TRUE),
                   as.character(.))
        ))
    } else {
      coccur_df_fmt <- coccur_df %>%
        dplyr::mutate(across(
          everything(),
          ~ ifelse(. >= min_bold,
                   kableExtra::cell_spec(sprintf("%.3f", .), bold = TRUE),
                   sprintf("%.3f", .))
        ))
    }
    rownames(coccur_df_fmt) <- rownames(coccur_df)
    matrix_out <- knitr::kable(
      coccur_df_fmt,
      format = "html",
      escape = FALSE,
      caption = paste(
        "Code Co-occurrence Matrix (Within Transcript)",
        ifelse(scale == "count", "Counts", "Proportions")
      ),
      align = "c"
    ) %>%
      kableExtra::kable_styling(
        full_width = FALSE,
        bootstrap_options = c("striped", "hover", "condensed")
      )
  }

  # Optional plot (always count-based)
  plot_out <- NULL
  if (plot) {
    g <- igraph::graph_from_adjacency_matrix(
      plot_matrix,
      mode = "undirected",
      weighted = TRUE,
      diag = FALSE
    )

    g <- igraph::delete_edges(g, igraph::E(g)[weight < edge_min])

    freq <- diag(plot_matrix)
    igraph::V(g)$freq <- freq[igraph::V(g)$name]

    g <- igraph::delete_vertices(g, which(igraph::degree(g) == 0))

    plot_out <- ggraph::ggraph(g, layout = layout) +
      ggraph::geom_edge_link(aes(width = weight, color = weight), alpha = 0.6) +
      ggraph::scale_edge_width(range = c(0.2, 2), guide = "none") +
      ggraph::scale_edge_color_gradient(
        low = edge_color_low,
        high = edge_color_high,
        guide = "none"
      ) +
      ggraph::geom_node_point(aes(size = freq), color = node_color) +
      ggraph::geom_node_text(aes(label = igraph::V(g)$name),
                             repel = TRUE,
                             max.overlaps = Inf) +
      ggplot2::theme_void()
  }

  list(matrix = matrix_out, plot = plot_out)
}
