# Global variables used in NSE contexts to suppress R CMD check warnings

#' @importFrom utils globalVariables
#' @importFrom dplyr %>% across everything desc
#' @importFrom ggplot2 scale_fill_gradient
NULL

utils::globalVariables(c(
  # Variables used in coccur.R
  "Frequency", "Label", "Code1", "Code2",

  # Variables used in create_saturation_tracking.R
  "Excerpt Range", "Excerpt Date", "Resource Creator", "Resource Date",
  "Excerpt Creator", "coder_rank", "Media Title", "Code: Priority excerpt Applied",
  "Excerpt Copy", "Code: Heterogeniety Applied", "Applied", "Code",
  "Priority_Applied", "Heterogeneity_Applied", "Priority_Count", "Heterogeneity_Count",

  # Variables used in plot_saturation.R and set_saturation.R
  "Count", "Type",

  # Additional variables flagged by R CMD check
  "Meets", "Set", "applied", "code", "code_label", "excerpt_copy", "excerpt",
  "excerpt_creator", "media_title", "n_media_titles",
  "name", "prop_media_titles", "weight", "word", "text", "where", "setNames",
  "na.omit", "stats",

  # Tidyverse pronouns/placeholders
  ".", ".data", "count"
))
