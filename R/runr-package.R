#' runr: Run R Experiments
#'
#' Helper functions and driver scripts for running large-scale, corpus-wide R
#' experiments: extracting runnable code from packages, running it under a
#' controlled R process, and reading back the results of GNU parallel jobs.
#'
#' @importFrom stats na.omit
#' @importFrom utils available.packages getParseData head install.packages
#'   installed.packages untar
#' @keywords internal
"_PACKAGE"

# Column names referenced through dplyr's non-standard evaluation, plus the
# symbols of the rex DSL used in split_on_line_directives(). None of these are
# ordinary bindings, so R CMD check cannot see where they come from.
utils::globalVariables(c(
  # cloc / sloc columns
  "language", "filename", "blank", "code",
  "blank.x", "blank.y", "comment.x", "comment.y", "code.x", "code.y",
  # extract_package_code columns
  "type", "test_driver", "test_name",
  # read_parallel_log columns
  "starttime", "jobruntime",
  # rex DSL
  "start", "any_spaces", "spaces", "digit", "quotes", "anything", "capture"
))
