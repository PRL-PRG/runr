#' Count lines of code with cloc
#'
#' Runs the external `cloc` utility and parses its CSV output.
#'
#' @param path directory or file to count.
#' @param by_file report one row per file rather than one row per language.
#' @param r_only keep only the rows counting R code.
#' @param cloc_bin name of, or path to, the `cloc` executable.
#' @return A tibble with the `blank`, `comment` and `code` counts, identifying
#'   each row by `filename` when `by_file` is `TRUE` and by `language` and
#'   `files` otherwise (with the queried `path` prepended as a column), or `NULL`
#'   if `cloc` reported nothing.
#' @importFrom readr read_csv
#' @importFrom tibble add_column
#' @importFrom dplyr filter
#' @export
cloc <- function(path, by_file = FALSE, r_only = FALSE, cloc_bin = "cloc") {
  args <- c(
    "--follow-links",
    "-q",
    "--csv",
    if (by_file) "--by-file" else NULL,
    path
  )

  sloc <- system2(cloc_bin, args, stdout = TRUE)

  # cloc 1.x prints a blank line before the CSV header, cloc 2.x does not, so
  # locate the header instead of dropping a fixed number of leading lines. The
  # header is the only row carrying cloc's version banner as a trailing column.
  header <- grep(',"github.com/AlDanial/cloc', sloc, fixed = TRUE)

  if (length(header) > 0) {
    sloc <- sloc[header[1]:length(sloc)]
  }

  if (length(sloc) > 1) {
    sloc[1] <- stringr::str_replace(sloc[1], ',"github.com/AlDanial/cloc.*', "")
    df <- readr::read_csv(I(sloc), col_types = "cciii")

    if (!by_file) {
      df <- tibble::add_column(df, path = path, .before = "files")
    }

    if (r_only) {
      df <- filter(df, language == "R")
    }

    df
  } else {
    NULL
  }
}

# from: https://stackoverflow.com/a/15373917
#' Path of the script currently being executed
#'
#' Works both under `Rscript` (via `--file=`) and when the file is `source()`d.
#'
#' @return The normalised path of the running script, or `NULL` if it cannot be
#'   determined (for instance in an interactive session).
#' @export
current_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  arg_to_match <- "^--file="
  match <- grep(arg_to_match, args)
  if (length(match) > 0) {
    # in Rscript
    normalizePath(sub(arg_to_match, "", args[match]))
  } else {
    # in source
    file <- sys.frames()[[1]]$ofile
    if (!is.null(file)) {
      normalizePath(file)
    } else {
      NULL
    }
  }
}

#' Does a function dispatch on S3?
#'
#' @param fun the function to inspect.
#' @return `TRUE` if `fun` calls `UseMethod()` or `NextMethod()`, i.e. if it is
#'   an S3 generic or an S3 method that delegates further.
#' @importFrom codetools findGlobals
#' @export
is_s3_dispatch_method <- function(fun) {
  globals <- codetools::findGlobals(fun, merge = FALSE)$functions
  any(globals == "UseMethod" | globals == "NextMethod")
}

#' SHA1 digest of a file's contents
#'
#' @param file path to the file to hash.
#' @return The SHA1 digest of the file contents, as a string.
#' @importFrom digest sha1
#' @export
file_sha1 <- function(file) {
  code <- readChar(file, file.info(file)$size)
  digest::sha1(code)
}

#' Read a whole file into a single string
#'
#' @param filename path to the file to read.
#' @return The file contents as a length-one character vector, newlines
#'   included.
#' @export
read_file <- function(filename) {
  readChar(filename, file.info(filename)$size)
}
