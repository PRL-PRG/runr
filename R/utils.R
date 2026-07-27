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

#' @importFrom codetools findGlobals
#' @export
is_s3_dispatch_method <- function(fun) {
  globals <- codetools::findGlobals(fun, merge = FALSE)$functions
  any(globals == "UseMethod" | globals == "NextMethod")
}

#' @importFrom digest sha1
#' @export
file_sha1 <- function(file) {
  code <- readChar(file, file.info(file)$size)
  digest::sha1(code)
}

#' @export
read_file <- function(filename) {
  readChar(filename, file.info(filename)$size)
}
