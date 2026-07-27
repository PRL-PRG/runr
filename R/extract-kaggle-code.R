#' Extract the R code of a Kaggle notebook
#'
#' Copies `.R` files as they are, knits `.Rmd` with [knitr::purl()], and
#' converts `.irnb`/`.ipynb` notebooks to `.Rmd` first, which needs the
#' rmarkdown package.
#'
#' @param source_file the notebook or script to extract from.
#' @param target_file where to write the extracted R code.
#' @param quiet suppress knitr's progress output.
#' @return `target_file`, the path of the file written. Errors on an unsupported
#'   file type.
#' @importFrom stringr str_ends
#' @importFrom knitr purl
#' @export
extract_kaggle_code <- function(source_file, target_file, quiet=TRUE) {
  if (str_ends(source_file, "\\.[rR]")) {
    file.copy(source_file, target_file, overwrite=TRUE)
  } else if (str_ends(source_file, "\\.Rmd")) {
    knitr::purl(source_file, target_file, quiet=quiet)
  } else if (str_ends(source_file, "\\.irnb") || str_ends(source_file, "\\.ipynb")) {
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      stop("The rmarkdown package is required to extract code from ", source_file)
    }
    tmp <- tempfile(fileext = ".Rmd")
    rmarkdown::convert_ipynb(source_file, tmp)
    extract_kaggle_code(tmp, target_file)
  } else {
    stop("Unsupported file type: ", source_file)
  }

  target_file
}
