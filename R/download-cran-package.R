#' Download and unpack a CRAN package's sources
#'
#' @param package name of the package to download.
#' @param version version to download, or `NULL` for the current one. Archived
#'   versions are resolved through the CRAN archive.
#' @param dest_dir directory to extract into.
#' @param repos the repositories to download from.
#' @return The path to the extracted source directory. Errors if the sources
#'   could not be downloaded or did not extract where expected.
#' @importFrom remotes download_version
#' @export
download_cran_package_source <- function(package, version = NULL, dest_dir = NULL,
                                  repos = getOption("repos")) {
  archive <- remotes::download_version(package, version, repos)
  on.exit(unlink(archive))

  utils::untar(archive, exdir = dest_dir)

  dir <- file.path(dest_dir, package)
  if (!dir.exists(dir)) {
    stop("Expected extracted sources in ", dir, " but it does not exist")
  }

  dir
}
