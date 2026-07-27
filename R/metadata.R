#' Inventory of the functions in a package namespace
#'
#' Loads `package` and describes every function bound in its namespace,
#' including the ones it does not export.
#'
#' @param package name of the package to inspect.
#' @param lib_loc library path to load the package from, or `NULL` for the
#'   current one.
#' @return A data frame with one row per function and the columns `pkg_name`,
#'   `fun_name`, `exported`, `is_s3_dispatch` (calls `UseMethod()` or
#'   `NextMethod()`), `is_s3_method` (registered as an S3 method) and `params`
#'   (the parameter names, `;`-separated).
#' @importFrom withr local_libpaths
#' @export
metadata_functions <- function(package, lib_loc=NULL) {
  withr::local_libpaths(lib_loc)

  ns <- getNamespace(package)
  exports <- getNamespaceExports(package)
  bindings <- ls(envir = ns, all.names = TRUE)

  function_bindings <- sapply(bindings, USE.NAMES = FALSE, function(x) {
    f <- get0(x, envir = ns)
    if (!is.function(f)) NA else x
  })
  function_bindings <- na.omit(function_bindings)
  functions <- lapply(function_bindings, get0, envir = ns)

  params <- lapply(functions, function(x) names(formals(x)))

  s3_methods <- NULL
  if (exists(".__NAMESPACE__.", envir = ns)) {
    s3_methods <- ns$.__NAMESPACE__.$S3methods[,3]
  }

  if (is.null(s3_methods)) {
    s3_methods <- character(0)
  }

  is_s3_dispatch <- sapply(functions, USE.NAMES = FALSE, is_s3_dispatch_method)
  is_s3_method <- function_bindings %in% s3_methods

  params <- sapply(params, USE.NAMES = FALSE, paste0, collapse = ";")
  exported <- function_bindings %in% exports

  df <- data.frame(
    pkg_name = package,
    fun_name = function_bindings,
    exported,
    is_s3_dispatch,
    is_s3_method,
    params
  )

  df
}
