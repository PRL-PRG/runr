#' Run an R file with a specific R build
#'
#' Runs `file` in a subprocess started from `r_home`, from the file's own
#' directory. Use this rather than [run_one()] when the experiment is about a
#' particular R build or library; `callr` cannot be told which R to use.
#'
#' @param file path to the R file to run.
#' @param timeout kill the process after this many seconds.
#' @param r_envir named character vector of environment variables to set.
#' @param r_args command line arguments passed to R.
#' @param r_home the `R_HOME` of the R installation to run.
#' @param lib_path library to run against, exported as `R_LIBS`, or `NULL` to
#'   inherit the default.
#' @param keep_output whether to return the captured output `"always"`,
#'   `"never"`, or only `"on_error"`.
#' @param quiet unused, kept for symmetry with the other runners.
#' @return A one-row tibble with `file`, the exit `status`, the `elapsed` time in
#'   seconds and the combined stdout/stderr `output` (`NA` when `keep_output`
#'   suppresses it).
#' @importFrom withr with_dir
#' @importFrom tibble tibble
#' @export
run_r_file <- function(file, timeout,
                       r_envir = c("R_TESTS" = "", "R_BROWSER" = "false", "R_PDFVIEWER" = "false"),
                       r_args = c("--no-save", "--quiet", "--no-readline"),
                       r_home = R.home(), lib_path = NULL,
                       keep_output = c("always", "never", "on_error"), quiet = TRUE) {
  dir <- dirname(file)
  file <- basename(file)

  r_home <- normalizePath(r_home, mustWork = TRUE)
  cmd <- file.path(r_home, "bin", "R")
  args <- c(r_args, "-f", file)

  if (!is.null(lib_path)) {
    lib_path <- normalizePath(lib_path, mustWork = TRUE)
    r_envir <- c(r_envir, "R_LIBS" = lib_path)
  }

  r_envir <- paste(names(r_envir), r_envir, sep = "=")

  message("Running: `", paste(r_envir, collapse=" "), " ", cmd, " ", paste(args, collapse = " "), "' ...")

  # it would be tempting to use callr::rcmd, but there one cannot specify
  # which R should be used.
  time <- system.time(
    withr::with_dir(dir, {
      res <- system2(
        cmd,
        args,
        stdout = TRUE,
        stderr = TRUE,
        env = r_envir,
        timeout = timeout
      )
    })
  )

  elapsed <- time["elapsed"]
  status <- attr(res, "status")
  if (is.null(status)) {
    status <- 0
  }

  output <- paste(res, collapse = "\n")
  output <- switch(match.arg(keep_output),
    always = output,
    never = NA_character_,
    on_error = if (status == 0) NA_character_ else output
  )

  tibble::tibble(file=file.path(dir, file), status, elapsed, output)
}
