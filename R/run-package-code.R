#' Create a package environment with access to all package functions
#'
#' Based on `testthat:::test_pkg_env`.
#'
#' @param package name of the package whose namespace to expose.
#' @return An environment containing all bindings of the package namespace.
#' @export
run_test_env <- function(package) {
  list2env(
    as.list(getNamespace(package), all.names=TRUE),
    parent=parent.env(getNamespace(package))
  )
}

#' Run a testthat directory the way test_check would
#'
#' Simulates `testthat::test_check()`, otherwise [testthat::test_dir()] might
#' skip some tests. Based on `testthat:::test_package_dir`.
#'
#' @param package name of the package under test.
#' @param path directory containing the testthat tests.
#' @param ... passed on to [testthat::test_dir()].
#' @return The value of [testthat::test_dir()].
#' @importFrom testthat test_dir
#' @importFrom withr local_options local_envvar
#' @export
run_test_dir <- function(package, path, ...) {
  env <- run_test_env(package)
  withr::local_options(
    list(
      topLevelEnvironment=env,
      # need to set this to prevent quick death when the error is set to quit
      # the session
      error=NULL
    )
  )
  withr::local_envvar(list(TESTTHAT_PKG=package, TESTTHAT_DIR=path))
  # TODO use external R process
  testthat::test_dir(path=path, env=env, ...)
}

rcmd_batch_runner <- function(file, out_file, quiet=F) {
  callr::rcmd(
    "BATCH",
    list(file, out_file),
    spinner=T,
    env=c(
      "LANGUAGE"="en",
      "LC_COLLATE"="C",
      "LC_TIME"="C",
      "SRCDIR"="."
    )
  )
}

#' Run a single R file in a fresh R subprocess
#'
#' Runs `file` with the current R build under a fixed environment (`LC_ALL=C`, no
#' browser, no PDF viewer, sources kept) so that runs are comparable across a
#' corpus.
#'
#' @param file path to the R file to run.
#' @param out_file where to write the combined stdout and stderr, or `NULL` to
#'   let it go to the console.
#' @param cwd run from the file's own directory rather than the current one.
#' @param quiet do not print the command being run.
#' @param stats also report the elapsed time, recovered from the trailing
#'   `proc.time()` in the output.
#' @return A one-row data frame with the `exitval`, and the elapsed `time` when
#'   `stats` is `TRUE` (`NA` if the run failed or the timing could not be read).
#' @export
run_one <- function(file, out_file, cwd=TRUE, quiet=TRUE, stats=TRUE) {
  stopifnot(file.exists(file))

  if (is.null(out_file)) {
    out_file <- ""
  } else {
    stopifnot(dir.exists(dirname(out_file)))
  }

  error <- as.character(NA)
  time <- as.double(NA)

  cmd <- file.path(R.home("bin"), "R")
  args <- c(
    "--no-save",
    "--quiet",
    "--no-readline",
    "--silent"
  )
  env <- c(
    "LANGUAGE=en",
    "LC_COLLATE=C",
    "LC_TIME=C",
    "LC_ALL=C",
    "SRCDIR=.",
    'R_TESTS=""',
    "R_BROWSER=false",
    "R_PDFVIEWER=false",
    "R_BATCH=1",
    "R_KEEP_PKG_SOURCE=yes",
    "R_KEEP_PKG_PARSE_DATA=yes"
  )

  if (!quiet) {
    cat("Running:",
        paste(env, collapse=" "),
        cmd,
        paste(args, collapse=" "),
        "<", file, "2>&1", out_file,
        "\n"
    )
  }

  if (cwd) {
    wd <- dirname(file)
    file <- basename(file)
  } else {
    wd <- getwd()
  }

  withr::with_dir(wd, {
    exitval <- system2(
      cmd,
      args,
      stdin=file,
      stdout=out_file,
      stderr=out_file,
      env=env
    )
  })

  if (stats) {
    time <- NA
    if (exitval == 0L) {
      tryCatch({
        tmp <- readLines(out_file)
        if (tmp[length(tmp)-2] == "> proc.time()") {
          x <- tmp[length(tmp)]
          x <- strsplit(x, " ")[[1]]
          x <- trimws(x, "both")
          x <- x[x != ""]
          x <- as.double(x)
          time <- x[3]
        }
      }, error=function(e) {
        warning("Unable to get timing from: ", out_file)
      })
    }

    data.frame(exitval, time)
  } else {
    data.frame(exitval)
  }
}

#' Run every R file under a directory
#'
#' Runs each `.R` file with [run_one()]. The individual files under a `testthat/`
#' directory are skipped, because the extracted testthat drivers run them. By
#' default the code is copied into a scratch directory first, so the corpus is
#' left untouched even when `wrap_code_fun` rewrites files.
#'
#' @param path directory holding the R files to run.
#' @param output_dir directory to write the per-file `.out` files to.
#' @param run_dir scratch directory to copy the code into before running. Pass
#'   `path` to run in place.
#' @param filter regexp on file names, or `NULL` to run everything.
#' @param wrap_code_fun `function(code)` returning the new file contents, applied
#'   to each file just before it runs.
#' @param clean remove `run_dir` afterwards.
#' @param quiet do not report progress.
#' @param skip_if_out_exists treat a file whose `.out` already exists as done.
#' @return A data frame with one row per file and the columns `file`, `out_file`,
#'   `exitval`, `time` and `error` (the message if the file could not be run at
#'   all, `NA` otherwise).
#' @importFrom stringr str_detect
#' @export
run_all <- function(path, output_dir=getwd(), run_dir=tempfile(), filter=NULL,
                    wrap_code_fun=NULL, clean=TRUE, quiet=TRUE, skip_if_out_exists=TRUE) {
  stopifnot(dir.exists(path))
  stopifnot(dir.exists(output_dir))

  path <- normalizePath(path, mustWork=TRUE)
  output_dir <- normalizePath(output_dir, mustWork=TRUE)
  run_dir <- normalizePath(run_dir)

  result <- data.frame(
    file=character(0),
    out_file=character(0),
    exitval=integer(0),
    time=double(0),
    error=character(0)
  )

  files <- Sys.glob(file.path(path, "*"))
  if (length(files) == 0) {
    return(result)
  }

  if (path != run_dir) {
    if (dir.exists(run_dir)) unlink(run_dir, recursive=TRUE)
    dir.create(run_dir)
    if (clean) {
      on.exit({
        if (!quiet) cat("Removing running dir", run_dir, "\n")
        unlink(run_dir, recursive=TRUE)
      })
    }

    if (!quiet) cat("Copying files from:", path, "to:", run_dir, "...\n")
    ret <- file.copy(files, run_dir, recursive=TRUE)
    if (!all(ret)) stop("Unable to copy:", files[!ret])
  }

  files <- list.files(run_dir, pattern=".*\\.[rR]$", full.names=TRUE, recursive=TRUE)

  # we need to exclude the individual testthat tests as they will be run by the
  # testthat driver
  files <- files[!str_detect(files, "/testthat/")]

  # apply filter
  if (!is.null(filter)) {
    files <- files[str_detect(basename(files), filter)]
  }

  if (!quiet) cat("Running", length(files), "R files ...\n")

  rows <- lapply(files, function(file) {
    out_file <- file.path(
      output_dir,
      paste0(tools::file_path_sans_ext(basename(file)), ".out")
    )

    if (!quiet) cat("-", file, "(output", out_file, ") ... ")

    tryCatch({
      if (!is.null(wrap_code_fun)) {
        code <- readChar(file, file.info(file)$size)
        code <- wrap_code_fun(code)
        writeChar(code, file)
      }

      if (skip_if_out_exists && file.exists(out_file)) {
        if (!quiet) cat("already done\n")
        res <- data.frame(exitval=0, time=0)
      } else {
        res <- run_one(file, out_file, cwd=TRUE, quiet=TRUE)
        if (!quiet) {
          if (res$exitval == 0) {
            cat("done (in", res$time, ")\n")
          } else {
            cat("failed (exitval", res$exitval, ")\n")
          }
        }
      }

      cbind(file, out_file, res, error=NA)
    }, error=function(e) {
      if (!quiet) cat("failed (", e$message, ")\n")
      
      data.frame(file, out_file=NA, exitval=NA, time=NA, error=e$message)
    })
  })

  result <- if (length(rows) > 0) {
    do.call(rbind, rows)
  }

  result
}
