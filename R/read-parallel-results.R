#' Read the results of a GNU parallel run
#'
#' Joins the job log with the per-job output directories under `path`. The join
#' is driven by the directories, so a job retried with `--retry-failed` — which
#' appends a second log entry but reuses its directory — contributes a single
#' row, for the attempt whose output was kept.
#'
#' @param path the run directory, holding `parallel.log` and one subdirectory per
#'   job.
#' @param stdout add the captured stdout.
#' @param stderr add the captured stderr.
#' @return A tibble of [read_parallel_log()] joined with `job` and `path`, plus a
#'   column per requested stream and a matching `_error` column holding the
#'   message when that file could not be read.
#' @importFrom dplyr left_join bind_cols
#' @importFrom purrr map_dfr
#' @importFrom stringr str_c
#' @importFrom tibble tibble as_tibble
#' @export
#'
read_parallel_results <- function(path, stdout=TRUE, stderr=TRUE) {
  log <- read_parallel_log(path)
  seq <- read_parallel_seq(path)
  # It is important that the seq with log and not the other way around.
  # The reason is that log could have some duplication due to
  # multiple run of the same job. Each job gets a unique seq ID stored
  # in the log, but since we use the job name as a directory name, running
  # the same job will have two different seqs, but only one will be kept
  # in the output directory
  df <- left_join(seq, log, by="seq")

  read_extras <- function(name) {
    process_row <- function(x) {
      if (is.character(x) && length(x) == 0) {
        x <- as.character(NA)
      }

      row <- if (inherits(x, "error")) {
        list(as.character(NA), x[[3]])
      } else if (inherits(x, "condition")) {
        list(as.character(NA), x$message)
      } else {
        list(str_c(x, collapse="\n"), as.character(NA))
      }

      names(row) <- c(name, str_c(name, "_error"))
      as_tibble(row)
    }

    files <- file.path(df$path, name)
    content <- read_files(df$job, files)
    map_dfr(content, process_row)
  }

  extras <- c("stdout", "stderr")
  extras <- extras[c(stdout, stderr)]
  for (e in extras) df <- bind_cols(df, read_extras(e))
  df
}

#' Read many files, turning failures into values
#'
#' Bulk reader behind the other `read_parallel_*` functions. Shows a progress
#' bar, and records a read failure as a value instead of aborting the batch, so
#' that one unreadable file does not lose the whole run.
#'
#' @param jobs job names, parallel to `files`.
#' @param files paths to read, parallel to `jobs`.
#' @param readf function used to read one file.
#' @param mapf `function(job, contents)` applied to each successful read.
#' @param mapf_error function called with the job, the file and the error
#'   message when a read fails. Defaults to returning a condition object.
#' @param reducef function applied to the whole list of results.
#' @param quiet do not message about individual read failures.
#' @return The result of `reducef` applied to the per-file results, a list named
#'   by `files` before reduction.
#' @importFrom purrr map2 discard keep
#' @importFrom readr read_lines
#' @importFrom stringr str_glue
#' @importFrom progress progress_bar
#' @export
#'
read_files <- function(jobs, files,
                       readf=read_lines,
                       mapf=function(job, x) x,
                       mapf_error=function(...) structure(list(...), class="error"),
                       reducef=identity,
                       quiet=TRUE) {

  stopifnot(length(jobs) == length(files))

  pb <- progress::progress_bar$new(
    format="reading :file [:bar] :current/:total :percent, :eta",
    total=length(jobs),
    clear=FALSE,
    width=80
  )

  read_one <- function(job, file) {
    tryCatch({
      mapf(job, readf(file))
    }, error=function(e) {
      msg <- str_glue("[{job}] unable to read: {file}: {e$message}")

      if (!quiet) message(msg)

      mapf_error(job, file, e$message)
    }, finally={
      if (is.na(file)) file <- "NA"
      else if (is.null(file)) file <- "NULL"
      else file <- basename(file)
      pb$tick(tokens=list(file=file))
    })
  }

  results <- map2(jobs, files, read_one)
  names(results) <- files
  reducef(results)
}

#' Read the job sequence numbers of a GNU parallel run
#'
#' Each job directory holds a `seq` file with the sequence number GNU parallel
#' assigned it, which is what links a directory back to a log entry.
#'
#' @param path the run directory to scan recursively for `seq` files.
#' @param quiet do not message about unreadable `seq` files.
#' @return A tibble with one row per job directory and the columns `job`, `path`
#'   and `seq`.
#' @importFrom dplyr bind_rows
#' @importFrom purrr map2_dfr keep
#' @export
#'
read_parallel_seq <- function(path, quiet=TRUE) {
#  from some reason dir_ls keeps crashing on us with segfaults
#  files <- dir_ls(path, regex="/seq$", recurse=1)
  files <- list.files(path, pattern="^seq$", full.names=TRUE, recursive=TRUE)
  jobs <- basename(dirname(files))

  read_files(
    jobs,
    files,
    mapf=function(job, x) tibble(job, path=file.path(path, job), seq=as.integer(x)),
    mapf_error=function(...) NULL,
    reducef=bind_rows,
    quiet=quiet
  )
}
