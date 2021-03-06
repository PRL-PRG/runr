#' @importFrom dplyr mutate rename_all left_join select everything
#' @importFrom fs file_exists is_dir path
#' @importFrom lubridate as_datetime as.period
#' @importFrom readr read_tsv cols col_double col_character col_integer
#' @export
#'
read_parallel_log <- function(path) {
  log_file <- if (is_dir(path)) {
    file.path(path, "parallel.log")
  } else {
    path
  }

  stopifnot(file_exists(log_file))

  df <- read_tsv(
    log_file,
    col_types=cols(
      Seq=col_integer(),
      Host=col_character(),
      Starttime=col_double(),
      JobRuntime=col_double(),
      Send=col_integer(),
      Receive=col_integer(),
      Exitval=col_integer(),
      Signal=col_integer(),
      Command=col_character()
    )
  ) %>%
    rename_all(tolower) %>%
    mutate(
      starttime=as_datetime(starttime),
      jobruntime=as.period(jobruntime, unit="seconds")
    )
}
