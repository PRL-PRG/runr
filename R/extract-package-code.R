#' @param wrap either a NULL or filename of a function(package, file, type, body) which
#'   will be called for each extracted file and allows one to alter the file
#'   content. If it is a file it case use the {body}, {package}, {type}, {file} placeholders.
#' @importFrom dplyr select mutate filter vars bind_rows rename anti_join left_join ends_with `%>%`
#' @importFrom purrr keep imap_dfr
#' @importFrom stringr str_replace str_sub
#' @importFrom tibble tibble
#' @export
extract_package_code <- function(pkg, pkg_dir = find.package(pkg),
                                 types = c("examples", "tests", "vignettes", "all"),
                                 output_dir, wrap = NULL, filter = NULL,
                                 split_testthat = FALSE,
                                 compute_sloc = FALSE, quiet = FALSE) {
  stopifnot(is.character(pkg) && length(pkg) == 1)
  stopifnot(dir.exists(pkg_dir))
  stopifnot(is.null(filter) || is.character(filter))

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir <- normalizePath(output_dir, mustWork = TRUE)

  if ("all" %in% types) {
    types <- c("examples", "tests", "vignettes")
  }

  types <- match.arg(types, c("examples", "tests", "vignettes"), several.ok = TRUE)

  # so the output list is named
  names(types) <- types

  extracted_files <- lapply(types, function(type) {
    fun <- switch(type,
      examples = extract_package_examples,
      tests = function(...) extract_package_tests(..., split_testthat = split_testthat),
      vignettes = extract_package_vignettes
    )

    # each type has its own folder not to clash with one another
    output <- file.path(output_dir, type)
    stopifnot(dir.exists(output) || dir.create(output, recursive = TRUE))

    files <- fun(pkg, pkg_dir, output_dir = output)

    if (!is.null(filter)) {
      files <- files[grepl(filter, tools::file_path_sans_ext(files))]
    }

    names(files) <- NULL
    files
  })

  extracted_files <- purrr::discard(
    extracted_files,
    ~ length(.) == 0
  )

  if (length(extracted_files) == 0) {
    return(tibble::tibble(file = character(0), type = character(0)))
  }

  df <- purrr::imap_dfr(extracted_files, ~ tibble::tibble(file = .x, type = .y))

  if (compute_sloc) {
    sloc_all <- cloc(output_dir, by_file = TRUE, r_only = TRUE) %>%
      rename(file = filename)

    sloc <- left_join(df, sloc_all, by = "file") %>%
      mutate(
        blank = ifelse(is.na(blank), 0, blank),
        comment = ifelse(is.na(comment), 0, comment),
        code = ifelse(is.na(code), 0, code)
      )

    sloc_testthat <-
      sloc_all %>%
      filter(str_detect(file, file.path(output_dir, "tests/testthat/test.*\\.[rR]$"))) %>%
      mutate(
        test_name = str_replace(file, file.path(output_dir, "tests/testthat/(test.*)\\.[rR]$"), "\\1")
      )

    df <- if (nrow(sloc_testthat) > 0) {
      sloc_tests <-
        filter(sloc, type == "tests") %>%
        mutate(
          test_driver = sapply(file, is_testthat_driver),
          test_name = str_replace(file, file.path(output_dir, "tests/testthat-drv-(.*)\\.[rR]$"), "\\1")
        )

      sloc_tests_merged <-
        left_join(
          filter(sloc_tests, test_driver),
          sloc_testthat %>% select(test_name, blank, comment, code),
          by = "test_name"
        ) %>%
        mutate(
          blank = ifelse(is.na(blank.y), blank.x, blank.y),
          comment = ifelse(is.na(comment.y), comment.x, comment.y),
          code = ifelse(is.na(code.y), code.x, code.y)
        ) %>%
        select(-test_name, -test_driver, -ends_with(".x"), -ends_with(".y"))

      bind_rows(
        sloc_tests_merged,
        anti_join(sloc, sloc_tests, by = "file")
      )
    } else {
      sloc
    }
  }

  if (!is.null(wrap)) {
    other <- filter(df, !is_testthat_driver(file))
    other_files <- other$file
    other_types <- other$type
    test_files <- c()
    test_types <- c()

    if (nrow(other) != nrow(df)) {
      # we have to explicitly wrap all the testthat tests and helpers
      tt_dir <- file.path(output_dir, "tests", "testthat")
      tt_helpers <- list.files(tt_dir, pattern = "helper.*\\.[rR]$", full.names = T)
      tt_tests <- list.files(tt_dir, pattern = "test.*\\.[rR]$", full.names = T)
      test_files <- c(tt_helpers, tt_tests)
      test_types <- rep("tests", length(test_files))
    }

    files <- c(other_files, test_files)
    types <- c(other_types, test_types)

    wrap_fun <- if (is.function(wrap)) {
      wrap
    } else if (is.character(wrap) && length(wrap) == 1) {
      template <- if (file.access(wrap, 4) == 0) {
        readChar(wrap, file.info(wrap)$size)
      } else {
        stop(wrap, ": no such template file for wrapping")
      }
      wrap_using_template(template)
    } else {
      stop("Unsupported wrap argument: ", wrap)
    }
    wrap_files(pkg, files, types, wrap_fun, quiet)
  }

  df <- mutate(
    df,
    file = stringr::str_sub(file, nchar(output_dir) + 2, nchar(file))
  )
  df
}

#' @importFrom tools Rd_db Rd2ex
extract_package_examples <- function(pkg, pkg_dir, output_dir) {
  db <- tryCatch({
      tools::Rd_db(basename(pkg_dir), lib.loc = dirname(pkg_dir))
  }, error = function(e) {
      c()
  })

  if (!length(db)) {
    return(character())
  }

  files <- names(db)

  examples <- sapply(files, function(x) {
    f <- file.path(output_dir, paste0(basename(x), ".R"))
    tools::Rd2ex(db[[x]], f,
      defines = NULL,
      commentDontrun = TRUE, commentDonttest = TRUE
    )

    if (!file.exists(f)) {
      message("Rd file `", x, "' does not contain any code to be run")
      NA
    } else {
      # prepend the file with library call
      txt <- c(
        paste0("library(", pkg, ")"),
        "",
        "",
        readLines(f)
      )
      writeLines(txt, f)
      f
    }
  })

  na.omit(examples)
}

#' @importFrom purrr keep
extract_package_tests <- function(pkg, pkg_dir, output_dir, split_testthat = FALSE) {
  test_dir <- file.path(pkg_dir, "tests")

  if (!dir.exists(test_dir)) {
    return(character())
  }

  files <- Sys.glob(file.path(test_dir, "*"))
  file.copy(files, output_dir, recursive = TRUE)

  tests <- file.path(output_dir, basename(files))
  tests <- tests[!dir.exists(tests)]
  tests <- tests[grepl("\\.[rR]$", tests)]

  if (split_testthat) {
    testthat_drivers <- purrr::keep(tests, is_testthat_driver)
    if (length(testthat_drivers) > 0) {
      file.remove(testthat_drivers)
      expand_testthat_tests(pkg, output_dir)
      tests <- list.files(output_dir, pattern = "\\.[rR]$", full.names = TRUE, recursive = FALSE)
    }
  }

  tests
}

#' @importFrom stringr str_glue str_replace
#' @importFrom testthat find_test_scripts
expand_testthat_tests <- function(pkg_name, test_dir) {
  # this is a constant - also used in test_check
  testthat_dir <- file.path(test_dir, "testthat")
  test_files <- testthat::find_test_scripts(testthat_dir)
  for (file in test_files) {
    test_name <- tools::file_path_sans_ext(basename(file))
    # testthat filter stripts the 'test-' prefix and .R suffix
    test_name_filter <- str_replace(test_name, "^test[-_]", "")
    driver_file <- file.path(test_dir, paste0("testthat-drv-", test_name, ".R"))
    code <- str_glue(
      "library({pkg_name})",
      "library(testthat)",
      "test_check('{pkg_name}', filter='^{test_name_filter}$')",
      .sep = "\n"
    )
    writeLines(code, driver_file)
  }
}

#' @importFrom tools pkgVignettes checkVignettes
extract_package_vignettes <- function(pkg, pkg_dir, output_dir) {
  lib_path <- dirname(pkg_dir)
  vinfo <- tools::pkgVignettes(pkg, lib.loc = lib_path, source = T)
  if (length(vinfo$docs) == 0) {
    return(character())
  }

  if (length(vinfo$sources) == 0) {
    # It is possible that there are no sources. The following should generate
    # them if there are any sources in the R code. It might actually run the
    # vignettes as well. That is a pity, but there is no way to tell it not to
    # (the tangle is needed to it extracts the R code)
    #
    # This can fail (for example, one vignette might override output of another
    # one, cf. proto package). Yet some files will be extracted.
    tryCatch({
      tools::checkVignettes(pkg, pkg_dir, lib.loc = lib_path, tangle =
                            TRUE, weave = FALSE, workdir = "src")
    }, error=function(e) {
      warning(e$message)
    })
  }

  # check if there are some sources
  vinfo <- tools::pkgVignettes(pkg, lib.loc = lib_path, source = T)
  files <- as.character(unlist(vinfo$sources))
  if (length(files) == 0) {
    return(character())
  }

  # the pkgVignettes can return duplicates
  files <- unique(files)
  dirs <- unique(dirname(files))
  for (d in dirs) {
    fs <- Sys.glob(file.path(d, "*"))
    file.copy(fs, output_dir, recursive = TRUE)
  }

  file.copy(files, to = output_dir)
  vignettes <- file.path(output_dir, basename(files))

  vignettes
}

is_testthat_driver <- Vectorize(function(file) {
  file_lower <- tolower(file)
  dir.exists(file.path(dirname(file), "testthat")) &&
    (str_detect(file_lower, "testthat-drv-.*\\.r$") ||
      endsWith(file_lower, "testthat.r") ||
      endsWith(file_lower, "test-all.r") ||
      endsWith(file_lower, "run-all.r"))
})

#' @importFrom stringr str_replace
#' @importFrom magrittr %>%
#' @export
wrap_using_template <- function(template) {
  function(package, file, type, body) {
    template %>%
      str_replace(fixed(".PACKAGE."), package) %>%
      str_replace(fixed(".FILE."), file) %>%
      str_replace(fixed(".TYPE."), type) %>%
      str_replace(fixed(".BODY."), body)
  }
}

#' @export
wrap_files <- Vectorize(function(package, file, type, wrap_fun, quiet = TRUE) {
  if (!quiet) {
    message("- updating ", file, " (", type, ")")
  }

  # TODO share with run_all
  tryCatch(
    {
      body <- read_file(file)
      new_body <- wrap_fun(package, file, type, body)
      writeLines(new_body, file)
    },
    error = function(e) {
      message("E unable to wrap file", file, ": ", e$message)
    }
  )
}, vectorize.args = c("file", "type"))
