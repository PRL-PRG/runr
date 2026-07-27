test_that("testthat is properly handled", {
  # compute_sloc=TRUE shells out to cloc
  skip_if_not(nzchar(Sys.which("cloc")), "the cloc binary is not available")

  wrapper <- function(package, file, type, body) {
    str_glue(
      "# {package} :: {type} :: {basename(file)}",
      "{body}",
      .sep="\n"
    )
  }

  test_pkg_dir <- "data/pkg.testthat1"
  out_dir <- tempfile()

  files <- extract_package_code(
    "pkg.testthat1", test_pkg_dir,
    types="tests", out_dir, wrap=wrapper,
    split_testthat=TRUE, compute_sloc=TRUE, quiet=FALSE
  )

  # testthat::find_test_scripts() lists files in locale collation order, which
  # differs between the ambient locale and the LC_COLLATE=C that R CMD check
  # forces, so key every expectation by file name rather than by position.
  drv <- c("testthat-drv-test00_a.R", "testthat-drv-test-test1.R", "testthat-drv-test_test2.R")
  expect_setequal(files$file, file.path("tests", drv))

  by_drv <- function(x) setNames(x, basename(files$file))[drv]

  expect_equal(
    by_drv(purrr::map_chr(files$file, ~readLines(file.path(out_dir, .))[3])),
    setNames(
      c(
        "test_check('pkg.testthat1', filter='^test00_a$')",
        "test_check('pkg.testthat1', filter='^test1$')",
        "test_check('pkg.testthat1', filter='^test2$')"
      ),
      drv
    )
  )

  # the line count is before the wrapping
  expect_equal(by_drv(files$code), setNames(c(1, 6, 6), drv))

  # the drive files are not wrapped
  expect_equal(
    by_drv(purrr::map_int(files$file, ~length(readLines(file.path(out_dir, .))))),
    setNames(c(3L, 3L, 3L), drv)
  )

  # the testthat files are wrapped
  tt_tests <- file.path(dirname(files$file), "testthat", c("test00_a.R", "test-test1.R", "test_test2.R"))
  expect_equal(
    purrr::map_chr(tt_tests, ~readLines(file.path(out_dir, .))[1]),
    c(
      "# pkg.testthat1 :: tests :: test00_a.R",
      "# pkg.testthat1 :: tests :: test-test1.R",
      "# pkg.testthat1 :: tests :: test_test2.R"
    )
  )
})
