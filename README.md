<!-- badges: start -->
[![R-CMD-check](https://github.com/PRL-PRG/runr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PRL-PRG/runr/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/PRL-PRG/runr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PRL-PRG/runr)
[![r-universe](https://prl-prg.r-universe.dev/runr/badges/version)](https://prl-prg.r-universe.dev/runr)
[![mutator](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2FPRL-PRG%2Frunr%2Fgh-pages%2Fmutation-score.json)](https://github.com/PRL-PRG/runr/actions/workflows/mutation-testing.yaml)
<!-- badges: end -->

# runr

Toolkit for running R experiments over a large corpus of R code, typically all
of CRAN.

A *corpus-wide experiment* in runr is a task applied independently to every
package: run its test suite under a modified R, measure coverage, trace values,
collect metadata. runr covers the parts that are the same regardless of the
task:

1. **Acquire** package sources and install them into a dedicated library.
2. **Extract** the runnable R code buried in a package (examples, tests,
   vignettes) into plain, standalone `.R` files.
3. **Run** that code in a fresh, reproducibly configured R subprocess.
4. **Distribute** the per-package jobs across cores or machines with GNU
   parallel, with timeouts, restarts and retries.
5. **Collect** the per-job exit codes, timings, stdout and stderr back into
   tidy data frames.

The R functions are the building blocks; `inst/` holds the shell drivers and
ready-made task scripts that glue them together.

## Installation

From [r-universe](https://prl-prg.r-universe.dev/runr):

```r
install.packages("runr", repos = "https://prl-prg.r-universe.dev")
```

Or from source:

```r
remotes::install_github("PRL-PRG/runr")
```

External tools: [GNU parallel](https://www.gnu.org/software/parallel/) for the
`inst/` drivers, [cloc](https://github.com/AlDanial/cloc) for source-line
counts (`cloc()`, and `extract_package_code(compute_sloc = TRUE)`).

## Acquiring a corpus

```r
install_cran_packages(c("stringr", "dplyr"), lib_dir = "library", dest_dir = "src")
download_cran_package_source("stringr", version = "1.4.0", dest_dir = "src")
```

`install_cran_packages()` installs into `lib_dir` via a separate `callr`
process, skipping what is already installed and loadable, and installs with
`--install-tests --with-keep.source` so that tests and srcrefs survive. It
returns a tibble of `package`, `version`, `dir`. `inst/create-cran-snapshot.R`
downloads a full CRAN snapshot, falling back to `00Archive` for versions no
longer current.

## Extracting runnable code

A package's executable code is not directly runnable: examples live inside `Rd`
files, vignettes inside `Rmd`, tests behind a `testthat.R` driver.
`extract_package_code()` turns all of it into standalone `.R` files under
`output_dir/<type>/`:

```r
files <- extract_package_code(
  "stringr",
  types = c("examples", "tests", "vignettes"),
  output_dir = "extracted",
  compute_sloc = TRUE
)
```

It returns a tibble of `file` and `type`, plus `blank`/`comment`/`code` counts
when `compute_sloc = TRUE`. Notable arguments:

- `split_testthat = TRUE` — emit one driver per `test_*.R` file instead of a
  single `testthat.R`, so that one crashing test file does not take the whole
  suite down and each file gets its own exit code.
- `wrap` — a `function(package, file, type, body)` returning the new file
  content, used to instrument every extracted file. `wrap_using_template()`
  builds one from a template string with `.PACKAGE.`, `.FILE.`, `.TYPE.` and
  `.BODY.` placeholders; `wrap_files()` applies a wrapper in place.
- `filter` — a regexp on file names.

`extract_kaggle_code()` does the same for Kaggle notebooks (`.R`, `.Rmd`,
`.irnb`, `.ipynb`).

## Running code

`run_one()` runs a single file in a fresh R subprocess with a fixed environment
(`LC_ALL=C`, no browser, no PDF viewer) and returns its exit code and
`proc.time()`-derived elapsed time. `run_all()` does the same for every `.R`
file under a directory, skipping the individual `testthat/` files that the
drivers already run:

```r
run_one("extracted/tests/testthat.R", "testthat.out")
run_all("extracted", output_dir = "out")
```

`run_all()` returns a data frame of `file`, `out_file`, `exitval`, `time`,
`error`, and by default copies the code into a scratch directory so the corpus
is never modified. `wrap_code_fun` rewrites each file just before it runs.

`run_r_file()` is the variant to use when the point of the experiment is a
*different* R build: it takes an explicit `r_home` and `lib_path`, which is why
it shells out rather than using `callr::rcmd()` — the latter cannot be told
which R to use. It returns the captured output alongside the status and elapsed
time.

`run_test_env()` and `run_test_dir()` reproduce what `testthat::test_check()`
sets up (namespace environment, `TESTTHAT_PKG`, `TESTTHAT_DIR`) so that
`test_dir()` does not silently skip tests when invoked out of context.

## Distributing jobs

`inst/map.sh` maps an executable over the lines of an input file with GNU
parallel, one working directory per job:

```sh
runr/inst/map.sh -f packages.txt -o run -j 90% -t 30m -e runr/inst/tasks/package-metadata.R
```

A CSV input file is read with `--csv`, one column per argument; any other file
is one argument per line. Jobs run through `inst/run-job.sh`, which records
`task-output.txt` and `task-stats.csv` (`exitval`, `hostname`, `start_time`,
`end_time`, `command`) per job and makes restarts cheap: an already-completed
job is skipped unless `RUNR_RERUN` is `always`, `zero` (rerun jobs that
succeeded) or `non-zero` (rerun jobs that failed). Options can also be set via
`RUNR_TIMEOUT`, `RUNR_JOBS`, `RUNR_OUTPUT_DIR`, `RUNR_INPUT_FILE`,
`RUNR_WORK_DIR` and `RUNR_EXEC_WRAPPER`; unrecognised options are passed
through to GNU parallel.

`inst/on-each-package.sh` is the package-corpus specialisation, passing each
package's source directory as the first argument:

```sh
PACKAGES_SRC_DIR=/path/to/extracted R_LIBS=... R_BIN_DIR=... \
  runr/inst/on-each-package.sh runr/inst/tasks/package-coverage.R
```

It requires `PACKAGES_SRC_DIR`, `R_LIBS` and `R_BIN_DIR`; `PACKAGES` (a file, a
comma-separated list, or unset for everything under `PACKAGES_SRC_DIR`),
`NUM_JOBS`, `RUN_DIR`, `TASK_NAME`, `TIMEOUT` (default `30m`) and
`PARALLEL_ARGS` are optional. `inst/prl-env` is an example environment file
defining these plus a local CRAN mirror and a headless `DISPLAY`.

Ready-made tasks in `inst/tasks/`: `package-metadata.R` (version, size,
loadability, SLOC, reverse dependencies, function and S3 class inventories),
`package-coverage.R`, `package-revdep-coverage.R`, `package-runnable-code.R`,
`run-extracted-code.R`, `run-file.R`, and `echo.R` for debugging the
environment a job actually sees.

### Job failures

Individual jobs are expected to fail. Write task scripts so that a failure
exits non-zero — the exit code is recorded in the GNU parallel log and makes
the job retryable. To retry only the failures, set `PARALLEL_ARGS`:

```sh
PARALLEL_ARGS="--retry-failed" ./runr/inst/on-each-package.sh ./runr/inst/tasks/package-coverage.R
```

Retried jobs are appended to the log, so a job may appear in it more than once;
`read_parallel_results()` keeps the latest entry per job.

### Timeout

The default timeout is 30 minutes, changed with `TIMEOUT`:

```sh
TIMEOUT=1m ./runr/inst/on-each-package.sh ./runr/inst/tasks/package-load.R
```

## Collecting results

```r
log <- read_parallel_log("run/package-coverage")
res <- read_parallel_results("run/package-coverage")
```

`read_parallel_log()` reads the GNU parallel `--joblog` into a tibble with
lower-case names, `starttime` as a datetime and `jobruntime` as a period.
`read_parallel_results()` joins it with the per-job output directories to add
`job`, `path`, `stdout` and `stderr` (plus `stdout_error`/`stderr_error` when a
file could not be read), keeping one row per job. `read_files()` is the
underlying bulk reader: it maps over (job, file) pairs with a progress bar and
turns read errors into values instead of aborting the batch.
`inst/merge-files.R` concatenates the per-job CSVs of a run into one table.

## Analysis helpers

- `metadata_functions(package)` — every function in a namespace with its
  parameter names and whether it is exported, an S3 generic or an S3 method.
- `search_function_calls(expr, functions)` — find calls to given
  `package:::name` functions in an expression, matching both bare and
  namespace-qualified forms.
- `impute_fun_srcref(fun)` — reconstruct `srcref` attributes on a function's
  sub-expressions from its source file's parse data, so that coverage and
  tracing can attribute results to source locations.
- `cloc(path, by_file, r_only)` — `cloc` output as a tibble.
- `is_s3_dispatch_method(fun)`, `file_sha1(file)`, `read_file(file)`,
  `current_script()`.

## Development

```sh
make document   # roxygen
make test       # devtools::test()
make check      # R CMD build + R CMD check
```

## License

MIT
