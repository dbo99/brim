#!/usr/bin/env Rscript
# Portable synthetic inputs only; no retained data, network or application build.

run_tests <- function() {
  stopifnot(requireNamespace("dplyr", quietly = TRUE))
  script <- grep("^--file=", commandArgs(), value = TRUE)
  stopifnot(length(script) == 1L)
  root <- normalizePath(file.path(dirname(sub("^--file=", "", script)), ".."))
  builder <- file.path(root, "05_map_build", "04_build_portatreasure2_core_map.r")
  wanted <- c(
    "pt_clean_site_no_local", "pt_boolish_nullable_local",
    "pt_first_existing_path_local", "pt_enrich_usgs_streamgages_with_blm_distance"
  )
  owner <- new.env(parent = baseenv())
  found <- character()
  for (expr in parse(file = builder)) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% wanted) {
      stopifnot(is.call(expr[[3]]), identical(expr[[3]][[1]], as.name("function")))
      eval(expr, owner)
      found <- c(found, as.character(expr[[2]]))
    }
  }
  stopifnot(setequal(found, wanted), length(found) == length(wanted))

  work <- tempfile("streamgage-join-")
  dir.create(work)
  old_wd <- setwd(work)
  old_override <- Sys.getenv("USGS_STREAMFLOW_LIVE_INDEX_CSV", unset = NA_character_)
  on.exit({
    setwd(old_wd)
    if (is.na(old_override)) Sys.unsetenv("USGS_STREAMFLOW_LIVE_INDEX_CSV") else
      Sys.setenv(USGS_STREAMFLOW_LIVE_INDEX_CSV = old_override)
    unlink(work, recursive = TRUE)
  }, add = TRUE)
  owner$DIR <- list(root = work)
  index_path <- file.path(work, "index.csv")
  Sys.setenv(USGS_STREAMFLOW_LIVE_INDEX_CSV = index_path)
  join <- owner$pt_enrich_usgs_streamgages_with_blm_distance

  # Inspect the actual reader and derive its pre-fix counterpart in memory.
  # Replacing one call leaves NULL assignments and missing subscript arguments intact.
  reads <- list()
  walk <- function(x, replacement = NULL) {
    if (!is.call(x)) return(x)
    if (identical(x[[1]], quote(utils::read.csv)) &&
        identical(x[[2]], as.name("index_path"))) {
      reads[[length(reads) + 1L]] <<- x
      return(if (is.null(replacement)) x else replacement)
    }
    for (i in seq_along(x)) x[i] <- list(walk(x[[i]], replacement))
    x
  }
  invisible(walk(body(join)))
  stopifnot(length(reads) == 1L)
  reader <- reads[[1]]
  original_reader <- reader
  original_reader$colClasses <- NULL
  baseline <- join
  reads <- list()
  body(baseline) <- walk(body(join), original_reader)
  stopifnot(length(reads) == 1L)
  read_actual <- function(call) eval(call, list(index_path = index_path), baseenv())
  write_index <- function(x) utils::write.csv(x, index_path, row.names = FALSE, na = "")
  checks <- 0L
  test <- function(label, code) {
    force(code)
    checks <<- checks + 1L
    cat("PASS ", label, "\n", sep = "")
  }
  leading_assertion <- function(x) {
    if (!identical(x$dist_to_blm_mi[x$site_no == "01234567" & !is.na(x$site_no)], 1.25)) {
      stop("leading-zero site did not retain its exact distance association", call. = FALSE)
    }
  }

  index <- data.frame(
    site_no = c("01234567", "1234567", "23456789", "123456789012345"),
    on_blm_ca = c(FALSE, TRUE, FALSE, FALSE),
    dist_to_blm_mi = c(1.25, 0, 3.125, 0.5),
    dist_to_blm_ft = c(6600, 0, 16500, 2640),
    count = 2:5, available = c(TRUE, FALSE, TRUE, FALSE),
    description = c("leading", "unprefixed", "existing", "long identifier")
  )
  local <- data.frame(
    site_no = c(index$site_no, "99999999", "", NA_character_, "NA"),
    marker = seq_len(8L), current_ops_data_value_available = rep(c(TRUE, FALSE), 4L)
  )
  write_index(index)
  old <- baseline(local)
  result <- join(local)
  test("original reader fails the same leading-zero regression assertion", {
    failure <- tryCatch({leading_assertion(old); NULL}, error = identity)
    stopifnot(inherits(failure, "error"), identical(
      conditionMessage(failure), "leading-zero site did not retain its exact distance association"
    ))
    cat("BASELINE_EXPECTED_FAILURE: ", conditionMessage(failure), "\n", sep = "")
  })
  test("actual repaired reader passes the leading-zero regression assertion", leading_assertion(result))
  test("zero-prefixed and unprefixed identifiers remain distinct", {
    stopifnot(identical(result$site_no[1:2], index$site_no[1:2]),
              identical(result$on_blm_ca[1:2], c(FALSE, TRUE)),
              identical(result$dist_to_blm_mi[1:2], c(1.25, 0)))
  })
  fields <- c("on_blm_ca", "dist_to_blm_mi", "dist_to_blm_ft")
  test("existing non-leading-zero associations and all distance fields persist", {
    stopifnot(identical(result[3:4, fields], old[3:4, fields]))
    for (nm in fields) stopifnot(identical(result[[nm]][3:4], index[[nm]][3:4]))
  })
  test("legitimate unmatched and blank keys retain unknown distances", {
    stopifnot(identical(result$site_no[5], "99999999"), all(is.na(result$site_no[6:8])))
    for (nm in fields) stopifnot(all(is.na(result[[nm]][5:8])))
  })
  test("row order and unrelated Local attributes persist", {
    stopifnot(nrow(result) == nrow(local), identical(result$marker, local$marker),
              identical(result$current_ops_data_value_available, local$current_ops_data_value_available))
  })
  test("only the identifier parsed type changes", {
    parsed <- read_actual(reader)
    original <- read_actual(original_reader)
    stopifnot(is.character(parsed$site_no), is.numeric(original$site_no),
              identical(parsed$site_no, index$site_no))
    other <- setdiff(names(parsed), "site_no")
    stopifnot(identical(parsed[other], original[other]))
  })
  test("cache precedence remains independent for each distance field", {
    cached <- local[1:4, ]
    cached$on_blm_ca <- c(NA, NA, TRUE, NA)
    cached$dist_to_blm_mi <- c(NA, NA, 99, NA)
    cached$dist_to_blm_ft <- c(123, NA, NA, NA)
    out <- join(cached)
    stopifnot(identical(out$on_blm_ca, c(FALSE, TRUE, TRUE, FALSE)),
              identical(out$dist_to_blm_mi, c(1.25, 0, 99, 0.5)),
              identical(out$dist_to_blm_ft, c(123, 0, 16500, 2640)))
  })

  duplicate <- data.frame(
    site_no = c(" USGS-00055555.0 ", "NWIS_00055555", "44444444", "44444444", "", NA),
    on_blm_ca = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE),
    dist_to_blm_mi = c(0, 8, NA, 9, 0, 0),
    dist_to_blm_ft = c(0, 42240, NA, 47520, 0, 0)
  )
  write_index(duplicate)
  duplicate_local <- data.frame(site_no = c("00055555", "NWIS-00055555.0", "44444444"), marker = 1:3)
  out <- join(duplicate_local)
  test("normalization and first-duplicate handling do not multiply Local rows", {
    stopifnot(nrow(out) == 3L, identical(out$marker, 1:3),
              identical(out$site_no, c("00055555", "00055555", "44444444")),
              identical(out$on_blm_ca, c(TRUE, TRUE, FALSE)),
              identical(out$dist_to_blm_mi[1:2], c(0, 0)))
  })
  test("first duplicate is retained even when its distances are missing", {
    stopifnot(is.na(out$dist_to_blm_mi[3]), is.na(out$dist_to_blm_ft[3]))
  })
  test("existing prefix whitespace decimal-suffix and missing-token normalization persists", {
    stopifnot(identical(owner$pt_clean_site_no_local(c(
      " USGS-00055555.0 ", "NWIS_00055555", " 000 55555 ",
      "", "NA", "NaN", "NULL", "null", "undefined", NA_character_
    )), c(rep("00055555", 3L), rep(NA_character_, 7L))))
  })
  cat("RESULT passed=", checks, " failed=0; baseline_expected_failures=1\n", sep = "")
}

run_tests()
