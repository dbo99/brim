# Validate the BRIM source repository.
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

r_files <- list.files(
  repo_root,
  pattern = "\\.[Rr]$",
  recursive = TRUE,
  full.names = TRUE
)

parse_results <- lapply(r_files, function(path) {
  tryCatch({
    parse(file = path)
    data.frame(file = path, ok = TRUE, error = "")
  }, error = function(e) {
    data.frame(file = path, ok = FALSE, error = conditionMessage(e))
  })
})

parse_results <- do.call(rbind, parse_results)
print(parse_results[!parse_results$ok, , drop = FALSE])
stopifnot("R parse failures were found" = all(parse_results$ok))

all_files <- list.files(
  repo_root,
  recursive = TRUE,
  all.files = TRUE,
  full.names = TRUE,
  include.dirs = FALSE,
  no.. = TRUE
)

info <- file.info(all_files)
large <- all_files[!is.na(info$size) & info$size >= 50 * 1024^2]
stopifnot("Files at or above 50 MiB were found" = !length(large))

message("Source-repository validation passed.")
