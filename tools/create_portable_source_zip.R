# Create a dated portable ZIP of the BRIM source repository.
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
zip_path <- file.path(
  dirname(repo_root),
  paste0(basename(repo_root), "_", format(Sys.Date(), "%Y%m%d"), ".zip")
)

if (file.exists(zip_path)) {
  stop("ZIP already exists: ", zip_path)
}

if (Sys.info()[["sysname"]] == "Darwin" && file.exists("/usr/bin/ditto")) {
  status <- system2(
    "/usr/bin/ditto",
    args = c("-c", "-k", "--keepParent", shQuote(repo_root), shQuote(zip_path))
  )
} else {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dirname(repo_root))
  status <- utils::zip(
    zipfile = zip_path,
    files = basename(repo_root),
    flags = "-r9X"
  )
}

stopifnot("ZIP creation failed" = file.exists(zip_path))
message("Created: ", zip_path)
