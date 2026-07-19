# ==== 44_assign_external_catalog_ids.R ======================================
#
# PURPOSE:
#   Assign stable BRIM External Layer IDs to rows in:
#     00_config/external_service_catalog.csv
#
# WHY:
#   The External catalog is getting large.  A short visible ID such as #066 makes
#   it much easier to discuss, QA, move, or delete a layer without copying a
#   long display name.  For example: "remove #066" or "try #114".
#
# HOW IT WORKS:
#   * Adds an `external_layer_id` column if it is missing.
#   * Preserves existing IDs exactly.
#   * Fills only blank IDs with the next unused EXT### value.
#   * Writes a timestamped backup before modifying the CSV.
#
# IMPORTANT:
#   This script modifies the build input CSV.  It is intentionally conservative:
#   it does not reorder rows and it does not renumber existing IDs.  If you want
#   to intentionally renumber all layers later, make a separate explicit script.
#
# RUN FROM ANYWHERE:
#   local({
#     old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
#     setwd("C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2")
#     source("02_preprocess/44_assign_external_catalog_ids.R")
#   })

find_project_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)

  for (i in seq_len(8)) {
    if (file.exists(file.path(p, "00_config", "external_service_catalog.csv"))) {
      return(p)
    }
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }

  stop("Could not find BRIM project root from: ", start)
}

project_root <- find_project_root()
message("BRIM project root: ", project_root)

catalog_path <- file.path(project_root, "00_config", "external_service_catalog.csv")
if (!file.exists(catalog_path)) stop("Catalog not found: ", catalog_path)

catalog <- utils::read.csv(catalog_path, stringsAsFactors = FALSE, check.names = FALSE)

if (!"external_layer_id" %in% names(catalog)) {
  insert_after <- match("external_subgroup_order", names(catalog))
  if (is.na(insert_after)) insert_after <- match("display_name", names(catalog)) - 1
  if (is.na(insert_after) || insert_after < 1) insert_after <- 1

  catalog <- cbind(
    catalog[, seq_len(insert_after), drop = FALSE],
    external_layer_id = rep("", nrow(catalog)),
    catalog[, setdiff(seq_along(catalog), seq_len(insert_after)), drop = FALSE]
  )
}

catalog$external_layer_id <- trimws(as.character(catalog$external_layer_id))
catalog$external_layer_id[is.na(catalog$external_layer_id)] <- ""

used <- catalog$external_layer_id[nzchar(catalog$external_layer_id)]
used <- unique(used)

next_available_id <- function(used_ids) {
  n <- 1L
  repeat {
    candidate <- sprintf("EXT%03d", n)
    if (!candidate %in% used_ids) return(candidate)
    n <- n + 1L
  }
}

filled <- 0L
for (i in seq_len(nrow(catalog))) {
  if (!nzchar(catalog$external_layer_id[i])) {
    new_id <- next_available_id(used)
    catalog$external_layer_id[i] <- new_id
    used <- c(used, new_id)
    filled <- filled + 1L
  }
}

if (anyDuplicated(catalog$external_layer_id)) {
  dupes <- unique(catalog$external_layer_id[duplicated(catalog$external_layer_id)])
  stop("Duplicate external_layer_id values found after assignment: ", paste(dupes, collapse = ", "))
}

backup_path <- file.path(
  dirname(catalog_path),
  paste0("external_service_catalog_backup_before_ext_ids_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
)
file.copy(catalog_path, backup_path, overwrite = FALSE)
utils::write.csv(catalog, catalog_path, row.names = FALSE, na = "")

message("External catalog ID assignment complete.")
message("Rows: ", nrow(catalog))
message("Blank IDs filled: ", filled)
message("Backup written: ", backup_path)
message("Updated catalog: ", catalog_path)
