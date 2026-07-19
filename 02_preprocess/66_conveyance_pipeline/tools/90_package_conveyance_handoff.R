# ==== 90_package_conveyance_handoff.R ========================================
#
# Packages the complete BRIM conveyance workflow for transfer to another chat.
#
# Produces in Downloads:
#   BRIM_conveyance_pipeline_handoff_YYYYMMDD_HHMMSS.zip
#   BRIM_conveyance_pipeline_handoff_prompt_YYYYMMDD_HHMMSS.txt
#
# The ZIP includes the numbered entry script, staged R modules, configuration
# and decision tables, both raw shapefile bundles, canonical GeoPackage,
# BRIM-ready exports, QA outputs, pilot HTML, handoff prompt, and an MD5 manifest.
# ==============================================================================

PACKAGER_VERSION <- "CONVEYANCE_HANDOFF_PACKAGER_20260715_04"

PROJECT_ROOT <- normalizePath(
  "C:/Users/doconnor/OneDrive - DOI/Documents/brim_conveyance_redo_july",
  winslash = "/",
  mustWork = TRUE
)

ENTRY_SCRIPT <- file.path(
  PROJECT_ROOT,
  "preprocessors",
  "66_build_conveyance_pipeline.R"
)

PIPELINE_ROOT <- file.path(
  PROJECT_ROOT,
  "preprocessors",
  "66_conveyance_pipeline"
)

PILOT_HTML <- file.path(
  PIPELINE_ROOT,
  "output",
  "html",
  "brim_conveyance_pilot.html"
)

CANONICAL_GPKG <- file.path(
  PIPELINE_ROOT,
  "output",
  "canonical",
  "brim_conveyance_master.gpkg"
)

required_paths <- c(
  ENTRY_SCRIPT,
  PIPELINE_ROOT,
  file.path(PIPELINE_ROOT, "R"),
  file.path(PIPELINE_ROOT, "config"),
  file.path(PIPELINE_ROOT, "input", "raw"),
  PILOT_HTML,
  CANONICAL_GPKG
)

missing_paths <- required_paths[
  !file.exists(required_paths) &
    !dir.exists(required_paths)
]

if (length(missing_paths) > 0L) {
  stop(
    "The handoff cannot be packaged because these required paths are missing:\n  ",
    paste(missing_paths, collapse = "\n  "),
    "\n\nRun preprocessors/66_build_conveyance_pipeline.R successfully first."
  )
}

raw_dir <- file.path(
  PIPELINE_ROOT,
  "input",
  "raw"
)

for (source_basename in c("majorconveyance", "WW_Canals")) {
  source_files <- list.files(
    raw_dir,
    pattern = paste0("^", source_basename, "\\."),
    ignore.case = TRUE,
    full.names = TRUE
  )

  if (!any(
    grepl("\\.shp$", source_files, ignore.case = TRUE)
  )) {
    stop(
      "Missing raw shapefile bundle in the pipeline for:\n  ",
      source_basename
    )
  }
}

downloads_dir <- file.path(
  Sys.getenv("USERPROFILE"),
  "Downloads"
)

if (!dir.exists(downloads_dir)) {
  stop(
    "Could not locate the Windows Downloads folder:\n  ",
    downloads_dir
  )
}

timestamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

handoff_basename <- paste0(
  "BRIM_conveyance_pipeline_handoff_",
  timestamp
)

zip_path <- file.path(
  downloads_dir,
  paste0(handoff_basename, ".zip")
)

prompt_path <- file.path(
  downloads_dir,
  paste0(
    "BRIM_conveyance_pipeline_handoff_prompt_",
    timestamp,
    ".txt"
  )
)

stage_root <- file.path(
  tempdir(),
  paste0(
    "cvy_handoff_",
    format(Sys.time(), "%H%M%S")
  )
)

if (dir.exists(stage_root)) {
  unlink(
    stage_root,
    recursive = TRUE,
    force = TRUE
  )
}

dir.create(
  stage_root,
  recursive = TRUE,
  showWarnings = FALSE
)

stage_project <- file.path(
  stage_root,
  "BRIM_conveyance_handoff"
)

dir.create(
  stage_project,
  recursive = TRUE,
  showWarnings = FALSE
)

copy_file_checked <- function(from, to) {
  dir.create(
    dirname(to),
    recursive = TRUE,
    showWarnings = FALSE
  )

  copied <- file.copy(
    from = from,
    to = to,
    overwrite = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (!isTRUE(copied) || !file.exists(to)) {
    stop(
      "Failed to copy into handoff staging:\n  ",
      from,
      "\n  -> ",
      to
    )
  }

  invisible(TRUE)
}

copy_directory_tree <- function(from, to) {
  from <- normalizePath(
    from,
    winslash = "/",
    mustWork = TRUE
  )

  dir.create(
    to,
    recursive = TRUE,
    showWarnings = FALSE
  )

  source_directories <- list.dirs(
    from,
    recursive = TRUE,
    full.names = TRUE
  )

  relative_directories <- substring(
    source_directories,
    nchar(from) + 2L
  )

  relative_directories[
    source_directories == from
  ] <- ""

  for (relative_directory in relative_directories) {
    if (!nzchar(relative_directory)) {
      next
    }

    dir.create(
      file.path(to, relative_directory),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  source_files <- list.files(
    from,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    include.dirs = FALSE
  )

  if (length(source_files) == 0L) {
    return(invisible(TRUE))
  }

  relative_files <- substring(
    source_files,
    nchar(from) + 2L
  )

  destination_files <- file.path(
    to,
    relative_files
  )

  destination_directories <- unique(
    dirname(destination_files)
  )

  for (destination_directory in destination_directories) {
    dir.create(
      destination_directory,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  copy_results <- logical(length(source_files))

  for (file_index in seq_along(source_files)) {
    copy_results[[file_index]] <- file.copy(
      from = source_files[[file_index]],
      to = destination_files[[file_index]],
      overwrite = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
  }

  copy_results <- copy_results &
    file.exists(destination_files)

  if (!all(copy_results)) {
    failed_index <- which(!copy_results)[[1]]

    stop(
      "Failed to copy ",
      sum(!copy_results),
      " file(s) into handoff staging.\n",
      "First failure:\n  ",
      source_files[[failed_index]],
      "\n  -> ",
      destination_files[[failed_index]]
    )
  }

  invisible(TRUE)
}

copy_file_checked(
  ENTRY_SCRIPT,
  file.path(
    stage_project,
    "preprocessors",
    basename(ENTRY_SCRIPT)
  )
)

copy_directory_tree(
  PIPELINE_ROOT,
  file.path(
    stage_project,
    "preprocessors",
    basename(PIPELINE_ROOT)
  )
)

remove_patterns <- c(
  "\\.tmp$",
  "\\.lock$",
  "\\.aux\\.xml$",
  "\\.gpkg-wal$",
  "\\.gpkg-shm$",
  "\\.DS_Store$",
  "Thumbs\\.db$"
)

staged_files_initial <- list.files(
  stage_project,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  include.dirs = FALSE
)

for (pattern in remove_patterns) {
  candidates <- staged_files_initial[
    grepl(
      pattern,
      staged_files_initial,
      ignore.case = TRUE
    )
  ]

  if (length(candidates) > 0L) {
    unlink(
      candidates,
      force = TRUE
    )
  }
}

handoff_prompt <- paste0(
"BRIM CONVEYANCE PIPELINE — HANDOFF TO MAIN BRIM DEVELOPMENT CHAT\n",
"================================================================\n\n",

"PURPOSE\n",
"-------\n",
"This ZIP transfers the complete, reproducible California conveyance-building ",
"workflow developed as a sandbox before integration into BRIM. Do not treat the ",
"RDS files as unexplained final products. The durable record is the raw source ",
"data + configuration/decision tables + staged R pipeline + QA outputs + canonical ",
"GeoPackage. Every derived BRIM export must remain rebuildable from those inputs.\n\n",

"WORKING LOCATION USED DURING DEVELOPMENT\n",
"----------------------------------------\n",
"C:/Users/doconnor/OneDrive - DOI/Documents/brim_conveyance_redo_july\n\n",

"RECOMMENDED LOCATION IN THE MAIN BRIM PROJECT\n",
"---------------------------------------------\n",
"preprocessors/66_build_conveyance_pipeline.R\n",
"preprocessors/66_conveyance_pipeline/\n\n",

"SINGLE BUILD ENTRY POINT\n",
"------------------------\n",
"source(\"preprocessors/66_build_conveyance_pipeline.R\")\n\n",

"ARCHITECTURE\n",
"------------\n",
"The model deliberately separates logical facilities from meaningful physical ",
"segments and label anchors:\n\n",
"1. facilities: one logical named facility, with canonical name, aliases, parent ",
"system, ownership/operation, project hierarchy, status, and display rank;\n",
"2. conveyance_segments: line geometry, allowing multiple segments per facility ",
"and preserving meaningful canal/tunnel/pipeline/penstock/tailrace/siphon or ",
"ownership/project transitions;\n",
"3. conveyance_labels: dedicated label points with lbl, lbl_full, priority and ",
"zoom thresholds so labels are not duplicated for every line segment.\n\n",
"Do not dissolve all pieces of a named canal into a single geometry when that ",
"would erase meaningful component or type transitions. Contiguous same-facility, ",
"same-type source pieces may be combined when defensible.\n\n",

"RAW INPUTS\n",
"----------\n",
"The pipeline includes complete copies of:\n",
"- Major Conveyance: input/raw/majorconveyance.*\n",
"- DeltaMAPP canals: input/raw/WW_Canals.*\n\n",
"Do not edit these raw files. Geometry choices and corrections belong in the ",
"decision ledger, override tables, supplemental registry, or staged surgery code.\n\n",

"IMPORTANT DATA-MODEL PRINCIPLES\n",
"-------------------------------\n",
"- canonical_name is the preferred public name;\n",
"- aliases and parent-system names are retained unless clearly wrong;\n",
"- ownership, operation, project membership, and program affiliation are separate;\n",
"- project membership is many-to-many and supports project_family, project_name, ",
"project_division, project_unit and project_subunit;\n",
"- CVP/SWP joint-use membership does not automatically imply identical ownership;\n",
"- specifically named facilities should not split merely because source operator ",
"or capitalization differs;\n",
"- generic names such as Main Canal require additional system/spatial disambiguation;\n",
"- source row IDs and internal IDs stay in QA/crosswalk tables, not ordinary public popups.\n\n",

"MAJOR COMPLETED WORK\n",
"--------------------\n",
"- audited Major Conveyance and DeltaMAPP source geometry and names;\n",
"- reviewed or rule-resolved all strong different-name overlap clusters;\n",
"- preserved aliases and parent/component relationships;\n",
"- implemented selected Major, selected DeltaMAPP, trimmed combination, split-component, ",
"separate-facility and omitted-unresolved decisions;\n",
"- separated Clear Creek Tunnel from Judge Francis Carr penstocks;\n",
"- separated Spring Creek penstocks and powerplant tailrace;\n",
"- kept San Luis Drain separate from San Luis Canal;\n",
"- split Old River Pipeline from the Los Vaqueros Intake Pipeline continuation;\n",
"- added authoritative CVP/SWP and other Reclamation project hierarchy rules;\n",
"- added expected-CVP and unassigned-Reclamation QA reports;\n",
"- stabilized named-facility identity so capitalization/operator variants such as ",
"South Coast Conduit do not create separate logical facilities;\n",
"- implemented public labels with dedicated tooltip anchors and zoom thresholds;\n",
"- cleaned the public popup so source rows/internal IDs appear only in QA artifacts;\n",
"- produced a canonical multi-table GeoPackage plus BRIM-ready RDS exports.\n\n",

"CURRENT OUTPUTS\n",
"---------------\n",
"output/canonical/brim_conveyance_master.gpkg\n",
"output/brim_exports/conveyance_segments_brim.rds\n",
"output/brim_exports/conveyance_labels_brim.rds\n",
"output/brim_exports/conveyance_facilities_brim.rds\n",
"output/csv/*.csv\n",
"output/html/brim_conveyance_pilot.html\n\n",

"PUBLIC-PILOT EXPECTATIONS\n",
"-------------------------\n",
"The public pilot should support search, ownership class, project family, named ",
"project/system, facility group, display rank, low-confidence toggle and lbl checkbox. ",
"Labels should be driven by conveyance_labels and should report the number shown at ",
"the current zoom. The user-facing popup should end with only a concise QA/QC note.\n\n",

"KNOWN PENDING ITEMS\n",
"-------------------\n",
"1. Delta-Mendota Canal / California Aqueduct Intertie: supplemental registry entry ",
"SUP001 is prepared. The preferred source may be the CalSim3 layer once the model ID, ",
"source layer and ID field/value are identified. It must receive both CVP and SWP ",
"project memberships and be modeled as a bidirectional pipeline intertie.\n",
"2. Sly Park/Jenkinson Lake completeness: Camino Conduit is expected but may require ",
"authoritative supplemental geometry. El Dorado Main and related distribution mains ",
"are optional/local-supporting rather than required for the first major/medium layer.\n",
"3. BLM-land preprocessing is still a substantive, untested data stage—not merely ",
"a BRIM UI task. The current handoff reserves the intended fields and integration ",
"concept, but the on/off-BLM classification, crossing calculations, minimum/maximum ",
"distance-to-BLM values, field-office attribution and facility-level aggregation have ",
"not yet been implemented and validated end to end. Desired segment fields include ",
"blm_crosses, blm_length_mi, blm_pct_length, blm_nearest_mi, blm_crossing_count and ",
"blm_field_offices, plus facility aggregates. This stage may expose additional geometry, ",
"identity, segmentation, CRS, performance or aggregation issues that require data-side ",
"corrections before BRIM visualization is finalized.\n",
"4. Review qa_same_name_multiple_facilities.csv, qa_expected_cvp_facilities.csv, ",
"qa_reclamation_unassigned.csv and qa_supplemental_geometry_registry.csv after every build.\n",
"5. Preserve the case-35 split-or-omit rule: use defensible West Interception Canal and ",
"Live Oak Canal features where supported; do not force East Interception Canal onto ",
"those reaches; omit residual unresolved local pieces from the curated layer while ",
"retaining them in the raw sources.\n\n",

"NEXT DEVELOPMENT ORDER\n",
"----------------------\n",
"1. Run the complete pipeline unchanged and inspect the included pilot HTML and QA CSVs.\n",
"2. Correct only demonstrated identity, label, project or geometry defects through the ",
"appropriate config or staged module—not by manually editing a final RDS.\n",
"3. Add the DMC–California Aqueduct Intertie through the supplemental registry and a ",
"reproducible supplemental-geometry ingestion stage.\n",
"4. Resolve any remaining expected-CVP or Reclamation-project QA gaps.\n",
"5. Build and test the BLM-land preprocessing stage against the accepted canonical ",
"segments. Validate on/off-BLM flags, crossing lengths and counts, nearest-distance ",
"calculations, field-office attribution, facility aggregation, edge cases and performance. ",
"Treat any resulting segmentation or identity defects as data-pipeline work, not merely ",
"display issues.\n",
"6. Only after that data stage passes QA, integrate the resulting segment and label RDS ",
"files into BRIM while preserving the ",
"full preprocessor as the rebuild source of truth.\n\n",

"BRIM INTEGRATION INTENT\n",
"-----------------------\n",
"The eventual BRIM layer should expose a robust, filterable hierarchy including:\n",
"- ownership class: federal, California state, local/regional public, private, joint/multiple, unknown;\n",
"- project family: CVP, SWP, other Reclamation, other state, other public systems;\n",
"- named projects/systems and CVP/SWP divisions, units, branches and subunits;\n",
"- facility groups/types: canals/aqueducts, pipelines/tunnels, drains/wasteways, ",
"power conveyance, interties, laterals/supporting facilities;\n",
"- display rank and status;\n",
"- canonical-name/alias/parent-system/owner/operator search;\n",
"- label checkbox using the dedicated lbl field and label-point layer;\n",
"- after a separately validated data-preprocessing stage, BLM-crossing and custom ",
"minimum/maximum distance-to-BLM filtering. These controls must not be wired to placeholder ",
"or untested values.\n\n",

"READINESS STATUS\n",
"----------------\n",
"The current conveyance geometry, naming, project hierarchy, labels and pilot interface ",
"have received useful review, but the complete dataset is not yet declared production-ready. ",
"The receiving chat should expect some remaining data engineering and QA work, especially ",
"for supplemental facilities and BLM-land preprocessing. Do not assume that all remaining ",
"tasks are styling, legend organization or UI wiring.\n\n",

"NON-NEGOTIABLE HANDOFF RULE\n",
"---------------------------\n",
"Do not replace this source-to-output workflow with an unexplained edited shapefile or ",
"RDS. Maintain source lineage, stable facility/segment IDs, decision evidence, QA reports ",
"and a clean rebuild path from the two raw shapefiles.\n\n",

"PACKAGE BUILD INFORMATION\n",
"-------------------------\n",
"Packager version: ", PACKAGER_VERSION, "\n",
"Package created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n",
"Source project root: ", PROJECT_ROOT, "\n"
)

writeLines(
  handoff_prompt,
  con = file.path(
    stage_project,
    "HANDOFF_PROMPT.txt"
  ),
  useBytes = TRUE
)

writeLines(
  handoff_prompt,
  con = prompt_path,
  useBytes = TRUE
)

staged_files <- list.files(
  stage_project,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  include.dirs = FALSE
)


if (length(staged_files) == 0L) {
  stop(
    "Handoff staging completed without any files."
  )
}

message(
  "Handoff staging complete: ",
  length(staged_files),
  " files before manifest."
)

relative_paths <- substring(
  staged_files,
  nchar(stage_project) + 2L
)

manifest <- data.frame(
  relative_path = relative_paths,
  size_bytes = unname(
    file.info(staged_files)$size
  ),
  modified_time = format(
    file.info(staged_files)$mtime,
    "%Y-%m-%d %H:%M:%S"
  ),
  md5 = unname(
    tools::md5sum(staged_files)
  ),
  stringsAsFactors = FALSE
)

manifest <- manifest[
  order(manifest$relative_path),
]

utils::write.csv(
  manifest,
  file = file.path(
    stage_project,
    "PACKAGE_MANIFEST.csv"
  ),
  row.names = FALSE,
  na = ""
)

if (file.exists(zip_path)) {
  unlink(
    zip_path,
    force = TRUE
  )
}

old_wd <- getwd()
on.exit(
  setwd(old_wd),
  add = TRUE
)

setwd(stage_root)

files_to_zip <- list.files(
  "BRIM_conveyance_handoff",
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  include.dirs = FALSE
)

if (length(files_to_zip) == 0L) {
  stop(
    "No staged files were found for ZIP creation."
  )
}

zip_success <- FALSE

if (requireNamespace("zip", quietly = TRUE)) {
  zip::zipr(
    zipfile = zip_path,
    files = files_to_zip,
    root = stage_root,
    include_directories = FALSE
  )

  zip_success <- file.exists(zip_path)
}

if (!zip_success) {
  utils::zip(
    zipfile = zip_path,
    files = files_to_zip,
    flags = "-r9X"
  )

  zip_success <- file.exists(zip_path)
}

if (!zip_success) {
  stop(
    "The handoff files were staged successfully, but the ZIP could not be created.\n",
    "Staging directory:\n  ",
    stage_root
  )
}

message("\nBRIM conveyance handoff package complete.")
message("ZIP:\n  ", normalizePath(zip_path, winslash = "/", mustWork = TRUE))
message("Prompt:\n  ", normalizePath(prompt_path, winslash = "/", mustWork = TRUE))
message("Files packaged: ", nrow(manifest))
message(
  "ZIP size: ",
  round(file.info(zip_path)$size / 1024^2, 1),
  " MB"
)
