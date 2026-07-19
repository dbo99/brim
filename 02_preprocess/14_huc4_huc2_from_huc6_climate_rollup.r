# ==== 14_huc4_huc2_from_huc6_climate_rollup.r ================================
##
## PURPOSE:
##   Create lightweight HUC4 and HUC2 climate/recharge tables by rolling up the
##   completed direct HUC6 PRISM/BCMv8 extraction.
##
## WHY:
##   HUC6 direct extraction is already complete and defensible.
##   Exact HUC4/HUC2 extraction is too slow because the parent polygons span
##   very large raster extents.
##
## OUTPUTS:
##   04_processed_data/rds/huc4_climate_recharge_table.rds
##   04_processed_data/rds/huc2_climate_recharge_table.rds
##
## NOTE:
##   PRISM precip is rolled up from all available HUC6 children.
##   BCMv8 recharge is reported only where the parent recharge valid fraction
##   is >= RECH_VALID_MIN. Otherwise recharge values are set to NA and a note
##   is carried for the popup.

source("00_config/config_paths.r")
source("03_functions/cache_helpers.r")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

# ==== 1. Settings ============================================================

RUN_TS <- make_timestamp()

MM_PER_INCH <- 25.4
ACREFT_M3 <- 1233.48184

RECH_VALID_MIN <- 0.75

# ==== 2. Read direct HUC6 table ==============================================

h6_path <- file.path(DIR$rds, "huc6_climate_recharge_table.rds")

if (!file.exists(h6_path)) {
  stop("Missing direct HUC6 climate/recharge table: ", h6_path)
}

h6 <- readRDS(h6_path)

required_cols <- c(
  "huc6",
  "huc6_name",
  "area_m2",
  "area_sqmi",
  "area_acres",
  "ppt_acft",
  "rech_acft",
  "ppt_valid_area_m2",
  "rech_valid_area_m2",
  "prnt_huc4_code",
  "prnt_huc4_name",
  "prnt_huc2_code",
  "prnt_huc2_name"
)

missing_cols <- setdiff(required_cols, names(h6))

if (length(missing_cols) > 0) {
  stop(
    "HUC6 table is missing required column(s): ",
    paste(missing_cols, collapse = ", "),
    "\nAvailable columns: ",
    paste(names(h6), collapse = ", ")
  )
}

# ==== 3. Helper functions ====================================================

save_rollup <- function(x, base_name) {
  
  save_rds_cached(
    x = x,
    timestamped_path = file.path(
      DIR$rds,
      timestamped_name(base_name, "rds", RUN_TS)
    ),
    latest_path = file.path(
      DIR$rds,
      paste0(base_name, ".rds")
    )
  )
}

rollup_from_huc6 <- function(h6, level) {
  
  if (level == 4) {
    code_col <- "prnt_huc4_code"
    name_col <- "prnt_huc4_name"
    out_code_col <- "huc4"
    out_name_col <- "huc4_name"
  } else if (level == 2) {
    code_col <- "prnt_huc2_code"
    name_col <- "prnt_huc2_name"
    out_code_col <- "huc2"
    out_name_col <- "huc2_name"
  } else {
    stop("Only HUC4 and HUC2 are supported by this rollup.")
  }
  
  out <- h6 |>
    dplyr::filter(
      !is.na(.data[[code_col]]),
      .data[[code_col]] != ""
    )
  
  ## HUC2 special case:
  ## Drop Pacific Northwest Region, matching the agreed HUC2 display scope.
  if (level == 2) {
    out <- out |>
      dplyr::filter(.data[[name_col]] != "Pacific Northwest Region")
  }
  
  out |>
    dplyr::mutate(
      roll_code = as.character(.data[[code_col]]),
      roll_name = as.character(.data[[name_col]])
    ) |>
    dplyr::group_by(.data$roll_code, .data$roll_name) |>
    dplyr::summarise(
      child_huc6_count = dplyr::n(),
      
      area_m2 = sum(.data$area_m2, na.rm = TRUE),
      area_sqmi = sum(.data$area_sqmi, na.rm = TRUE),
      area_acres = sum(.data$area_acres, na.rm = TRUE),
      
      ppt_acft = sum(.data$ppt_acft, na.rm = TRUE),
      ppt_valid_area_m2 = sum(.data$ppt_valid_area_m2, na.rm = TRUE),
      
      rech_acft = sum(.data$rech_acft, na.rm = TRUE),
      rech_valid_area_m2 = sum(.data$rech_valid_area_m2, na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ppt_kaf = .data$ppt_acft / 1000,
      rech_kaf = .data$rech_acft / 1000,
      
      ppt_mm = dplyr::if_else(
        .data$ppt_valid_area_m2 > 0,
        ((.data$ppt_acft * ACREFT_M3) / .data$ppt_valid_area_m2) * 1000,
        NA_real_
      ),
      ppt_in = .data$ppt_mm / MM_PER_INCH,
      
      rech_mm = dplyr::if_else(
        .data$rech_valid_area_m2 > 0,
        ((.data$rech_acft * ACREFT_M3) / .data$rech_valid_area_m2) * 1000,
        NA_real_
      ),
      rech_in = .data$rech_mm / MM_PER_INCH,
      
      map_mm = .data$ppt_mm,
      map_in = .data$ppt_in,
      
      ppt_valid_frac = dplyr::if_else(
        .data$area_m2 > 0,
        .data$ppt_valid_area_m2 / .data$area_m2,
        NA_real_
      ),
      rech_valid_frac = dplyr::if_else(
        .data$area_m2 > 0,
        .data$rech_valid_area_m2 / .data$area_m2,
        NA_real_
      ),
      
      rech_eff_pct = dplyr::if_else(
        .data$ppt_acft > 0,
        100 * .data$rech_acft / .data$ppt_acft,
        NA_real_
      ),
      
      bcmv8_recharge_note = dplyr::if_else(
        is.na(.data$rech_valid_frac) | .data$rech_valid_frac < RECH_VALID_MIN,
        paste0(
          "BCMv8 recharge not reported because less than ",
          round(RECH_VALID_MIN * 100),
          "% of this HUC is covered by the hydrologic-California BCMv8 raster domain."
        ),
        NA_character_
      ),
      
      ## Mask recharge for map display where BCMv8 coverage is inadequate.
      rech_mm = dplyr::if_else(!is.na(.data$bcmv8_recharge_note), NA_real_, .data$rech_mm),
      rech_in = dplyr::if_else(!is.na(.data$bcmv8_recharge_note), NA_real_, .data$rech_in),
      rech_acft = dplyr::if_else(!is.na(.data$bcmv8_recharge_note), NA_real_, .data$rech_acft),
      rech_kaf = dplyr::if_else(!is.na(.data$bcmv8_recharge_note), NA_real_, .data$rech_kaf),
      rech_eff_pct = dplyr::if_else(!is.na(.data$bcmv8_recharge_note), NA_real_, .data$rech_eff_pct),
      
      climate_summary_source = "Rolled up from direct HUC6 PRISM precipitation and BCMv8 recharge summaries, 1991-2020",
      climate_summary_run_ts = RUN_TS
    ) |>
    dplyr::rename(
      !!out_code_col := roll_code,
      !!out_name_col := roll_name
    ) |>
    dplyr::select(
      dplyr::all_of(c(out_code_col, out_name_col)),
      child_huc6_count,
      area_m2,
      area_sqmi,
      area_acres,
      map_mm,
      map_in,
      ppt_acft,
      ppt_kaf,
      rech_mm,
      rech_in,
      rech_acft,
      rech_kaf,
      rech_eff_pct,
      ppt_valid_frac,
      rech_valid_frac,
      ppt_valid_area_m2,
      rech_valid_area_m2,
      bcmv8_recharge_note,
      climate_summary_source,
      climate_summary_run_ts
    )
}

# ==== 4. Build HUC4 and HUC2 tables ==========================================

h4 <- rollup_from_huc6(h6, level = 4)
h2 <- rollup_from_huc6(h6, level = 2)

save_rollup(h4, "huc4_climate_recharge_table")
save_rollup(h2, "huc2_climate_recharge_table")

# ==== 5. QA ==================================================================

qa <- dplyr::bind_rows(
  h4 |>
    dplyr::mutate(layer_id = "huc4"),
  h2 |>
    dplyr::mutate(layer_id = "huc2")
) |>
  dplyr::group_by(.data$layer_id) |>
  dplyr::summarise(
    rows = dplyr::n(),
    area_sqmi_sum = sum(.data$area_sqmi, na.rm = TRUE),
    ppt_kaf_sum = sum(.data$ppt_kaf, na.rm = TRUE),
    rech_kaf_sum = sum(.data$rech_kaf, na.rm = TRUE),
    recharge_note_count = sum(!is.na(.data$bcmv8_recharge_note)),
    run_timestamp = RUN_TS,
    .groups = "drop"
  )

out_qa <- file.path(
  DIR$qa,
  paste0("huc4_huc2_from_huc6_rollup_qa_", RUN_TS, ".csv")
)

readr::write_csv(qa, out_qa)

message("\nSaved QA CSV:")
message("  ", out_qa)
print(qa)

message("\nDone: HUC4 and HUC2 climate/recharge rollup tables created.")
message("Latest outputs:")
message("  ", file.path(DIR$rds, "huc4_climate_recharge_table.rds"))
message("  ", file.path(DIR$rds, "huc2_climate_recharge_table.rds"))