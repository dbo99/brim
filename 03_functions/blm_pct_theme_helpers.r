# ==== blm_pct_theme_helpers.r ===============================================
##
## PURPOSE:
##   Keep BRIM's fixed BLM-managed-land percentage classification in one
##   neutral helper shared by Bulletin 118 and the six HUC levels.

PT_BLM_PCT_BIN_LEVELS <- c(
  "0%",
  ">0\u20131%",
  ">1\u20135%",
  ">5\u201315%",
  ">15\u201330%",
  ">30\u201350%",
  ">50\u201375%",
  ">75%",
  "Missing"
)

PT_BLM_PCT_COLORS <- c(
  "0%" = "#F2F2F2",
  ">0\u20131%" = "#F1E6F4",
  ">1\u20135%" = "#DFC7E5",
  ">5\u201315%" = "#C9A3D2",
  ">15\u201330%" = "#AA78B7",
  ">30\u201350%" = "#87539A",
  ">50\u201375%" = "#673A7B",
  ">75%" = "#452357",
  "Missing" = "#9E9E9E"
)

if (!identical(names(PT_BLM_PCT_COLORS), PT_BLM_PCT_BIN_LEVELS)) {
  stop("Shared %BLM labels and colors are not aligned.", call. = FALSE)
}

pt_blm_pct_values <- function(x, label = "%BLM values") {
  values <- suppressWarnings(as.numeric(x))
  if (any(!is.na(values) & (values < 0 | values > 100))) {
    stop(label, " must be within 0 to 100.", call. = FALSE)
  }
  values
}

pt_blm_pct_bin <- function(x) {
  x <- pt_blm_pct_values(x)
  out <- rep("Missing", length(x))

  out[!is.na(x) & x == 0] <- "0%"
  out[!is.na(x) & x > 0 & x <= 1] <- ">0\u20131%"
  out[!is.na(x) & x > 1 & x <= 5] <- ">1\u20135%"
  out[!is.na(x) & x > 5 & x <= 15] <- ">5\u201315%"
  out[!is.na(x) & x > 15 & x <= 30] <- ">15\u201330%"
  out[!is.na(x) & x > 30 & x <= 50] <- ">30\u201350%"
  out[!is.na(x) & x > 50 & x <= 75] <- ">50\u201375%"
  out[!is.na(x) & x > 75] <- ">75%"

  factor(out, levels = PT_BLM_PCT_BIN_LEVELS, ordered = TRUE)
}

pt_blm_pct_legend_rows <- function(x) {
  bins <- pt_blm_pct_bin(x)
  counts <- table(factor(bins, levels = PT_BLM_PCT_BIN_LEVELS))

  lapply(seq_along(PT_BLM_PCT_BIN_LEVELS), function(i) {
    level <- PT_BLM_PCT_BIN_LEVELS[[i]]
    list(
      color = unname(PT_BLM_PCT_COLORS[[level]]),
      label = level,
      count = unname(as.integer(counts[[i]]))
    )
  })
}
