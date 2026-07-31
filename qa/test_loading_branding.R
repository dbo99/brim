#!/usr/bin/env Rscript

## Source-only QA for approved DOI/BLM branding on the BRIM loading page.
## This builds an in-memory widget only; it does not read caches or write HTML.

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  "qa/test_loading_branding.R"
}
project_root <- normalizePath(
  file.path(dirname(script_path), ".."),
  mustWork = TRUE
)
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

required_packages <- c(
  "base64enc", "digest", "htmltools", "htmlwidgets", "leaflet"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing required loading-emblem QA package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

asset_specs <- list(
  doi = list(
    path = file.path("00_config", "doicolor.gif"),
    extension = "gif",
    size = 34643,
    sha256 = "f98df670f111f6ea6c300d0896aeda54c3f1033bbee02ed58cd819e1b751bd8c",
    mime_prefix = "data:image/gif;base64,",
    id = "brim-startup-doi-seal"
  ),
  blm = list(
    path = file.path("00_config", "blmlogo.svg"),
    extension = "svg",
    size = 81536,
    sha256 = "cab98ef6c9ec7d1a13f71b0aa8a9263faaa5580c0871354cc41bba53dc6134a4",
    mime_prefix = "data:image/svg+xml;base64,",
    id = "brim-startup-blm-emblem"
  )
)
topo_spec <- list(
  path = file.path("00_config", "Topo3whiteReversed.svg"),
  extension = "svg",
  size = 105628,
  sha256 = "8486a7f182caecfbf110a965259f2b03711d04e9538659821821209a49b4cb4e",
  mime_prefix = "data:image/svg+xml;base64,"
)

asset_bytes <- lapply(asset_specs, function(spec) {
  assert_true(
    file.exists(spec$path),
    paste("Approved loading-page asset is missing:", spec$path)
  )
  assert_true(
    identical(unname(file.info(spec$path)$size), spec$size),
    paste("Approved loading-page asset byte size changed:", spec$path)
  )
  assert_true(
    tolower(tools::file_ext(spec$path)) == spec$extension,
    paste("Approved loading-page asset extension changed:", spec$path)
  )
  assert_true(
    identical(
      digest::digest(
        spec$path,
        algo = "sha256",
        file = TRUE,
        serialize = FALSE
      ),
      spec$sha256
    ),
    paste("Approved loading-page asset checksum changed:", spec$path)
  )
  readBin(
    spec$path,
    what = "raw",
    n = as.integer(file.info(spec$path)$size)
  )
})
assert_true(
  file.exists(topo_spec$path) &&
    identical(unname(file.info(topo_spec$path)$size), topo_spec$size) &&
    tolower(tools::file_ext(topo_spec$path)) == topo_spec$extension,
  "Browser-safe topo background asset is missing or changed"
)
assert_true(
  identical(
    digest::digest(
      topo_spec$path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    topo_spec$sha256
  ),
  "Topo background SVG checksum changed"
)
topo_bytes <- readBin(
  topo_spec$path,
  what = "raw",
  n = as.integer(file.info(topo_spec$path)$size)
)
topo_source <- rawToChar(topo_bytes)
assert_true(
  grepl('viewBox="0 0 2688 1920"', topo_source, fixed = TRUE) &&
    !grepl(
      "<script|<foreignObject|(?:xlink:)?href=",
      topo_source,
      ignore.case = TRUE,
      perl = TRUE
    ),
  "Topo background SVG is unsafe or has the wrong viewport"
)

assert_true(
  identical(rawToChar(asset_bytes$doi[seq_len(6L)]), "GIF89a"),
  "Approved DOI seal GIF signature changed"
)
doi_dimensions <- c(
  width = as.integer(asset_bytes$doi[[7]]) +
    256L * as.integer(asset_bytes$doi[[8]]),
  height = as.integer(asset_bytes$doi[[9]]) +
    256L * as.integer(asset_bytes$doi[[10]])
)
assert_true(
  identical(doi_dimensions, c(width = 600L, height = 604L)),
  "Approved DOI seal GIF dimensions changed"
)
assert_true(
  grepl(
    'viewBox="0 0 540 504"',
    rawToChar(asset_bytes$blm[seq_len(2048L)]),
    fixed = TRUE
  ),
  "Approved BLM emblem viewBox changed"
)

source("03_functions/leaflet_loading_helpers.r")
m <- pt_add_startup_loading_overlay(leaflet::leaflet())

collect_tags <- function(node) {
  tags <- list()
  visit <- function(x) {
    if (inherits(x, "shiny.tag")) {
      tags[[length(tags) + 1L]] <<- x
      lapply(x$children, visit)
    } else if (is.list(x)) {
      lapply(x, visit)
    }
    invisible(NULL)
  }
  visit(node)
  tags
}

tags <- collect_tags(m$prepend)
tags_with_id <- function(id) {
  Filter(function(tag) identical(tag$attribs$id, id), tags)
}
tags_with_class <- function(class_name) {
  Filter(function(tag) {
    classes <- strsplit(tag$attribs$class %||% "", "[[:space:]]+")[[1]]
    class_name %in% classes
  }, tags)
}

overlays <- tags_with_id("brim-startup-loading-overlay")
assert_true(
  length(overlays) == 1L,
  "Loading page must contain exactly one startup overlay"
)
topo_style <- overlays[[1]]$attribs$style %||% ""
topo_style_prefix <- paste0(
  "--brim-topo-background: url('",
  topo_spec$mime_prefix
)
assert_true(
  startsWith(topo_style, topo_style_prefix) &&
    endsWith(topo_style, "');"),
  "Loading overlay does not use the embedded topo SVG"
)
topo_payload <- substring(
  topo_style,
  nchar(topo_style_prefix) + 1L,
  nchar(topo_style) - 3L
)
assert_true(
  identical(base64enc::base64decode(topo_payload), topo_bytes),
  "Embedded topo background bytes changed"
)

brand_marks <- Map(function(spec, expected_bytes) {
  matches <- tags_with_id(spec$id)
  assert_true(
    length(matches) == 1L && identical(matches[[1]]$name, "img"),
    paste("Loading page must contain exactly one branding image:", spec$id)
  )
  mark <- matches[[1]]
  assert_true(
    startsWith(mark$attribs$src, spec$mime_prefix),
    paste("Loading brand mark has the wrong embedded MIME type:", spec$id)
  )
  encoded_payload <- substring(
    mark$attribs$src,
    nchar(spec$mime_prefix) + 1L
  )
  assert_true(
    identical(base64enc::base64decode(encoded_payload), expected_bytes),
    paste("Embedded loading brand bytes changed:", spec$id)
  )
  assert_true(
    !grepl("file://|/Users/|https?://", mark$attribs$src),
    paste("Loading brand mark has a local or runtime web dependency:", spec$id)
  )
  mark_classes <- strsplit(
    mark$attribs$class %||% "",
    "[[:space:]]+"
  )[[1]]
  assert_true(
    identical(mark$attribs$alt, "") &&
      identical(mark$attribs[["aria-hidden"]], "true") &&
      identical(mark$attribs$draggable, "false") &&
      "brim-startup-brand-mark" %in% mark_classes &&
      is.null(mark$attribs$tabindex) &&
      is.null(mark$attribs$title) &&
      !any(startsWith(names(mark$attribs), "on")),
    paste("Loading brand accessibility attributes changed:", spec$id)
  )
  mark
}, asset_specs, asset_bytes)

lower_rows <- tags_with_class("brim-startup-lower")
progress_blocks <- tags_with_class("brim-startup-progress")
branding_groups <- tags_with_class("brim-startup-branding")
cards <- tags_with_class("brim-startup-card")
headings <- tags_with_class("brim-startup-heading")
statuses <- tags_with_id("brim-startup-loading-status")
assert_true(
  length(lower_rows) == 1L &&
    length(progress_blocks) == 1L &&
    length(branding_groups) == 1L &&
    length(cards) == 1L &&
    length(headings) == 1L &&
    length(statuses) == 1L,
  "Loading-page lower layout is incomplete or duplicated"
)
assert_true(
  identical(branding_groups[[1]]$attribs[["aria-hidden"]], "true"),
  "Decorative branding group must be hidden from assistive technology"
)

direct_card_classes <- vapply(
  Filter(function(x) inherits(x, "shiny.tag"), cards[[1]]$children),
  function(tag) tag$attribs$class %||% "",
  character(1)
)
assert_true(
  identical(
    direct_card_classes,
    c(
      "brim-startup-heading",
      "brim-photo-loader",
      "brim-startup-lower"
    )
  ),
  "Loading card must contain heading, flexible hero, and footer grid rows"
)

direct_heading_classes <- vapply(
  Filter(function(x) inherits(x, "shiny.tag"), headings[[1]]$children),
  function(tag) tag$attribs$class %||% "",
  character(1)
)
assert_true(
  identical(
    direct_heading_classes,
    c(
      "brim-startup-kicker",
      "brim-startup-title",
      "brim-startup-fullname",
      "brim-startup-subtitle"
    )
  ),
  "Loading heading content is incomplete or out of order"
)

direct_lower_classes <- vapply(
  Filter(function(x) inherits(x, "shiny.tag"), lower_rows[[1]]$children),
  function(tag) tag$attribs$class %||% "",
  character(1)
)
assert_true(
  identical(
    direct_lower_classes,
    c("brim-startup-progress", "brim-startup-branding")
  ),
  "Progress and branding are not separate siblings in the lower loading grid"
)
assert_true(
  length(progress_blocks[[1]]$children) == 1L &&
    identical(
      progress_blocks[[1]]$children[[1]]$attribs$id,
      "brim-startup-loading-status"
    ),
  "Loading progress must contain only the single status block"
)
status <- statuses[[1]]
assert_true(
  identical(status$name, "p") &&
    identical(status$attribs$role, "status") &&
    identical(status$attribs[["aria-live"]], "polite") &&
    identical(status$attribs[["aria-atomic"]], "true") &&
    grepl(
      "Initializing the BRIM browser map and core services",
      as.character(status),
      fixed = TRUE
    ),
  "Loading status block accessibility or initial wording changed"
)

direct_brand_ids <- vapply(
  Filter(function(x) inherits(x, "shiny.tag"), branding_groups[[1]]$children),
  function(tag) tag$attribs$id %||% "",
  character(1)
)
assert_true(
  identical(
    direct_brand_ids,
    c("brim-startup-doi-seal", "brim-startup-blm-emblem")
  ),
  "DOI must precede BLM and be the only other mark in the branding group"
)

markup <- paste(vapply(m$prepend, as.character, character(1)), collapse = "\n")
for (spec in asset_specs) {
  id_markup <- paste0('id="', spec$id, '"')
  assert_true(
    lengths(regmatches(
      markup,
      gregexpr(id_markup, markup, fixed = TRUE)
    )) == 1L,
    paste("Generated loading markup contains a duplicate mark:", spec$id)
  )
}

loading_source <- paste(
  readLines("03_functions/leaflet_loading_helpers.r", warn = FALSE),
  collapse = "\n"
)
core_source <- paste(
  readLines("03_functions/leaflet_core_helpers.r", warn = FALSE),
  collapse = "\n"
)
builder_source <- paste(
  readLines("05_map_build/04_build_portatreasure2_core_map.r", warn = FALSE),
  collapse = "\n"
)

required_loading_css <- c(
  "min-height: 100vh",
  "min-height: 100dvh",
  "padding: 12px",
  "--brim-topo-background",
  "background-image: var(--brim-topo-background)",
  "opacity: 0.24",
  "height: min(960px, calc(100vh - 24px))",
  "height: min(960px, calc(100dvh - 24px))",
  "max-height: calc(100dvh - 24px)",
  "grid-template-rows: auto minmax(0, 1fr) auto",
  "background: #ffffff",
  ".brim-startup-heading",
  ".brim-photo-loader",
  "background-size: cover",
  "background-position: center top",
  "preserveAspectRatio='xMidYMin slice'",
  ".brim-startup-status",
  "min-height: 44px",
  ".brim-startup-lower",
  "grid-template-columns: minmax(0, 1fr) auto",
  ".brim-startup-branding",
  "--brim-doi-width: 120px",
  "--brim-blm-width: 129px",
  "--brim-brand-clearspace: 55px",
  "grid-template-columns: var(--brim-doi-width) var(--brim-blm-width)",
  "gap: var(--brim-brand-clearspace)",
  "padding: var(--brim-brand-clearspace)",
  ".brim-startup-brand-mark",
  "min-width: 47px",
  "height: auto",
  "pointer-events: none",
  "animation: none",
  "@media (max-width: 760px)",
  "--brim-doi-width: 80px",
  "--brim-blm-width: 86px",
  "--brim-brand-clearspace: 37px",
  "@media (max-width: 480px)",
  "--brim-doi-width: 55px",
  "--brim-blm-width: 59px",
  "--brim-brand-clearspace: 26px",
  "@media (max-width: 260px)",
  "@media (max-height: 900px) and (min-width: 761px)",
  "--brim-doi-width: 110px",
  "--brim-blm-width: 119px",
  "--brim-brand-clearspace: 51px",
  "@media (max-height: 720px) and (min-width: 761px)",
  "--brim-doi-width: 85px",
  "--brim-blm-width: 91px",
  "--brim-brand-clearspace: 39px",
  '--brim-headline-font: "Franklin Gothic Condensed", "Franklin Gothic Medium Cond", "Arial Narrow", "Aptos Narrow", "Helvetica Neue", Arial, sans-serif',
  '--brim-body-font: Garamond, "Adobe Garamond Pro", "EB Garamond", Georgia, serif'
)
for (required_text in required_loading_css) {
  assert_true(
    grepl(required_text, loading_source, fixed = TRUE),
    paste("Missing loading-page branding rule:", required_text)
  )
}

css_rule <- function(selector) {
  rule_start <- regexpr(paste0(selector, " {"), loading_source, fixed = TRUE)[1]
  assert_true(
    rule_start > 0,
    paste("Missing loading-page CSS selector:", selector)
  )
  rule_tail <- substring(loading_source, rule_start)
  rule_end <- regexpr("}", rule_tail, fixed = TRUE)[1]
  substring(rule_tail, 1L, rule_end)
}

overlay_rule <- css_rule("#brim-startup-loading-overlay")
card_rule <- css_rule(".brim-startup-card")
hero_rule <- css_rule(".brim-photo-loader")
assert_true(
  grepl("min-height: 100vh", overlay_rule, fixed = TRUE) &&
    grepl("min-height: 100dvh", overlay_rule, fixed = TRUE) &&
    grepl("align-items: center", overlay_rule, fixed = TRUE) &&
    grepl("justify-content: center", overlay_rule, fixed = TRUE),
  "Loading overlay must use viewport-aware centering"
)
assert_true(
  grepl(
    "grid-template-rows: auto minmax(0, 1fr) auto",
    card_rule,
    fixed = TRUE
  ) &&
    grepl(
      "max-height: calc(100dvh - 24px)",
      card_rule,
      fixed = TRUE
    ),
  "Loading card must use a viewport-capped three-row grid"
)
assert_true(
  grepl("min-height: 0", hero_rule, fixed = TRUE) &&
    grepl("background-size: cover", hero_rule, fixed = TRUE) &&
    grepl("background-position: center top", hero_rule, fixed = TRUE) &&
    !grepl("aspect-ratio:", hero_rule, fixed = TRUE),
  "Flexible hero must preserve its upper composition without distortion"
)

laptop_viewports <- data.frame(
  width = c(1512, 1440, 1280),
  height = c(982, 900, 800)
)
laptop_viewports$card_height <- pmin(
  960,
  laptop_viewports$height - 24
)
laptop_viewports$vertical_margin <- (
  laptop_viewports$height - laptop_viewports$card_height
) / 2
assert_true(
  identical(laptop_viewports$card_height, c(958, 876, 776)) &&
    all(laptop_viewports$width > 760) &&
    all(laptop_viewports$vertical_margin >= 12),
  "Representative laptop viewport-fit calculations changed"
)

for (selector in c(
  ".brim-startup-kicker",
  ".brim-startup-title",
  ".brim-startup-fullname"
)) {
  assert_true(
    grepl(
      "font-family: var(--brim-headline-font)",
      css_rule(selector),
      fixed = TRUE
    ),
    paste("Headline stack is not applied to:", selector)
  )
}
assert_true(
  grepl(
    "font-family: var(--brim-body-font)",
    css_rule(".brim-startup-subtitle"),
    fixed = TRUE
  ),
  "Garamond-oriented body stack is not applied to descriptive copy"
)
assert_true(
  grepl(
    'font-family: Inter, "Segoe UI", Roboto, Arial, Helvetica, sans-serif',
    css_rule(".brim-startup-status"),
    fixed = TRUE
  ),
  "Loading status must retain a legible local sans-serif stack"
)
status_sequence <- c(
  "Initializing the BRIM browser map and core services…",
  "Preparing cached resource datasets and screening layers…",
  "Loading live-condition interfaces and resource-review tools…",
  "Finalizing map annotations, legends, and browser presentation…",
  "BRIM is ready."
)
for (status_message in status_sequence) {
  assert_true(
    grepl(status_message, loading_source, fixed = TRUE),
    paste("Missing professional loading status:", status_message)
  )
}
assert_true(
  grepl("statusByStep[stepName]", loading_source, fixed = TRUE) &&
    !grepl(
      "brim-startup-steps|brim-loading-step-|stepOrder|setStep",
      loading_source
    ) &&
    !grepl("Labels and final styling", loading_source, fixed = TRUE),
  "Superseded bullet-progress implementation or wording remains"
)
assert_true(
  !grepl(
    "@font-face|fonts[.]googleapis|[.](woff2?|ttf|otf|eot)",
    loading_source,
    ignore.case = TRUE,
    perl = TRUE
  ),
  "Loading-page source introduced an external or embedded font asset"
)

minimum_clearspace <- function(width) {
  ceiling(width * (20 / 47))
}
assert_true(
  identical(
    minimum_clearspace(c(47, 100, 133)),
    c(20, 43, 57)
  ),
  "Official 47/100/133 px clear-space calculations changed"
)

brand_configs <- list(
  desktop = c(doi = 120, blm = 129, clearspace = 55),
  laptop = c(doi = 110, blm = 119, clearspace = 51),
  stacked = c(doi = 80, blm = 86, clearspace = 37),
  compact = c(doi = 55, blm = 59, clearspace = 26),
  short = c(doi = 85, blm = 91, clearspace = 39)
)
for (config_name in names(brand_configs)) {
  config <- brand_configs[[config_name]]
  widths <- config[c("doi", "blm")]
  required_clearspace <- minimum_clearspace(widths)
  assert_true(
    all(widths >= 47),
    paste("Brand mark is below 47 px in configuration:", config_name)
  )
  assert_true(
    max(required_clearspace) <= config[["clearspace"]],
    paste("Shared gap or outer clear space is insufficient:", config_name)
  )

  rendered_heights <- c(
    doi = widths[["doi"]] * 604 / 600,
    blm = widths[["blm"]] * 504 / 540
  )
  assert_true(
    abs(diff(rendered_heights)) < 1,
    paste("Brand mark heights are not optically balanced:", config_name)
  )
}

branding_rule <- css_rule(".brim-startup-branding")
brand_mark_rule <- css_rule(".brim-startup-brand-mark")
assert_true(
  grepl("display: grid", branding_rule, fixed = TRUE) &&
    grepl("gap: var(--brim-brand-clearspace)", branding_rule, fixed = TRUE) &&
    grepl("padding: var(--brim-brand-clearspace)", branding_rule, fixed = TRUE) &&
    !grepl("position:", branding_rule, fixed = TRUE) &&
    !grepl("position:", brand_mark_rule, fixed = TRUE),
  "Branding group must reserve its gap and outer clearance in normal flow"
)

abandoned_map_tokens <- c(
  "pane_blm_emblem_background",
  "pt_add_blm_emblem_background",
  "_ptBlmEmblemCleanup",
  "pt-blm-emblem-background"
)
map_sources <- paste(core_source, builder_source, sep = "\n")
for (token in abandoned_map_tokens) {
  assert_true(
    !grepl(token, map_sources, fixed = TRUE),
    paste("Abandoned interactive-map emblem code remains:", token)
  )
}

for (mark in brand_marks) {
  assert_true(
    !grepl(
      "addEventListener|onclick|onmouseover",
      as.character(mark)
    ),
    "Loading brand marks must not install event listeners"
  )
}
assert_true(
  !grepl("<a[[:space:]>]", as.character(branding_groups[[1]])),
  "Loading brand marks must not be linked"
)

allowed_changes <- c(
  "00_config/Topo3whiteReversed.svg",
  "00_config/doicolor.gif",
  "00_config/blmlogo.svg",
  "03_functions/leaflet_loading_helpers.r",
  "qa/test_loading_branding.R"
)
status_lines <- system2(
  "git",
  c("status", "--porcelain", "--untracked-files=all"),
  stdout = TRUE
)
changed_paths <- trimws(substring(status_lines, 4L))
assert_true(
  all(changed_paths %in% allowed_changes),
  paste(
    "Unrelated source changes present:",
    paste(setdiff(changed_paths, allowed_changes), collapse = ", ")
  )
)
assert_true(
  !any(grepl("\\.html?$", changed_paths, ignore.case = TRUE)),
  "Generated HTML must not be added by this source-only change"
)
assert_true(
  !any(grepl("\\.(woff2?|ttf|otf|eot)$", changed_paths, ignore.case = TRUE)),
  "A font binary must not be added by this loading-page change"
)

message("DOI/BLM loading-page branding source QA passed.")
