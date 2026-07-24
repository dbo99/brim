# UIC aquifer-exemptions pipeline

This developer/research pipeline preserves source-specific records, attributes,
exact overlaps, and provenance for audit, comparison, and standalone sandbox
review. It never promotes a refresh implicitly, and none of its products are
read by the normal production BRIM map.

## Data locations

Generated products follow BRIM’s external-data policy and are ignored by Git:

- immutable raw snapshots:
  `01_raw_data/uic_aquifer_exemptions/raw_snapshots/<snapshot_id>/`
- review candidates:
  `04_processed_data/uic_aquifer_exemptions/candidate/<candidate_id>/`
- approved comparison baseline:
  `04_processed_data/uic_aquifer_exemptions/approved/`
- prior approved products:
  `04_processed_data/uic_aquifer_exemptions/archive/`
- checks and QA:
  `04_processed_data/uic_aquifer_exemptions/{checks,qa}/`

EPA reference points and EPA county-location records are downloaded and
standardized for QA. The three configured polygon sources also receive
map-ready and label products for the standalone sandbox; those products are
not Local BRIM layers.

## Commands

Run from the repository root:

```r
source("02_preprocess/67_build_uic_aquifer_exemptions.R")

# Metadata, schema, object IDs, counts, and approved comparison.
# When an approved snapshot exists, current records are retrieved only into a
# temporary directory for ID/attribute/geometry comparison. Nothing is replaced.
check <- run_uic_pipeline("check_only")

# Complete immutable retrieval, processing, comparison, and candidate QA.
candidate <- run_uic_pipeline("refresh_candidate")

# Same controlled candidate workflow, explicitly documenting a forced review.
candidate <- run_uic_pipeline("force_rebuild")

# Standalone review map; never changes the production BRIM map.
run_uic_pipeline(
  "build_sandbox_map",
  candidate_id = candidate$candidate_id,
  data_variant = "candidate"
)

# Deliberate promotion only after reviewing the candidate QA and comparison.
run_uic_pipeline(
  "approve_candidate",
  candidate_id = candidate$candidate_id,
  confirm = "APPROVE UIC CANDIDATE"
)
```

`approve_candidate` refuses to proceed without the exact candidate ID, the
exact confirmation phrase, and a passing candidate QA status. It archives the
previous approved directory, records retrieval and approval times separately,
and writes rollback metadata.

## Required packages

The check step requires `httr2` and `jsonlite`. Candidate processing also
requires `sf` and `digest`. The optional sandbox map requires `leaflet`,
`htmlwidgets`, and `htmltools`. A self-contained sandbox file also requires
Pandoc; without Pandoc the builder writes a portable HTML file plus a sibling
`_files` dependency directory. The pipeline reports missing R packages and
does not install software automatically.

## Production boundary

Production UIC content is External-only under
**Energy / Minerals → Underground Injection Control (UIC)**. The ordinary
`build_final_map_only()` path does not resolve or read this pipeline's raw,
candidate, approved, map-ready, or label products. It also cannot build a real
BRIM application against a candidate.

Use `build_sandbox_map` for candidate or approved-product visual review. Its
labels use source capitalization, a minimum zoom of 8, and deduplicate only an
exact normalized label plus exact geometry hash. Source polygons are never
deduplicated, dissolved, or deleted.
