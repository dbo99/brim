# PT2 External Overlay Catalog DWR URL Repair

Generated: 2026-05-10 05:55

## Why this patch exists

The uploaded field-curation QA sheet shows that many DWR rows are being tested
against the same service URL:

```text
https://utility.arcgis.com/usrsvcs/servers/15e2bef61bdb4cabb2719c3147211049/rest/services/Geoscientific/i08_GroundwaterDepthSeasonal_Contours/MapServer/0
```

But those rows have different `source_page` / intended REST service URLs. That means
the metadata pass is technically working, but it is reading metadata from the wrong
service for many DWR rows.

## What to do

Copy this script into your project:

```text
02_preprocess/23_repair_pt2_external_overlay_catalog_bad_urls.r
```

Run it first as a dry run:

```r
source("02_preprocess/23_repair_pt2_external_overlay_catalog_bad_urls.r")
```

If the printed repair list looks right, open the script and change:

```r
DRY_RUN <- TRUE
```

to:

```r
DRY_RUN <- FALSE
```

Then rerun it.

After that, rerun the field-curation QA sheet:

```r
source("02_preprocess/21_build_pt2_external_overlay_field_curation_qa_sheet.r")
```

## Included preview

This bundle also includes a repaired preview CSV generated from the uploaded QA sheet:

```text
04_processed_data/qa/pt2_external_overlay_catalog_repaired_from_uploaded_qa_preview.csv
```

Use the script on your local project catalog rather than manually copying the preview,
unless you specifically want to inspect the repair logic.
