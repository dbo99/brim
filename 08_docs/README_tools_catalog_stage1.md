# PT2 Tools / Add Data catalog stage 1

This bundle adds the first staged catalog/dropdown version of the Tools / Add Data panel.

## Files included

```text
03_functions/leaflet_tools_adddata_helpers.r
00_config/external_service_catalog.csv
02_preprocess/20_build_external_service_catalog.r
```

## What changed

- Keeps manual URL entry.
- Keeps measurement tools.
- Keeps the built-in BLM CA Land Status / SMA overlay.
- Adds a starter embedded service catalog.
- Adds dropdowns for:
  - Agency / source
  - Theme
  - Layer
- Adds two catalog buttons:
  - Use selected: fills the manual fields
  - Add selected: fills the manual fields and immediately adds the layer
- Keeps the text box for manual URLs.
- Adds clearer popup wording:
  - FeatureServer and GeoJSON can have popups when supported.
  - MapServer layers are currently visual overlays only.
- Adds BLM AIM links to the source-links modal.
- Adds a cautious starter catalog with BLM AIM, DWR, USFWS, CDFW, FEMA, and BLM land-status entries.

## Rebuild

After copying the files into the project, run:

```r
source("run_build_map.r")
build_final_map_only()
```

No cache rebuild is needed.

## Optional catalog maintenance

The included script can regenerate the starter catalog:

```r
source("02_preprocess/20_build_external_service_catalog.r")
```

For this stage, the script writes a reviewed seed catalog. Future versions can add optional automated harvesters that write broad candidate service lists to `04_processed_data/qa/`, while keeping the map's approved catalog curated.

## Test checklist

1. Open the new HTML.
2. Confirm Notes and HUC fill still appear.
3. Open Tools / Add Data.
4. Confirm the catalog dropdown has rows.
5. Choose a catalog layer and click Use selected.
6. Confirm the name, URL, and service type fill into the manual fields.
7. Click Add layer.
8. Try a MapServer catalog row and confirm the popup checkbox is disabled / explained.
9. Try a FeatureServer catalog row and confirm popups can be enabled.
