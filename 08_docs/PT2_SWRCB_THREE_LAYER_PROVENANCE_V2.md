# PT2 SWRCB Three-Layer Provenance Symbology v2

Generated: 2026-05-13 18:14

## Files included

```text
03_functions/leaflet_layer_helpers.r
05_map_build/04_build_portatreasure2_core_map.r
05_map_build/02_cache_blocks/04_cache_admin_water_reference_layers.r
08_docs/PT2_SWRCB_THREE_LAYER_PROVENANCE_V2.md
```

## What changed from v1

The two SWRCB layers are replaced with three mutually exclusive layers:

```text
SWRCB 2026 BLM WR list records [1]
SWRCB additional PODs spatially matched to BLM [1]
SWRCB additional BLM name/text-match candidates [1]
```

## Layer logic

```text
SWRCB 2026 BLM WR list records:
  swrcb_2026_blm_wr_list == TRUE

SWRCB additional PODs spatially matched to BLM:
  swrcb_2026_blm_wr_list == FALSE
  AND blm_include_reason_display is Spatial + name match or Spatial only

SWRCB additional BLM name/text-match candidates:
  swrcb_2026_blm_wr_list == FALSE
  AND blm_include_reason_display is Name match only
```

## Symbology

```text
Point radius = face value / AFY
Fill color   = screening source / provenance
Ring color   = WR status
```

Fill colors:

```text
Blue   = SWRCB 2026 BLM WR list
Teal   = additional PT2 spatial POD match
Purple = additional PT2 BLM name/text candidate
```

Ring colors:

```text
Bright green = active / recognized
Orange       = pending
Red          = inactive / cancelled
Gray         = unknown
```

The additional name/text candidate layer is hidden by default.

## Popup changes

The popup now reports:

```text
Screening source
BLM match detail
Spatial BLM match
Interpretation
SWRCB 2026 BLM WR list
POD feature ID
POD/WR-list ID
```

## Notes button

Note [1] was updated to explain the three layers and their symbology.

## Rebuild sequence

Because the cache block and map build script changed, rebuild the cache first:

```r
source("05_map_build/02_build_core_map_cache.r")
source("run_build_map.r")
build_final_map_only()
```
