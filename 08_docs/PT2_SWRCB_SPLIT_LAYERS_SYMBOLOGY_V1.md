# PT2 SWRCB Split Layers + Symbology v1

Generated: 2026-05-13 17:05

## Files included

```text
03_functions/leaflet_layer_helpers.r
05_map_build/04_build_portatreasure2_core_map.r
05_map_build/02_cache_blocks/04_cache_admin_water_reference_layers.r
08_docs/PT2_SWRCB_SPLIT_LAYERS_SYMBOLOGY_V1.md
```

## What this patch does

The single layer:

```text
SWRCB PODs/WRs relevant to BLM [1]
```

is split into two layer-control checkboxes under the existing Points section:

```text
SWRCB PODs spatially matched to BLM [1]
SWRCB BLM-associated WR/POD records [1]
```

The split uses `blm_include_reason_display`:

```text
Spatial layer:
  Spatial + name match
  Spatial only

BLM-associated WR/POD records layer:
  Name match only
```

## Symbology

```text
Point radius = face value / AFY, using cached swrcb_radius
Fill color   = BLM relevance mechanism / layer
Ring color   = WR status
```

Fill colors:

```text
spatial match = blue/teal
name-only WR/POD record = muted purple
```

Ring colors:

```text
Active / recognized  = bright green
Pending              = orange
Inactive / cancelled = red
Unknown              = gray
```

The name-only layer is added but hidden by default with `leaflet::hideGroup()`.
Users can turn it on from the Points section.

## Popup clarification

The cache block now adds clearer popup language:

```text
POD feature ID
POD/WR-list ID
BLM Relevance
Spatial BLM match
Interpretation
```

This is meant to avoid implying that WR/name/list-only points are physically on
BLM land.

## Important note

The Notes button text was intentionally not fully revised in this patch. After
this builds and looks right, update note [1] so it matches the new two-layer
symbology.
