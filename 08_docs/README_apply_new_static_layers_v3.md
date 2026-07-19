# PortaTreasure2 replacement scripts v3

This bundle updates the v2 new-static-layer work with a focused cleanup pass.

## Fixes included

- Keeps the main layer-control title fully visible and prevents rows from showing through under the sticky `Layers` ribbon.
- Changes the layer-control display from `Deltamapr Canals` to `Deltamapr Conveyance`.
- Expands Deltamapr conveyance operator classification so Bureau/Reclamation/USBR text is styled as CVP.
- Keeps true operator text in Deltamapr hover/popups while using simplified operator groups only for symbology.
- Lowers Water District fill opacity and strengthens outlines/hover highlighting so overlapping polygons are easier to interpret.
- Removes redundant ` km` from always-visible X2 text labels while keeping `km` in hover/popups.
- Hides X2 text labels until zoom level 10+ to reduce regional-scale clutter.

## Suggested run

Because some style/group fields are cached, rebuild the core cache and map after copying these files:

```r
source("run_build_map.r")
rebuild_core_cache_and_map()
```

If the new static source RDS files have not yet been created, run instead:

```r
source("run_build_map.r")
refresh_new_static_water_layers_and_map()
```
