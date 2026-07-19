# PT2 Tools / Add Data Panel v1 fix 1

This focused fix replaces only:

```text
03_functions/leaflet_tools_adddata_helpers.r
```

## Issue fixed

The first version used `sprintf()` to inject one boolean value into a large JavaScript string. The JavaScript/CSS also contained literal percent signs such as `width: 100%;`. In R, `sprintf()` treats `%` as a formatting marker unless it is escaped as `%%`, so the build failed before the HUC theme block started.

## Change made

Literal CSS percent signs inside the `sprintf()` JavaScript string were escaped:

```text
100%  ->  100%%
```

The intended `sprintf()` placeholder for `PT2_ENABLE_BLM_SMA = %s` is unchanged.

## Rebuild

After copying the replacement script into the project, run:

```r
source("run_build_map.r")
build_final_map_only()
```

No cache rebuild is needed.
