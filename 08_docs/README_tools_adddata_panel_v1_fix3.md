# PT2 Tools / Add Data Panel v1 fix 3

This focused fix replaces only:

```text
03_functions/leaflet_tools_adddata_helpers.r
```

## Issue fixed

The v1/fix2 helper built successfully in R, but the browser-side JavaScript failed
to parse before the Tools panel, Notes button, and HUC fill dropdown could initialize.

The specific bug was in `ptNormalizeUrl()`:

```js
.replace(/\\/+$/, '')
```

That is not valid JavaScript regex syntax for trimming trailing forward slashes.

## Change made

It is corrected to:

```js
.replace(/\/+$/, '')
```

which appears in the final browser JavaScript as:

```js
.replace(/\/+$, '')
```

The intent is simply to trim trailing `/` characters from pasted service URLs.

## Rebuild

After copying the replacement script into the project, run:

```r
source("run_build_map.r")
build_final_map_only()
```

No cache rebuild is needed.

## Test

After opening the new HTML:

1. Confirm the Tools / Add Data tab appears on the left.
2. Confirm Notes button appears.
3. Confirm HUC fill dropdown appears.
4. Check browser console for red JavaScript errors.
