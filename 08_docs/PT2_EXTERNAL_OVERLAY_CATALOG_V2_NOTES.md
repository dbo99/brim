# PT2 External Overlay Catalog v2 Notes

This catalog is intended to be a strong starting point for the Tools / External Overlays panel.

## Review strategy

The catalog deliberately favors MapServer rows for large, pre-symbolized public services
when visual context is the main use case. FeatureServer rows are most useful where
attribute inspection, popups, or future feature-level styling are important.

## Performance guidance

Large national or statewide FeatureServer layers are set to current-view loading and
a zoom threshold. This avoids trying to draw large feature collections at small scales.

## Next catalog-building session

Recommended next steps:

1. Test DWR / CNRA rows first, especially the groundwater and flood-management services.
2. Identify DWR services that should be MapServer-only versus FeatureServer inspectable.
3. Add more SWRCB/Water Boards rows from the public REST directories.
4. Expand BLM GBP rows beyond AIM and SMA.
5. Split large multi-layer MapServer services into useful sublayer rows where the legend
   or layer list indicates a high-value sublayer.
6. Add a true candidate-harvester workflow that writes broad candidates to QA first,
   then manually promotes approved rows into this catalog.
