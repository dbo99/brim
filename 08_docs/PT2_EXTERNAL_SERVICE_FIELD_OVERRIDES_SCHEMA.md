# external_service_field_overrides.csv schema

Rows in this file override generated field-curation suggestions.

Matching order used by the script:

1. service_url if provided
2. display_name if service_url is blank

Only nonblank override cells are applied. Blank cells leave generated values unchanged.

Important columns:

- display_name: easiest matching key for hand curation
- service_url: stronger matching key when available
- popup_fields: semicolon-separated popup fields
- hover_fields: semicolon-separated hover/tooltip fields
- out_fields: fields to request from FeatureServer current-view queries
- style_field_candidates: numeric fields that may be useful for future styling
- default_style_field: preferred first style field
- field_review_status: tracks curation state
- approved_for_catalog: leave blank until you are ready to promote rows
