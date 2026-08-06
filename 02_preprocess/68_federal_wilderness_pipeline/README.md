# Federal Wilderness focused pipeline

This directory owns the fail-closed refresh path for BRIM’s accepted Federal
Wilderness dataset: 197 mapped source components representing 158 named
wildernesses.

The builder requires the checksummed official source ZIP plus the five tracked
configuration/enrichment tables. It preserves all original source attributes,
repairs only a derived geometry copy, applies the simplification hook before
enrichment, and requires the accepted `simplify_keep = 0.5`. Its output is a new
candidate RDS and QA files; it never replaces the current processed RDS.

Promotion is a separate explicit call. It first makes a hash-verified archive
of the prior processed RDS, then copies the exact reviewed candidate into the
final processed path and verifies the copy hash. The focused cache refresh is
separate again and may change only the `fedwilderness` reference child and its
matching label child.

This pipeline performs no network requests. The handoff ZIP is an approved
input/evidence package and is not copied into Git. Generated RDS, GPKG, cache,
QA, HTML, screenshots, and logs remain outside normal source control.
