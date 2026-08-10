# BRIM Codex thread-start contract

Use this template when opening a fresh Codex thread. Fill the task block; do not rewrite the durable rules.

## Required attachments/context

Provide:

- `AGENTS.md`
- `README.md`
- `BUILD.md`
- `DATA.md`
- `08_docs/BRIM_DEVELOPMENT_ARCHITECTURE.md`
- relevant registry/contract files
- relevant accepted checkpoint(s)
- canonical research package(s) for the task
- exact task prompt

Do not attach unrelated rollback folders or broad historical dumps unless the task specifically requires them.

For National Monuments work, also read
`08_docs/features/NATIONAL_MONUMENTS.md`,
`02_preprocess/70_national_monuments_pipeline/README.md`, the tracked
`local_reference_national_monuments_*` tables, and the focused phase-5 tests.
Use `acquire_authoritative_sources.R` as the canonical acquisition entry point.
The earlier Python acquisition script has been removed from tracked source; its
retained immutable snapshot is QA evidence only and must not become a
production dependency.
For Tule Lake, also require the focused R-only
`acquire_tule_lake_authoritative_source.R` addendum and its exact USFWS
`FWSSpecialDesignation` `OBJECTID=135`/GlobalID contract. The NPS `TULE`
feature is only the Segregation Center agency component; it must never be
treated as the complete semantic monument.
If the task includes National Park/Preserve context, use the separate targeted
`acquire_nps_park_preserve_context.R` →
`build_nps_park_preserve_context.R` →
`05_map_build/11_refresh_local_reference_nps_context_cache.r` path. Preserve
the legislative-boundary-versus-tract-interest distinction; context must not
enter the 20-monument semantic universe.

## Environment roles

- Source authoring: `BRIM_v0.38_source_repo`
- Isolated realistic build/testing: `BRIM_v0.38_codex_ship`
- Production: `BRIM_v0.38`

Source code/config/docs are authored only in the source repository. Production remains untouched until a merged, accepted release gate.

## Task block

```text
Task title:
Layer/system:
Repository path:
Expected branch/base SHA:
Research package and expected hash:
In scope:
Out of scope:
Required semantic keys:
Required UI/lifecycle contracts:
Expected processing/cache stage:
Stop point:
```

## Mandatory preflight

Before editing:

1. verify repository identity, branch, HEAD, upstream, and Git status;
2. read current code/registries/tests and the relevant checkpoint;
3. inventory authoritative local data and research-package inputs;
4. reconcile keys/counts/schema/geometry before implementing;
5. classify unknowns and stop conditions;
6. propose the exact changed-file/build/test manifest.

Do not modify files if the base state or source reconciliation is materially inconsistent with the task.

## Implementation rules

- Reuse the shared architecture; do not build a parallel controller/popup/filter/lifecycle system.
- Keep raw source, curated enrichment, management evidence, and display tokens separate.
- Do not treat research ZIP code or prebuilt HTML as production architecture.
- Do not infer authority, offices, or joins without evidence.
- Use semantic-feature counts in normal UI unless the layer contract explicitly says otherwise.
- Preserve accepted sibling layers and cache children.
- Do not run preprocessors until the `BUILD.md` preprocessor gate is satisfied.

## Validation and realistic build

Run focused source tests, sync an exact manifest to `codex_ship`, run only the required stage, prove sibling integrity, build a realistic HTML, run mounted browser tests, and report path/size/SHA plus manual review targets.

Stop for human visual acceptance before staging or committing.

For the current National Monuments work, the isolated realistic candidate is
`06_output/html/PortaTreasure2_core_20260810_105524.html` (205,006,831 bytes;
SHA-256 `09fb5b9567a90a88431f9b960de18becfdd4e7ca5639e1a3789144e94baf65a1`).
It has passed the focused source/cache/browser contract and remains stopped at
the release gate. Human visual acceptance passed on 2026-08-10; production
remains untouched until the authorized merge and protected deployment gates
complete.

## Git/release boundary

Do not stage, commit, push, open a PR, merge, delete branches, or modify production unless the user explicitly authorizes that gate.

## Required final report

- root cause/design summary;
- exact reconciliation and semantic/component counts;
- changed-source manifest;
- processing/cache effects;
- tests and results;
- realistic HTML path/size/SHA;
- unresolved issues;
- production status;
- Git status and stop point.
