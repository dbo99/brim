# BRIM agent rules

## Authority and environments

BRIM is a self-contained Leaflet application built primarily in R.

The tracked source repository on merged `main` is the sole authored source of code, configuration, tests, and controlling documentation.

The three working trees have different roles:

- `BRIM_v0.38_source_repo` — lean Git source authority; author changes here.
- `BRIM_v0.38_codex_ship` — isolated build-capable integration workspace; sync exact source changes here for preprocessing, cache, browser, and realistic-build testing.
- `BRIM_v0.38` — full production workspace; modify only during an approved release after merge and backup.

Do not treat these trees as interchangeable. Do not edit authored source independently in `codex_ship` or production. Do not copy `.git` or broad directories between them.

When documentation conflicts, current merged code, registries, schemas, and focused tests outrank historical handoffs, checkpoints, audit outputs, research packages, rollback folders, and manual-patch copies.

## Before editing

1. Read `README.md`, `BUILD.md`, `DATA.md`, and the relevant architecture/feature document.
2. Verify repository root, branch, committed HEAD, upstream, and complete Git status.
3. Stop if unrelated changes are present unless the user explicitly authorizes working around them.
4. Inspect current code and tests before relying on historical notes.
5. State the exact task boundary and out-of-scope systems.

## Scope and implementation

Keep changes coherent and as small as practicable.

- Extend an existing registry, helper, controller, lifecycle, build step, or QA path when it already owns the responsibility.
- Avoid parallel implementations, duplicated constants, hidden fallbacks, speculative abstractions, and feature-specific forks of shared infrastructure.
- Preserve raw source attributes, stable identifiers, scientific meaning, schemas, and accepted behavior unless the task explicitly changes them.
- Do not invent missing data, silently skip required records, infer management authority from publication/intersection alone, or accept fuzzy joins without a reviewed crosswalk.
- Keep cleanup proportional to the task; record broad refactors separately.
- Remove temporary diagnostics and superseded code introduced by the current change before checkpointing.

## Production safety

Production is fail-closed.

- Do not edit, overwrite, delete, preprocess, rebuild, or sync production during implementation or realistic testing.
- Do not deploy before the change is committed, pushed, reviewed, merged to `main`, backed up, and accepted in an isolated realistic build.
- Production release must use an explicit file/artifact manifest, rollback folder, and post-copy hash verification.
- Deploy the exact visually accepted HTML/cache artifact when that artifact is the release candidate; do not substitute an unreviewed rebuild.

## Preprocessor safety

Do not run a preprocessor unless all of the following are identified first:

- exact script/function and dependency reason;
- required input files/services and their authority;
- expected outputs and locations;
- remote-service/network behavior;
- files/products that may be overwritten;
- sibling artifacts that must remain unchanged;
- validation tests and expected counts/hashes;
- recovery or rollback method.

Never run preprocessors generically, speculatively, by directory loop, or merely because they exist. Never run a broad convenience orchestration when a focused stage is sufficient.

Prefer, in order:

1. source-only parse/static tests;
2. focused unit/fixture tests;
3. final-map-only build with valid caches;
4. focused cache refresh for one owned child;
5. broader cache rebuild only with an explicit dependency case;
6. focused preprocessor only after the preprocessor gate above passes.

## Data and generated artifacts

- Large raw data, RDS/GPKG products, caches, realistic/final HTML, screenshots, logs, and sandbox outputs remain outside normal Git unless an established tracked contract explicitly says otherwise.
- Research ZIPs and workbooks are evidence/input, not production code or geometry authority.
- Keep semantic identity separate from geometry/audit identity.
- Preserve source values separately from curated display values and record evidence/confidence for overrides.
- Never commit secrets, machine-specific paths, temporary browser output, or generated production artifacts.

## Git and review

- Do not stage, commit, push, rebase, merge, open a PR, or delete a branch unless the user authorizes that gate.
- Use coherent checkpoint commits; do not mix unrelated documentation, feature, or cleanup work.
- Never force-push unless explicitly authorized for a known recovery case.
- Human visual acceptance is required for user-facing map changes before checkpoint/release.

## Validation

Run the narrowest complete test set for the changed responsibility.

At minimum:

- parse changed R files and validate JavaScript in its intended context;
- test exact identity/join/count contracts;
- test layer on/off and idempotent teardown;
- test Clear Local/Clear All and relevant reset actions;
- test hover, popup, legend/filter, chips/search, zoom, accessibility, narrow viewport, and browser console behavior when affected;
- prove unrelated cache children/layers remain unchanged;
- run `git diff --check`, secret scans, and newly added machine-path scans;
- report what was tested, what was not, and all remaining uncertainty.

Stop rather than stacking speculative fixes on an unproven root cause.
