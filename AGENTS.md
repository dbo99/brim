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

## Documentation maintenance

Every source-changing modernization or cleanup batch must assess whether its durable change makes existing authoritative BRIM documentation inaccurate or materially incomplete. Report:

```text
DOCUMENTATION_IMPACT: YES | NONE | UNRESOLVED
AUTHORITATIVE_DOCS_AFFECTED: <exact existing paths or NONE>
DOCUMENTATION_ACTION: UPDATED_IN_BATCH | NONE_REQUIRED | BLOCKED
REASON: <short durable-fact explanation>
```

- Prefer updating existing authoritative documents over creating new planning documents.
- When a change establishes or retires a durable architectural fact, update the relevant existing documentation in the same implementation sequence or report `DOCUMENTATION_IMPACT: NONE` with a defensible reason.
- Treat `DOCUMENTATION_IMPACT: UNRESOLVED` as a stop-before-commit condition.
- Describe current authority, not planned future behavior.
- Do not update durable documentation merely to narrate branches, worktrees, audit identifiers, temporary implementation process, or transient test/debug history.
- Modify `SOURCE_MANIFEST.csv` only when its actual repository contract requires it; it is not a generic documentation checklist.
- Handle documentation impact incrementally within normal modernization and cleanup batches rather than accumulating a final documentation phase.

## Bounded autonomy

After the maintainer approves the objective, baseline, maximum scope, and relevant protected boundaries for a non-production cleanup or modernization batch, Codex may autonomously complete the normal development-to-PR sequence:

1. verify repository, `main`, worktree authority, and collisions;
2. inspect exact current source and implement only the approved scope;
3. run the approved validation and assess documentation impact;
4. update affected durable documentation in the same batch;
5. stage and create exactly one validated commit;
6. push the exact feature branch normally and open exactly one non-draft pull request;
7. stop before merge.

Separate human approvals are not required for staging, that single validated commit, the normal branch push, or pull-request creation.

Merge remains an explicit human-review checkpoint. After explicit merge approval, Codex may perform fresh repository, pull-request, and control checks; merge using the approved normal method; preserve the branch and worktree unless cleanup was separately authorized; refresh authoritative local `main` to the exact merged authority; run the narrow required post-merge authority/integration checks; and stop before deployment or production synchronization. The local-`main` refresh and narrow post-merge verification do not require separate approval.

Bounded autonomy does not permit improvisation. Stop for maintainer review on:

- scope expansion or an unexpected changed path;
- ambiguous or contradictory runtime authority;
- failed validation requiring source changes beyond approved scope;
- a new dependency or material dependency change;
- preprocessor execution or modification;
- generated or protected-cache regeneration or mutation;
- an unresolved `SOURCE_MANIFEST.csv` contract or `DOCUMENTATION_IMPACT: UNRESOLVED`;
- a BRIM Live machine-facing contract, path, schema, or freshness change outside explicitly approved scope;
- force-push, rebase/history rewriting, or administrator bypass;
- repository controls that do not permit the intended merge;
- deployment, production synchronization, or production access not explicitly authorized.

Production remains explicitly approval-gated.

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

- Do not stage, commit, push, open a PR, merge, delete a branch, or modify production unless the current task explicitly authorizes it. An approved bounded-autonomy batch supplies that authorization only for its stated development-to-PR sequence; merge, branch/worktree cleanup, deployment, and production remain separate gates.
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
