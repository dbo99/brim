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
