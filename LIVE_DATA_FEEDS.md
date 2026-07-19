# BRIM live-data-feed integration

The live-feed generator is intentionally maintained as a separate public
repository:

`https://github.com/dbo99/brim-live-data-feeds`

The main BRIM source repository is private. The public feed repository
supports free scheduled runners and publication of non-sensitive feed
artifacts.

## Baseline integration choice

Do not embed the live-feed repository or its published data in this private
repository. Keep the repositories side by side for cross-repository work:

```text
Documents/
  BRIM_v0.38_source_repo/
  brim-live-data-feeds/
```

BRIM should depend on documented feed URLs, manifests, schemas, and fallback
behavior—not on an accidental nested working copy.

A Git submodule is intentionally deferred. A simple documented sibling repo
avoids duplicated history and initialization friction. If exact cross-repo
version pinning becomes necessary, record the tested feed commit SHA in this
document or a small interface-version file.

## Interface responsibilities

- Feed repo: acquire, normalize, publish, and monitor live artifacts.
- Main BRIM repo: fetch/read artifacts, render layers, manage UI and fallback.
- Sample feed fixtures may be added under `sample_data/`; production feed
  snapshots should remain outside this repository.
