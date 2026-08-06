# BRIM backup and baseline packaging

Do not work directly inside a Google Drive synchronized folder. Build and
validate archives locally first, then copy the finished archives to Drive.

Recommended dated baseline folder:

```text
BRIM_v0.38_baseline_YYYYMMDD/
  00_README_BASELINE.txt
  BRIM_v0.38_full_YYYYMMDD.zip
  BRIM_v0.38_codex_ship_YYYYMMDD.zip
  BRIM_v0.38_source_repo_YYYYMMDD.zip
  BRIM_v0.38_audit_records_YYYYMMDD.zip
  SHA256SUMS.txt
```

Use multiple independent ZIPs rather than one nested mega-archive. This
allows selective restoration and limits the damage from a single corrupt
transfer.

Every archive should be tested before upload and accompanied by a SHA-256
checksum. After upload, compare the Drive-downloaded checksum for at least
the full and source-repository archives.
