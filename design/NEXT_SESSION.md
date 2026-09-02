# Next session — pick up here (as of 2026-09-02)

**Current focus: make the 16S arm *installable* (the "usable" thrust).** The methods are in decent
shape; the tool is still hand-crank-only, and easy install is the whole point of the project.

## Where we are
- The 16S arm **runs end-to-end** on real tools/reads (proven on macOS today; plumbing-level only —
  a non-vaginal mock, since real vaginal test data is a separate problem, see "parked").
- **Committed + pushed today** to `design/fgt-workflow-map`:
  - CST centroid fix — default → **2024 VALENCIA centroids** (match speciateIT v6 naming; the 2020
    set silently dropped core BV taxa and degraded CST IV calls)
  - DADA2 single-sample crash fix
  - Tool errors now **surface cleanly** (no more hidden R/tool failures behind a Python traceback)
  - Region names fixed (`V4V4`/`V1V9` recognized; `V4` kept as an alias)
- Detailed maps: **`design/install_runbook_16s.md`** (the verified manual install) and
  **`design/method_log.md`** (method findings M1–M4).

## The plan (agreed)
Deliberate split:
- **Environment = the user's responsibility** — laptop vs shared cluster differ (module systems,
  HPC policies, can't always make conda envs). We *help* but don't manage it.
- **`microfgt setup` = owns the FGT-specific tools + databases** — the pain that's universal.

**Next steps, in order:**
1. **Ship `environment-16s.yml`** — a lean 16S env (`python + pip + cutadapt + bioconductor-dada2`),
   split out from the bundled `environment.yml`, which does NOT solve on Apple Silicon
   (VISTA's `r-randomforestsrc` has no arm64 build and poisons the whole env). Convenience/guidance,
   NOT something `setup` runs.
2. **Build `microfgt setup --region <R> --dest <dir>`:**
   - Download the matching vSpeciateDB models from figshare (pinned version + sha256 verify).
   - Fetch the speciateIT binary (clone `ravel-lab/speciateIT`; binary ships in `bin/`).
   - Print (or write) the config lines pointing at them, then run `microfgt check`.
   - `--dest` can be any location (supports a shared `/ref` dir on a cluster).

## Reference facts already nailed down (don't re-derive — see runbook for detail)
- **vSpeciateDB**: figshare DOI `10.6084/m9.figshare.25254229` (currently v6), **CC BY 4.0** (so we
  may mirror). Region zips: V1V3 142 MB · V1V9 321 MB · V3V4 123 MB (sha256 `0f6c0e19…`) ·
  V4V4 89 MB (sha256 `e38171df…`). Dirs: `vSpeciateIT_{V1V3,V1V9,V3V4,V4V4}`.
- speciateIT binary is **native arm64** on macOS; runs `classify -d <db> -i <fasta> -o <out>`.
- **Gotcha:** the speciateIT binary and conda scripts break on paths containing **spaces** (this
  machine's home is `/Users/Megan Johnson/`). Use a space-free workdir/env prefix locally; most
  users/clusters are unaffected.
- Local assets live OUTSIDE the repo: `~/Projects/microfgt-refdata/` (speciateIT clone + V1V3/V3V4/
  V4V4 models); lean env at `/Users/Shared/microfgt-16s`; a working config at
  `~/Projects/microfgt-refdata/mock_run/config.yaml`.

## Parked (NOT this focus)
- **Trustworthy thrust** — real vaginal data *with* quality scores for a meaningful run. PIN
  (PRJNA876771) is vaginal + open but its SRA reads are **quality-stripped** (constant Q30) →
  unusable for DADA2. Need a different source. Plus the M4 follow-ups (spot-check our port vs
  official `Valencia.py`-2024; a 2024-named gold-standard cohort to re-earn the 99.9% gate).
- genus parsing (M2) — decorative, deprioritized.
- DADA2 binned-quality handling (M1) / clear-error on quality-less input (M3).
- **Shotgun arm** — entirely untouched; VISTA's env won't build on arm64 (the `r-randomforestsrc`
  problem). A whole separate effort.
