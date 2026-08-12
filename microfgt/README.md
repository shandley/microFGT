# microFGT

A flexible, tunable, **one-stop** tool for **female genital tract (FGT) microbiome
analysis**. It runs the standard FGT tools (speciateIT, VALENCIA, VIRGO) or
reimplements their concepts, and lets method choices across the whole workflow be
explored — vs. the existing tools, which are rigid, hard to install, and frustrating
to use.

This is the Python rebuild. Two hard constraints drive every decision: **user-friendliness**
(one-command install, a turnkey CLI, and the tool owns all the glue — joins, reshaping,
annotation — so the user never scripts it) and **scientific soundness** (every method is
validated against a ground-truth reference before it's trusted; compositional correctness;
reproducible, provenance-recorded runs; honest sample reconciliation across assays).

## Install

```bash
pip install -e ".[dev]"     # from a clone, with test deps
```

or with conda:

```bash
conda env create -f environment.yml
conda activate microfgt
pip install -e .
```

The package itself is pure Python and pip-installs in one command (the UX-first
constraint, exercised in CI on 3.10 + 3.12). The heavy external tools and their
reference data are **not** bundled — see [External tools & reference data](#external-tools--reference-data).

## What it does today

microFGT models the 16S workflow as a single stage graph —
`primer-trim → denoise (DADA2) → assign (speciateIT) → import → classify CST → integrate`
— defined in exactly one place (`microfgt/stages/registry.py`) and consumed by two
executors (a local runner and a Snakemake/Slurm generator), so the laptop and cluster
pipelines can never drift apart.

You don't pick a stage to start at. **The entry point is just whichever inputs your
config provides** — microFGT resolves the shortest path to a finished object:

| You have… | Enters at | Runs |
|---|---|---|
| Paired FASTQs (`fastq_dir`) | the top | cutadapt → DADA2 → speciateIT → CST → analysis |
| An ASV table + rep-seqs | `assign` | speciateIT → CST → analysis |
| Existing speciateIT / VALENCIA output | `import` | CST → analysis |

The output is one `.h5mu` (MuData) holding sample-keyed assays — `composition`
(ASV×sample; the source of truth, carrying each ASV's sequence), its taxon roll-up
`composition_taxon` (taxon×sample), and optionally `function` (gene×sample) — with the CST
label, its **augment descriptors** (dominant taxon, % dominant, # taxa >10%), and analysis
results attached as sample-level annotations. CST stays one method (VALENCIA); the
descriptors surface the community structure a single class flattens, without competing it.

### Command line (turnkey, config-first)

```bash
microfgt check   -c config.yaml                 # preflight: are the tools/paths this entry point needs present?
microfgt run     -c config.yaml -o out.h5mu     # import → CST → analysis → one .h5mu
microfgt run     -c config.yaml --executor snakemake   # emit a Snakefile to submit on a cluster instead
microfgt classify -i out.h5mu -o out.h5mu -m centroid  # (re)classify CST on an existing object
microfgt analyze  -i out.h5mu -o out.h5mu --transform clr --alpha shannon
```

`microfgt check` is a preflight doctor — it inspects the *resolved* entry point and
reports missing binaries, R packages, DB paths, and region↔DB mismatches up front,
with actionable messages, instead of failing deep in a run. A runnable example config
(against the repo fixtures) is at `example_config.yaml`.

### Python API (for power users)

```python
from microfgt.io import import_virgo, import_valencia, import_speciateit, build_mudata

func = import_virgo("path/to/virgo_outputs/")          # dir of <sample>.out files
comp = import_speciateit("MC_order7_results.txt", "count_table.csv", fasta="asvs.fasta")
cst  = import_valencia("valencia_output.csv")          # sample-keyed CST/subCST/score

mdata = build_mudata(composition=comp, function=func, cst=cst)
# build_mudata materialises the taxon roll-up (composition_taxon) from the ASV-grain
# composition and attaches the augment descriptors automatically.
```

`import_speciateit` returns an **ASV×sample** composition (each ASV keeps its
classification, genus, and — with `fasta=` — its sequence); `collapse_to_taxon(comp)` is the
taxon roll-up CST reads.

`from microfgt.cst import classify_cst` and the `microfgt.analysis` module (compositional
transforms, α/β diversity, PCoA, ANCOM differential abundance — all bought from
scikit-bio, none reimplemented) operate on the same objects.

## What's validated vs. wired

This is the honest state, because it's what determines whether a result can be trusted.
**Stubs prove the plumbing; they do not prove the biology.** Anything marked ⚠️ has had
its wiring exercised end-to-end but has never run against the real tool's output.

| Piece | Status |
|---|---|
| **Centroid CST → VALENCIA** | ✅ **Validated against ground truth** — 99.94% subCST agreement on all 13,231 published samples (≥99.9% target), plus exact reproduction of genuine `Valencia.py` output on the head fixture. |
| `import_virgo` | ✅ Real-output validated (`virgo_sub*.out`). |
| `import_valencia` | ✅ Real-output validated (`valencia_genuine_output_head.csv`). |
| `import_speciateit` | ⚠️ Unit-tested against a fixture built from the tool's README, **not** a genuine `MC_order7_results.txt`. Logic ported from speciateIT's `count_table.py` + `FORMATS.md`. |
| Orchestration wrappers (speciateIT, VIRGO) | ⚠️ Validated via **stub executables** that emit the documented formats — wiring only. |
| Preprocessing ladder (cutadapt, DADA2) | ⚠️ Validated via **stubs** end-to-end (FASTQs → `.h5mu`) — plumbing, not real denoising. |
| Snakemake/Slurm executor | ⚠️ Snakefile is generated and asserted, but **never yet run on a cluster**. |

Run the suite (55 tests; the CST gate needs VALENCIA's ~8 MB published dataset — stage it
with `python validation/fetch_valencia_published_data.py`, else it skips):

```bash
pytest -q
```

### Open validation IOUs

Two gaps remain, both requiring a real run rather than more code:

1. **speciateIT real-output** — `import_speciateit` has never parsed genuine classifier
   output. Discharged by one real `MC_order7_results.txt` (the importer check needs only
   the small output file; *producing* it needs the ~2.6 GB models).
2. **HTCF ladder run** — the orchestration + cluster path proven only with stubs.
   Discharged by one real run on WashU's HTCF (real cutadapt + DADA2 + speciateIT on a
   small 16S set), which also discharges #1.

## External tools & reference data

microFGT ships **no installers and no reference data** (the tools' DBs are too large to
bundle); the orchestration layer locates an installed tool by configured path or PATH.
You only need the tools your **entry point** uses — run `microfgt check -c config.yaml` at
any time to see exactly what's still missing for *your* config, with install hints.

What each step requires, at a glance:

| Step | Tool | Reference data |
|---|---|---|
| Primer trim | cutadapt | none (primers are config strings) |
| Denoise | R + Bioconductor `dada2` | **none** — denoise-only; taxonomy is speciateIT's job |
| Taxonomy | speciateIT (`classify`) | **vSpeciateDB models, ~2.6 GB** (separate download) |
| Function (upcoming) | VIRGO | large gene catalog (separate download) |

So the entire 16S **front-end** (real cutadapt + real DADA2 on real reads) runs with only
small installs and **no multi-GB download** — the models are needed solely for the
taxonomy-assignment step. Per-tool setup follows.

### cutadapt + DADA2 (16S front-end — only if you start from raw FASTQs)

Both are on bioconda and come with `environment.yml`:

```bash
conda env create -f environment.yml && conda activate microfgt && pip install -e .
```

Neither needs a reference database. (If you enter at an existing ASV table or existing tool
outputs, you don't need these at all.)

### speciateIT (taxonomy — turns ASVs into the `composition` matrix)

A C++ 16S classifier from the Ravel Lab: <https://github.com/ravel-lab/speciateIT>.

1. **Binary.** A precompiled `classify` ships in the repo under `bin/linux` / `bin/macosx`
   — clone the repo and put that directory on your `PATH` (no build needed in the common
   case). <!-- TODO: verify build-from-source steps + whether a bioconda package or
   container exists; web access was unavailable when this was written. -->
2. **Models (~2.6 GB, separate download).** The `vSpeciateDB` reference models are *not*
   bundled with the binary. They come as region-specific directories named
   `vSpeciateIT_<REGION>` for **V1V3**, **V3V4**, or **V4** — download the one matching your
   amplicon. <!-- TODO: confirm the exact figshare/Zenodo URL + the distributed directory
   names before relying on this; not verifiable when written. -->
3. **Point microFGT at it** in your config:

   ```yaml
   composition:
     speciateit:
       classify: classify                 # binary name on PATH, or an explicit path
       db: /path/to/vSpeciateIT_V3V4       # the downloaded model directory
   ```

4. **Verify:** `microfgt check -c config.yaml` confirms the binary resolves, the `db` path
   exists, and the region matches the model directory before you run.

speciateIT runs as `classify -d <db> -i <asvs.fasta> -o <outdir>`, always writing
`MC_order7_results.txt`; microFGT then owns the join to the ASV count table.

### VIRGO (function — metagenomic gene profiling)

The vaginal gene catalog + read-mapping pipeline from the Ravel Lab.
<!-- TODO: confirm canonical repo URL, the catalog download location + size, runtime deps
(bowtie/perl/python), and bioconda/container status; web access was unavailable when this
was written. -->

- **Install** unpacks to a directory containing `0_db/`, `1_VIRGO/`, and `3_run_VIRGO/`
  (the gene catalog is a large download — size TODO).
- VIRGO maps **single-end** reads — merge each sample's reads into one FASTQ first.
- microFGT runs `bash 3_run_VIRGO/runMapping.step1.sh -r <reads> -p <sample> -d <virgo_path>`
  per sample, then stacks the per-sample `.out` files into the `function` modality. Point it
  at the install with:

  ```yaml
  function:
    virgo:
      dir: /path/to/VIRGO
  ```

## Build status (phased — walking skeleton first)

- **P0 — Skeleton + install** ✅ one-command install, CI.
- **P1 — Integrated object + importers** ✅ MuData + importers grounded in
  `prototype/real_fixtures/FORMATS.md` (see the validation table above).
- **P2 — Centroid CST behind `classify_cst`** ✅ faithful VALENCIA port behind a swappable
  `classify_cst(composition, method=...)` seam; validated 99.94% vs the paper.
- **P3 — Orchestration wrappers** ✅ `run_speciateit` / `run_virgo`, each locating an
  installed tool, recording provenance, and handing output to the matching importer.
- **P3.5 — Preprocessing front-end + multi-entry workflow** ✅ one stage registry, two
  executors (local resolver + Snakefile generator), the multi-entry ladder, and
  `microfgt check`. DADA2 orchestrated with region-aware, overridable defaults.
- **P4 — Analysis + viz + turnkey CLI** ✅ commodity stats bought from scikit-bio
  (transforms, α/β diversity, PCoA, ANCOM); minimal matplotlib viz behind the `viz`
  extra; the config-first CLI. Zero-count samples handled honestly (excluded from
  compositional steps, recorded in `uns`, never silently dropped).
- **Augment descriptors (not rival CST)** ✅ CST is one blessed method (VALENCIA); the
  diffuse/continuum structure it flattens is read out by *augmenting* the label with
  per-sample descriptors — dominant taxon, % dominant, # taxa >10% — attached alongside CST.
  (This replaces the earlier "alternative CST methods" framing: the object carries ASVs and
  their sequences as the source of truth, with a materialised `composition_taxon` roll-up.)
