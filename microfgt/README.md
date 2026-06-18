# microFGT

A flexible, tunable, **one-stop** tool for **female genital tract (FGT) microbiome
analysis**. It runs the standard FGT tools (speciateIT, VALENCIA, VIRGO) or
reimplements their concepts, and lets method choices across the whole workflow be
explored — vs. the existing tools, which are rigid, hard to install, and frustrating
to use.

This is the Python rebuild. The two hard constraints are **user-friendliness**
(one-command install, turnkey CLI, the tool owns the glue) and **scientific
soundness** (every method validated against a ground-truth reference first;
compositional correctness; reproducibility; honest sample reconciliation).

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

## Status (build is phased — walking skeleton first)

- **P0 — Skeleton + install** ✅ this package, one-command install, CI.
- **P1 — Integrated object + importers** ✅ MuData (`mudata`) + importers ported from
  the validated prototype and grounded in `prototype/real_fixtures/FORMATS.md`.
  - `import_virgo` — **real-output validated** (`virgo_sub*.out`).
  - `import_valencia` — **real-output validated** (`valencia_genuine_output_head.csv`).
  - `import_speciateit` — **NOT real-output validated.** The fixtures carry speciateIT
    *inputs* only (no genuine `MC_order7_results.txt`). Logic is ported from the tool's
    own `bin/count_table.py` + `FORMATS.md` and unit-tested against a fixture built from
    the genuine ASV assignments in the speciateIT README. **Real-output validation is an
    IOU for P3** (the orchestration wrapper runs speciateIT on `test.fasta`, then this
    importer is re-validated against the genuine output).
- **P2 — Centroid CST behind a `classify_cst` interface** ✅ Faithful VALENCIA port behind
  a swappable `classify_cst(composition, method=...)` seam. **Validated: 99.94% subCST
  agreement vs the paper's labels on all 13,231 published samples** (≥99.9% target), and
  exact reproduction of genuine `Valencia.py` output on the head fixture. The full gate
  (`tests/python/test_cst_validation_gate.py`) needs VALENCIA's ~8 MB published dataset —
  stage it with `python validation/fetch_valencia_published_data.py` (skips in CI otherwise).
- **P3 — Orchestration wrappers** ✅ `microfgt/orchestrate/`: `run_speciateit` (grounded in
  speciateIT's `classify -d/-i/-o` command) and `run_virgo`/`run_virgo_samples` (grounded in
  VIRGO's `runMapping.step1.sh`), each locating an installed tool (configurable path / PATH —
  microFGT ships no installers; the tools' reference data is too large to bundle), recording
  provenance (constraint B), and handing output to the matching importer. Validated end-to-end
  via stub executables that emit the documented genuine formats. **speciateIT real-output IOU:
  downgraded but still open** — `import_speciateit`'s handling of the real header is now
  exercised, but a true classifier run on `test.fasta` (needs the ~2.6 GB vSpeciateDB models)
  is the remaining discharge.
- **P4 — Analysis + viz + turnkey CLI** ✅ Analysis framework chosen deliberately:
  **scikit-bio for the commodity stats on a mudata-native container** (no framework lock-in).
  `microfgt/analysis/` buys compositional transforms (relabund, CLR), α/β diversity, PCoA
  ordination, and differential abundance (ANCOM) from scikit-bio — none reimplemented.
  Minimal matplotlib viz behind the `viz` extra. Config-first turnkey CLI:
  `microfgt run -c config.yaml` runs import → CST → analysis → one `.h5mu`; plus
  `microfgt classify` / `microfgt analyze` on an existing object. See `example_config.yaml`.
  Zero-count samples are handled honestly (excluded from compositional steps, recorded in
  `uns`, never silently dropped).
- **P5** — Alternative CST methods, diffed against the centroid baseline.

## Quickstart (Python API)

```python
from microfgt.io import import_virgo, import_valencia, import_speciateit, build_mudata

func = import_virgo("path/to/virgo_outputs/")          # dir of <sample>.out files
comp = import_speciateit("MC_order7_results.txt", "count_table.csv")
cst  = import_valencia("valencia_output.csv")           # sample-keyed CST/subCST/score

mdata = build_mudata(composition=comp, function=func, cst=cst)
```
