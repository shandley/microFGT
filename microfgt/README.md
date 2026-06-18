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
- **P2** — Centroid CST behind a `classify_cst` interface (validate 99.9% vs VALENCIA).
- **P3** — Orchestration wrappers (actually run the tools).
- **P4** — Analysis + viz + turnkey CLI; pick the analysis framework here.
- **P5** — Alternative CST methods, diffed against the centroid baseline.

## Quickstart (Python API)

```python
from microfgt.io import import_virgo, import_valencia, import_speciateit, build_mudata

func = import_virgo("path/to/virgo_outputs/")          # dir of <sample>.out files
comp = import_speciateit("MC_order7_results.txt", "count_table.csv")
cst  = import_valencia("valencia_output.csv")           # sample-keyed CST/subCST/score

mdata = build_mudata(composition=comp, function=func, cst=cst)
```
