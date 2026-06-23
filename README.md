# microFGT

<img src="microFGT_logo.png" alt="microFGT Logo" width="150" align="right"/>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

A flexible, tunable, **one-stop** tool for **female genital tract (FGT) microbiome
analysis**. It runs the standard FGT tools (speciateIT, VALENCIA, VIRGO) or reimplements
their concepts, and lets method choices across the whole workflow be explored — vs. the
existing tools, which are rigid, hard to install, and frustrating to use.

> ⚠️ **This is an experimental branch (`design/fgt-workflow-map`): a ground-up Python
> rebuild of microFGT.** It is **not** the R package on `main`. The original R/S4 package
> (`R/`, `R-new/`, the `FGTExperiment` class, the `TESTING_*`/`INTEGRATION_*` docs) is being
> retired and replaced by the Python tool under [`microfgt/`](microfgt/). If you want the
> previous R package, switch to the `main` branch. The design reasoning behind the rebuild
> is in [`design/`](design/).

## What it is

The Python rebuild is built to two hard constraints: **user-friendliness** (one-command
install, a turnkey CLI, and the tool owns all the glue) and **scientific soundness**
(every method validated against a ground-truth reference before it's trusted —
compositional correctness, reproducible runs, honest sample reconciliation).

It models the 16S workflow as one stage graph
(`primer-trim → denoise → assign taxonomy → import → classify CST → integrate`), and the
**entry point is just whichever inputs your config provides**: paired FASTQs run the full
chain, an ASV table enters at taxonomy assignment, existing tool outputs enter at import.
The output is one `.h5mu` (MuData) holding sample-keyed assays plus CST and analysis
results.

## Install

```bash
pip install -e ".[dev]"          # from a clone, with test deps
```

or via conda (`conda env create -f environment.yml && conda activate microfgt && pip install -e .`).

## Quickstart

```bash
microfgt check -c config.yaml                  # preflight: are the needed tools/paths present?
microfgt run   -c config.yaml -o out.h5mu      # import → CST → analysis → one .h5mu
```

See [`example_config.yaml`](example_config.yaml) for a config runnable against the repo
fixtures.

## 📖 Full documentation

**[`microfgt/README.md`](microfgt/README.md) is the source of truth** — the CLI surface,
the Python API, the build status by phase, and (importantly) an honest account of what is
**validated against real tool output** vs. what is currently **wired and tested only
against stubs**. Read it before trusting a result.

## Contributing

Contributions are welcome — see the [Contributing Guidelines](CONTRIBUTING.md).

## License

MIT — see the [LICENSE](LICENSE) file.

## Citation

If you use microFGT in your research, please cite:

```
Handley, S. (2023-2025). microFGT: Comprehensive Analysis of Female Genital Tract
Microbiome Data. GitHub repository, https://github.com/shandley/microFGT
```
