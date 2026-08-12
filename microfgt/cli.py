"""Turnkey CLI — runs the whole workflow, with the Python API underneath for power users.

Config-first (constraint A). The entry point is just which inputs the config provides:
FASTQs run the full ladder (primer-trim -> DADA2 -> speciateIT -> CST -> analysis); an ASV
table enters at speciateIT; existing outputs enter at import. ``microfgt check`` verifies the
tools/paths the resolved entry point needs, up front.
"""

from __future__ import annotations

import argparse
import sys
import tempfile

from microfgt import __version__


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="microfgt", description="One-stop FGT microbiome analysis."
    )
    parser.add_argument("--version", action="version", version=f"microfgt {__version__}")
    parser.set_defaults(_run=lambda _a: parser.print_help())
    sub = parser.add_subparsers(title="commands")

    run = sub.add_parser("run", help="Run the workflow from a config file (any entry point).")
    run.add_argument("-c", "--config", required=True, help="YAML workflow config.")
    run.add_argument("-o", "--output", help="Output .h5mu (overrides config 'output').")
    run.add_argument("--workdir", help="Directory for intermediate artifacts.")
    run.add_argument("--executor", choices=["local", "snakemake"], default="local")
    run.set_defaults(_run=_cmd_run)

    chk = sub.add_parser("check", help="Preflight: verify tools/paths for the resolved entry point.")
    chk.add_argument("-c", "--config", required=True)
    chk.set_defaults(_run=_cmd_check)

    clf = sub.add_parser("classify", help="Classify CST on a .h5mu's composition modality.")
    clf.add_argument("-i", "--input", required=True)
    clf.add_argument("-o", "--output", required=True)
    clf.add_argument("-m", "--method", default="centroid")
    clf.set_defaults(_run=_cmd_classify)

    ana = sub.add_parser("analyze", help="Run analysis steps on a .h5mu's composition modality.")
    ana.add_argument("-i", "--input", required=True)
    ana.add_argument("-o", "--output", required=True)
    ana.add_argument("--transform", nargs="*", default=[], choices=["relabund", "clr"])
    ana.add_argument("--alpha", nargs="*", default=[], metavar="METRIC")
    ana.add_argument("--beta", default=None, metavar="METRIC")
    ana.add_argument("--ordinate", action="store_true")
    ana.add_argument("--diffabund-group", default=None, metavar="OBS_COL")
    ana.set_defaults(_run=_cmd_analyze)

    rs = sub.add_parser("_run-stage", help=argparse.SUPPRESS)  # internal (Snakemake calls it)
    rs.add_argument("stage")
    rs.add_argument("--workdir", required=True)
    rs.add_argument("--config", required=True)
    rs.add_argument("--output")
    rs.set_defaults(_run=_cmd_run_stage)
    return parser


def _load_config(path: str) -> dict:
    import yaml

    with open(path) as fh:
        return yaml.safe_load(fh) or {}


def _attach_cst(mdata, cst_df) -> None:
    from microfgt.io.integrate import attach_cst_annotations

    # Labels -> .obs; <subCST>_sim vectors -> composition_taxon.obsm['cst_sim'].
    attach_cst_annotations(mdata, cst_df)


def _cmd_run(args: argparse.Namespace) -> None:
    from microfgt.stages import LocalExecutor, SnakemakeExecutor, provided_artifacts, resolve

    config = _load_config(args.config)
    out = args.output or config.get("output")
    if not out:
        raise SystemExit("No output path: pass -o/--output or set 'output:' in the config.")
    workdir = args.workdir or tempfile.mkdtemp(prefix="microfgt_")
    stages = resolve("mudata", provided_artifacts(config))
    plan = " -> ".join(s.id for s in stages) or "(nothing to do)"

    if args.executor == "snakemake":
        path = SnakemakeExecutor().run(stages, args.config, workdir, out)
        print(f"entry-point plan: {plan}")
        print(f"wrote Snakefile: {path}\nsubmit on the cluster with snakemake + a Slurm profile.")
        return

    print(f"entry-point plan: {plan}")
    written = LocalExecutor().run(stages, workdir, config, out)
    print(f"wrote {written}")


def _cmd_check(args: argparse.Namespace) -> None:
    from microfgt.stages import check

    results = check(_load_config(args.config))
    for r in results:
        print(r.message)
    missing = [r for r in results if not r.ok]
    if missing:
        raise SystemExit(f"{len(missing)} prerequisite(s) missing — see above.")
    print("all prerequisites satisfied.")


def _cmd_run_stage(args: argparse.Namespace) -> None:
    from microfgt.stages import execute_stage

    execute_stage(args.stage, args.workdir, _load_config(args.config), args.output)


def _cmd_classify(args: argparse.Namespace) -> None:
    import mudata as md

    from microfgt.cst import classify_cst

    mdata = md.read(args.input)
    # CST is computed from the taxon roll-up; fall back to composition for older objects.
    mod = "composition_taxon" if "composition_taxon" in mdata.mod else "composition"
    if mod not in mdata.mod:
        raise SystemExit("Input has no composition modality to classify.")
    cst = classify_cst(mdata[mod], method=args.method)
    _attach_cst(mdata, cst)
    mdata.write(args.output)
    print(f"wrote {args.output}: classified CST ({args.method}) for {cst.shape[0]} samples")


def _cmd_analyze(args: argparse.Namespace) -> None:
    import mudata as md

    from microfgt.workflow import apply_analysis

    mdata = md.read(args.input)
    if "composition" not in mdata.mod:
        raise SystemExit("Input has no 'composition' modality to analyze.")
    cfg = {
        "transforms": args.transform,
        "alpha": args.alpha,
        "beta": args.beta,
        "ordinate": args.ordinate,
    }
    if args.diffabund_group:
        cfg["diffabund"] = {"group": args.diffabund_group, "method": "ancom"}
    apply_analysis(mdata["composition"], cfg)
    mdata.write(args.output)
    print(f"wrote {args.output}: analysis applied to composition modality")


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv if argv is not None else sys.argv[1:])
    args._run(args)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
