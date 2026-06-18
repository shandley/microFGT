"""Turnkey CLI — runs the whole workflow, with the Python API underneath for power users.

Config-first (constraint A): ``microfgt run -c config.yaml`` drives the end-to-end workflow;
``classify`` and ``analyze`` operate on an existing ``.h5mu``. microFGT owns the glue.
"""

from __future__ import annotations

import argparse
import sys

from microfgt import __version__


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="microfgt", description="One-stop FGT microbiome analysis."
    )
    parser.add_argument("--version", action="version", version=f"microfgt {__version__}")
    parser.set_defaults(_run=lambda _a: parser.print_help())
    sub = parser.add_subparsers(title="commands")

    run = sub.add_parser("run", help="Run the whole workflow from a config file.")
    run.add_argument("-c", "--config", required=True, help="YAML workflow config.")
    run.add_argument("-o", "--output", help="Output .h5mu (overrides config 'output').")
    run.set_defaults(_run=_cmd_run)

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
    return parser


def _load_config(path: str) -> dict:
    import yaml

    with open(path) as fh:
        return yaml.safe_load(fh) or {}


def _attach_cst(mdata, cst_df) -> None:
    aligned = cst_df.reindex([str(s) for s in mdata.obs_names])
    for col in aligned.columns:
        mdata.obs[col] = aligned[col].to_numpy()


def _cmd_run(args: argparse.Namespace) -> None:
    from microfgt.workflow import run_workflow

    config = _load_config(args.config)
    out = args.output or config.get("output")
    if not out:
        raise SystemExit("No output path: pass -o/--output or set 'output:' in the config.")
    mdata = run_workflow(config)
    mdata.write(out)
    print(f"wrote {out}: {mdata.shape[0]} samples, modalities {list(mdata.mod)}")
    print(mdata.uns.get("reconciliation_summary", ""))


def _cmd_classify(args: argparse.Namespace) -> None:
    import mudata as md

    from microfgt.cst import classify_cst

    mdata = md.read(args.input)
    if "composition" not in mdata.mod:
        raise SystemExit("Input has no 'composition' modality to classify.")
    cst = classify_cst(mdata["composition"], method=args.method)
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
