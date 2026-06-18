"""Turnkey CLI entry point.

The full workflow CLI (``microfgt run`` / ``import`` / ``classify`` …) is designed at
P4; this is the P0 skeleton so the one-command install registers a working ``microfgt``
command. The Python API in :mod:`microfgt.io` is the substance for now.
"""

from __future__ import annotations

import argparse
import sys

from microfgt import __version__


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="microfgt",
        description="One-stop FGT microbiome analysis (CLI under construction — see P4).",
    )
    parser.add_argument("--version", action="version", version=f"microfgt {__version__}")
    parser.set_defaults(_run=lambda _args: parser.print_help())

    sub = parser.add_subparsers(title="commands")
    imp = sub.add_parser("import", help="Import tool outputs into the integrated object.")
    imp.add_argument("tool", choices=["speciateit", "valencia", "virgo"])
    imp.set_defaults(_run=_cmd_import)
    return parser


def _cmd_import(args: argparse.Namespace) -> None:
    raise SystemExit(
        f"`microfgt import {args.tool}` is not wired to the CLI yet (P4). "
        f"Use the Python API: `from microfgt.io import import_{args.tool}`."
    )


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv if argv is not None else sys.argv[1:])
    args._run(args)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
