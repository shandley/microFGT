"""End-to-end workflow — the turnkey path the CLI drives.

Config-first (constraint A): one declarative config describes the inputs and the steps, and
microFGT owns all the glue — import each modality, classify CST, run the bought analysis,
assemble one MuData. The same functions are callable from Python for power users.

Config shape (any subset; see example_config.yaml at the repo root):

    composition:
      speciateit: {results: MC_order7_results.txt, count_table: counts.csv}
    function:
      virgo: {dir: path/to/virgo_outs}
    cst:
      method: centroid            # classify from composition, OR
      valencia: valencia_out.csv  # import existing VALENCIA labels
    analysis:
      transforms: [relabund, clr]
      alpha: [shannon]
      beta: braycurtis
      ordinate: true
      diffabund: {group: CST, method: ancom}
    output: result.h5mu
"""

from __future__ import annotations

import anndata as ad
import mudata as md
import pandas as pd

from microfgt import analysis
from microfgt.cst import classify_cst
from microfgt.io import (
    build_mudata,
    collapse_to_taxon,
    import_speciateit,
    import_valencia,
    import_virgo,
)

_TRANSFORMS = {"relabund": analysis.relative_abundance, "clr": analysis.clr_transform}


def apply_analysis(adata: ad.AnnData, cfg: dict) -> None:
    """Run the configured analysis steps on a modality, in place."""
    for name in cfg.get("transforms", []):
        if name not in _TRANSFORMS:
            raise ValueError(f"Unknown transform {name!r} (have: {sorted(_TRANSFORMS)}).")
        _TRANSFORMS[name](adata)
    for metric in cfg.get("alpha", []):
        analysis.alpha_diversity(adata, metric=metric)
    if cfg.get("beta"):
        analysis.beta_diversity(adata, metric=cfg["beta"])
    if cfg.get("ordinate"):
        analysis.ordinate(adata, metric=cfg.get("beta") or "braycurtis")
    da = cfg.get("diffabund")
    if da:
        result = analysis.differential_abundance(
            adata, group_key=da["group"], method=da.get("method", "ancom")
        )
        adata.uns["diffabund"] = {
            "group": da["group"],
            "method": da.get("method", "ancom"),
            "result": result.reset_index().to_dict(orient="list"),
        }


def run_workflow(config: dict) -> md.MuData:
    """Import -> characterize (CST + descriptors) -> analyze -> assemble one MuData."""
    composition = composition_taxon = function = cst = None

    comp_cfg = config.get("composition", {})
    if "speciateit" in comp_cfg:
        s = comp_cfg["speciateit"]
        composition = import_speciateit(s["results"], s["count_table"], fasta=s.get("fasta"))
        composition_taxon = collapse_to_taxon(composition)

    func_cfg = config.get("function", {})
    if "virgo" in func_cfg:
        function = import_virgo(func_cfg["virgo"]["dir"])

    # CST is computed from the taxon roll-up (VALENCIA operates on taxon abundances).
    cst_cfg = config.get("cst", {})
    if "valencia" in cst_cfg:
        cst = import_valencia(cst_cfg["valencia"])
    elif cst_cfg.get("method") and composition_taxon is not None:
        cst = classify_cst(composition_taxon, method=cst_cfg["method"])

    # Analysis runs on the ASV-grain composition modality before assembly (results live on it).
    if composition is not None and config.get("analysis"):
        apply_analysis(composition, config["analysis"])

    return build_mudata(
        composition=composition,
        composition_taxon=composition_taxon,
        function=function,
        cst=cst,
    )
