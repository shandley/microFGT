"""CHARACTERIZE layer — per-sample descriptors that *augment* the CST label.

CST (VALENCIA) is one column that flattens community structure into a class. These
descriptors read out the structure it flattens — without replacing it or computing a second
CST. They are deterministic functions of the taxon-grain composition and land as plain
``.obs`` columns alongside CST:

* ``dominant_taxon``     — the taxon with the highest relative abundance
* ``dominance_pct``      — that taxon's abundance, as a percentage (0–100)
* ``n_taxa_over_10pct``  — how many taxa clear 10% (the diffuse/continuum signal)

More summaries (diversity, log-ratios) drop in the same way later.
"""

from __future__ import annotations

import anndata as ad
import numpy as np
import pandas as pd


def describe_composition(
    composition_taxon: ad.AnnData,
    *,
    dominance_threshold: float = 0.10,
) -> pd.DataFrame:
    """Compute per-sample augment descriptors from a taxon-grain composition.

    Parameters
    ----------
    composition_taxon:
        Taxon x sample AnnData (the ``composition_taxon`` assay from
        :func:`microfgt.io.collapse_to_taxon`).
    dominance_threshold:
        Fraction a taxon must exceed to count toward ``n_taxa_over_10pct`` (default 0.10).

    Returns
    -------
    pandas.DataFrame
        Indexed by sample, columns ``dominant_taxon`` / ``dominance_pct`` /
        ``n_taxa_over_10pct``. Computed over *all* taxa present, including any
        ``Unclassified`` bucket — an Unclassified-dominated sample is a real signal, not
        hidden. Zero-count samples get ``dominant_taxon = NaN`` and ``dominance_pct = 0``.
    """
    X = (
        composition_taxon.layers["counts"]
        if "counts" in composition_taxon.layers
        else composition_taxon.X
    )
    counts = pd.DataFrame(
        np.asarray(X, dtype=float),
        index=composition_taxon.obs_names.astype(str),
        columns=composition_taxon.var_names.astype(str),
    )

    totals = counts.sum(axis=1)
    rel = counts.div(totals, axis=0).fillna(0.0)  # zero-count sample -> all zeros

    dominance = rel.max(axis=1)
    dominant = rel.idxmax(axis=1).where(dominance > 0, other=np.nan)
    n_over = (rel > dominance_threshold).sum(axis=1).astype(int)

    return pd.DataFrame(
        {
            "dominant_taxon": dominant.to_numpy(),
            "dominance_pct": (dominance * 100).to_numpy(),
            "n_taxa_over_10pct": n_over.to_numpy(),
        },
        index=pd.Index(counts.index, name="sample"),
    )
