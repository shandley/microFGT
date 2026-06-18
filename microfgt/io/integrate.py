"""Assemble per-modality assays into one sample-keyed MuData (layer 1, the core object).

A MuData holds sample-keyed assays in one object: ``composition`` (taxon x sample, from
speciateIT) and ``function`` (gene x sample, from VIRGO), with CST and clinical/sample
variables as sample-level annotations. This is the currency every later layer reads/writes.

**Honest sample reconciliation** (constraint B): we never silently drop or double-count
samples across assays. MuData keeps the *union* of sample ids; ``build_mudata`` reports
how the assays and the CST table overlap so mismatches surface instead of hiding.

A caveat the fixtures make concrete: ``prototype/real_fixtures/`` carries outputs from
three *different* tool repos whose sample ids do NOT match (``sample1...`` vs ``sub1/sub2``
vs ``1,2,...``) — they are not a co-assayed cohort. So these fixtures validate each
importer and exercise the assembly/reconciliation mechanics, but a true end-to-end
cross-modality integration needs a real co-assayed dataset (an open question in the spec).
"""

from __future__ import annotations

from dataclasses import dataclass

import anndata as ad
import mudata as md
import pandas as pd


@dataclass
class Reconciliation:
    """How sample ids overlapped across the assembled assays + CST table."""

    n_samples: int          # size of the union
    per_modality: dict      # modality -> n samples
    shared_all: int         # samples present in every modality
    cst_matched: int        # CST rows that matched a sample in the union
    cst_unmatched: list     # CST sample ids with no assay

    def summary(self) -> str:
        mods = ", ".join(f"{k}={v}" for k, v in self.per_modality.items())
        s = (
            f"{self.n_samples} samples in union ({mods}); "
            f"{self.shared_all} shared across all assays"
        )
        if self.per_modality:
            s += f"; CST matched {self.cst_matched}, unmatched {len(self.cst_unmatched)}"
        return s


def build_mudata(
    composition: ad.AnnData | None = None,
    function: ad.AnnData | None = None,
    cst: pd.DataFrame | None = None,
    obs: pd.DataFrame | None = None,
) -> md.MuData:
    """Build a MuData from the available modalities and attach sample-level annotations.

    Parameters
    ----------
    composition:
        taxon x sample AnnData from :func:`microfgt.io.import_speciateit`.
    function:
        gene x sample AnnData from :func:`microfgt.io.import_virgo`.
    cst:
        Sample-keyed CST/subCST/score from :func:`microfgt.io.import_valencia`.
    obs:
        Optional extra sample-level (clinical) annotations, indexed by sample id.

    Returns
    -------
    mudata.MuData
        Modalities under ``composition`` / ``function`` keys; CST and clinical variables
        joined onto the global ``.obs`` by sample id. A :class:`Reconciliation` is stored
        in ``mdata.uns["reconciliation"]`` (and its one-line summary in
        ``...["reconciliation_summary"]``).
    """
    mods: dict[str, ad.AnnData] = {}
    if composition is not None:
        mods["composition"] = composition
    if function is not None:
        mods["function"] = function
    if not mods:
        raise ValueError("build_mudata needs at least one modality (composition/function).")

    # Adopt mudata's forthcoming default: don't auto-pull modality obs/var into the
    # global frame. We attach sample annotations explicitly below, so we don't rely on it.
    with md.set_options(pull_on_update=False):
        mdata = md.MuData(mods)

    # Reconciliation report over the union of sample ids (MuData's global obs).
    union = list(mdata.obs_names)
    union_set = set(union)
    per_modality = {k: a.n_obs for k, a in mods.items()}
    shared_all = len(set.intersection(*(set(a.obs_names) for a in mods.values())))

    cst_matched, cst_unmatched = 0, []
    if cst is not None:
        cst = cst.copy()
        cst.index = cst.index.astype(str)
        matched_idx = [s for s in cst.index if s in union_set]
        cst_matched = len(matched_idx)
        cst_unmatched = [s for s in cst.index if s not in union_set]
        # Align to the union without dropping assay samples (unmatched -> NaN).
        aligned = cst.reindex(union)
        for col in aligned.columns:
            mdata.obs[col] = aligned[col].to_numpy()

    if obs is not None:
        obs = obs.copy()
        obs.index = obs.index.astype(str)
        aligned = obs.reindex(union)
        for col in aligned.columns:
            mdata.obs[col] = aligned[col].to_numpy()

    recon = Reconciliation(
        n_samples=len(union),
        per_modality=per_modality,
        shared_all=shared_all,
        cst_matched=cst_matched,
        cst_unmatched=cst_unmatched,
    )
    mdata.uns["reconciliation_summary"] = recon.summary()
    mdata.uns["reconciliation"] = recon.__dict__
    return mdata
