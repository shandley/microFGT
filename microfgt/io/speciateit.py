"""speciateIT importer — taxonomic assignment, rolled up to taxon x sample.

REAL shape (FORMATS.md + the tool's own ``bin/count_table.py``): speciateIT classifies
**ASVs/sequences, NOT samples**. Its output ``MC_order7_results.txt`` is one row per
sequence keyed by the FASTA header: ``Sequence ID / Classification / posterior / nDecisions``.
**Sample identity is NOT in that file** — it lives in the ASV count table (the dada2
feature table: rows = sampleID, cols = ASVs).

GLUE we own (the integration burden speciateIT pushes onto the user): join
ASV->Classification against the ASV x sample counts and aggregate ASV->taxon.

Two faithfulness notes, because the format was NOT real-output validated (no genuine
``MC_order7_results.txt`` ships with the fixtures):

* **Header handling.** speciateIT's own ``count_table.py`` reads the file with
  ``header=None`` (positional columns), yet its README shows a header row. We therefore
  *auto-detect*: if the 3rd field of the first line is numeric, the file is headerless.
  This is robust to either real shape — but the importer carries a real-output-validation
  IOU (resolve at P3 by running speciateIT on ``test.fasta`` and re-checking).
* **Unclassified ASVs.** ``count_table.py`` keeps each unclassified ASV as its own column
  (spurious single-ASV "taxa"). We instead aggregate them into one ``Unclassified`` bucket
  by default (cleaner downstream; totals preserved via per-sample ``read_count``).
  Set ``bucket_unclassified=False`` to match ``count_table.py`` exactly.
"""

from __future__ import annotations

from pathlib import Path

import anndata as ad
import numpy as np
import pandas as pd


def _is_number(x) -> bool:
    try:
        float(x)
        return True
    except (TypeError, ValueError):
        return False


def _read_asv_to_taxon(results_path) -> dict[str, str]:
    """Parse MC_order7_results.txt -> {ASV id: Classification}, auto-detecting a header."""
    raw = pd.read_csv(results_path, sep="\t", header=None, dtype=str)
    if raw.shape[1] < 2:
        raise ValueError(
            f"{results_path} does not look like speciateIT output "
            "(need at least 'Sequence ID' and 'Classification' tab-separated columns)."
        )
    # Detect a header row: in real output the 3rd col is the posterior probability
    # (numeric). If it is non-numeric on row 0, row 0 is a header.
    probe_col = 2 if raw.shape[1] > 2 else 1
    if not _is_number(raw.iloc[0, probe_col]):
        raw = raw.iloc[1:]
    return dict(zip(raw.iloc[:, 0].astype(str), raw.iloc[:, 1].astype(str)))


def _genus_of(taxon: str) -> str:
    """First token of a Classification (e.g. 'Lactobacillus_iners' -> 'Lactobacillus').

    Imperfect for abbreviated names (e.g. 'Ca_Lachnocurva_vaginae' -> 'Ca'); refine when
    real speciateIT output is available (P3)."""
    return taxon.split("_")[0].split(" ")[0]


def import_speciateit(
    results_path,
    count_table_path,
    unclassified_label: str = "Unclassified",
    bucket_unclassified: bool = True,
) -> ad.AnnData:
    """Join speciateIT ASV classifications to an ASV count table -> taxon x sample.

    Parameters
    ----------
    results_path:
        speciateIT ``MC_order7_results.txt`` (ASV -> Classification).
    count_table_path:
        ASV count table CSV (rows = sampleID, cols = ASVs); the dada2/feature table.
    unclassified_label:
        Bucket name for ASVs with no classification (default ``"Unclassified"``).
    bucket_unclassified:
        If True (default) aggregate all unclassified ASVs into one bucket; if False keep
        each unclassified ASV as its own column (matches speciateIT's ``count_table.py``).

    Returns
    -------
    anndata.AnnData
        ``obs`` = samples (with ``read_count`` = per-sample total), ``var`` = taxa (with
        ``classification`` and ``genus``). ``X`` holds counts; ``layers["counts"]`` the
        integer copy. This is the ``composition`` modality of the integrated object.
    """
    asv2taxon = _read_asv_to_taxon(results_path)

    ct = pd.read_csv(count_table_path, index_col=0)  # samples x ASVs
    ct.index = ct.index.astype(str)

    label = (
        unclassified_label if bucket_unclassified
        else None  # sentinel: keep the ASV's own id
    )
    taxa = [
        asv2taxon.get(asv, label if label is not None else asv)
        for asv in ct.columns
    ]

    # samples x taxa (groupby on columns via transpose; DataFrame.groupby(axis=1)
    # is removed in pandas 2.x).
    grouped = ct.T.groupby(pd.Index(taxa, name="taxon")).sum().T
    grouped = grouped.astype(np.int64)
    taxa_names = list(grouped.columns)

    var = pd.DataFrame(
        {
            "classification": taxa_names,
            "genus": [_genus_of(t) for t in taxa_names],
        },
        index=pd.Index(taxa_names, name="taxon"),
    )
    obs = pd.DataFrame(
        {"read_count": grouped.sum(axis=1).to_numpy()},
        index=pd.Index(grouped.index, name="sample"),
    )
    counts = grouped.to_numpy()
    adata = ad.AnnData(X=counts.astype(np.float32), obs=obs, var=var)
    adata.layers["counts"] = counts
    return adata
