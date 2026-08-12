"""speciateIT importer — taxonomic assignment kept at ASV grain (the source of truth).

REAL shape (FORMATS.md + the tool's own ``bin/count_table.py``): speciateIT classifies
**ASVs/sequences, NOT samples**. Its output ``MC_order7_results.txt`` is one row per
sequence keyed by the FASTA header: ``Sequence ID / Classification / posterior / nDecisions``.
**Sample identity is NOT in that file** — it lives in the ASV count table (the dada2
feature table: rows = sampleID, cols = ASVs).

**Feature grain = ASV.** ``import_speciateit`` returns an **ASV x sample** ``composition``
assay: one feature per ASV, carrying its ``classification``, ``genus``, and (when the FASTA
is supplied) its ``sequence`` — the source of truth that gates the speciateIT→VALENCIA CST
path. The ASV→taxon roll-up is a *separate* step (:func:`collapse_to_taxon`) that
materialises the ``composition_taxon`` assay CST and the descriptors consume; it is not done
at read time, so sequences and per-ASV identity are never discarded.

Header handling: speciateIT's own ``count_table.py`` reads the results file with
``header=None`` (positional columns), yet its README shows a header row. We *auto-detect*:
if the 3rd field of the first line is numeric, the file is headerless. Robust to either real
shape, but the importer still carries a real-output-validation IOU (no genuine
``MC_order7_results.txt`` ships with the fixtures) — discharge on the first real HTCF run.
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


def _read_fasta(fasta_path) -> dict[str, str]:
    """Parse a FASTA -> {header id: sequence}. Header id = first whitespace token after '>'."""
    seqs: dict[str, list[str]] = {}
    current = None
    with open(fasta_path) as fh:
        for line in fh:
            line = line.rstrip("\n")
            if not line:
                continue
            if line.startswith(">"):
                current = line[1:].split()[0]
                seqs[current] = []
            elif current is not None:
                seqs[current].append(line.strip())
    return {k: "".join(v) for k, v in seqs.items()}


def _genus_of(taxon: str) -> str:
    """First token of a Classification (e.g. 'Lactobacillus_iners' -> 'Lactobacillus').

    Imperfect for abbreviated names (e.g. 'Ca_Lachnocurva_vaginae' -> 'Ca'); refine when
    real speciateIT output is available (first HTCF run)."""
    return taxon.split("_")[0].split(" ")[0]


def import_speciateit(results_path, count_table_path, fasta=None) -> ad.AnnData:
    """Import speciateIT classifications + an ASV count table -> **ASV x sample** composition.

    Parameters
    ----------
    results_path:
        speciateIT ``MC_order7_results.txt`` (ASV -> Classification).
    count_table_path:
        ASV count table CSV (rows = sampleID, cols = ASVs); the dada2/feature table. Its
        columns define the feature set — every ASV becomes a feature, classified or not.
    fasta:
        Optional FASTA whose headers are the ASV ids (speciateIT's own input). When given,
        each ASV's sequence is attached to ``var['sequence']`` — the source of truth. ASVs
        with no record in the FASTA (e.g. a trimmed fixture) get a missing sequence.

    Returns
    -------
    anndata.AnnData
        ``obs`` = samples (``read_count`` = per-sample total). ``var`` = ASVs, with
        ``classification`` (the speciateIT label, or NaN if the ASV was not classified),
        ``genus`` (first token of the classification), and ``sequence`` (if a FASTA was
        supplied). ``X`` / ``layers['counts']`` hold ASV counts. This is the ``composition``
        modality; roll it up to taxa with :func:`collapse_to_taxon`.
    """
    asv2taxon = _read_asv_to_taxon(results_path)

    ct = pd.read_csv(count_table_path, index_col=0)  # samples x ASVs
    ct.index = ct.index.astype(str)
    asvs = [str(c) for c in ct.columns]

    classification = [asv2taxon.get(a) for a in asvs]  # None -> unclassified (kept as a feature)
    genus = [_genus_of(c) if c is not None else None for c in classification]
    var = pd.DataFrame(
        {"classification": classification, "genus": genus},
        index=pd.Index(asvs, name="asv"),
    )
    if fasta is not None:
        seq_by_id = _read_fasta(fasta)
        var["sequence"] = [seq_by_id.get(a) for a in asvs]

    obs = pd.DataFrame(
        {"read_count": ct.to_numpy().sum(axis=1)},
        index=pd.Index(ct.index, name="sample"),
    )
    counts = ct.to_numpy().astype(np.int64)
    adata = ad.AnnData(X=counts.astype(np.float32), obs=obs, var=var)
    adata.layers["counts"] = counts
    return adata


def collapse_to_taxon(
    composition: ad.AnnData,
    *,
    bucket_unclassified: bool = True,
    unclassified_label: str = "Unclassified",
) -> ad.AnnData:
    """Roll an ASV-grain ``composition`` up to a **taxon x sample** ``composition_taxon`` assay.

    Aggregates ASV counts by ``var['classification']`` (the glue speciateIT pushes onto the
    user). Counts are conserved — no sample is dropped or double-counted.

    Parameters
    ----------
    composition:
        ASV-grain AnnData from :func:`import_speciateit` (needs ``var['classification']``).
    bucket_unclassified:
        If True (default) all unclassified ASVs collapse into one ``unclassified_label``
        taxon (cleaner downstream); if False each unclassified ASV is kept as its own
        "taxon" under its ASV id (matches speciateIT's own ``count_table.py``).
    unclassified_label:
        Name of the unclassified bucket (default ``"Unclassified"``).

    Returns
    -------
    anndata.AnnData
        ``obs`` = samples (``read_count`` = per-sample total), ``var`` = taxa (with
        ``genus``). ``X`` / ``layers['counts']`` hold taxon counts. This is the
        ``composition_taxon`` modality that CST and the descriptors read.
    """
    if "classification" not in composition.var:
        raise ValueError(
            "collapse_to_taxon needs an ASV-grain composition with var['classification'] "
            "(as produced by import_speciateit)."
        )

    X = composition.layers["counts"] if "counts" in composition.layers else composition.X
    counts = pd.DataFrame(
        np.asarray(X),
        index=composition.obs_names.astype(str),
        columns=composition.var_names.astype(str),
    )  # samples x ASV

    cls = composition.var["classification"]
    labels = [
        (str(cls.loc[asv]) if not pd.isna(cls.loc[asv])
         else (unclassified_label if bucket_unclassified else str(asv)))
        for asv in composition.var_names
    ]

    # samples x taxa (groupby on columns via transpose; DataFrame.groupby(axis=1)
    # is removed in pandas 2.x).
    grouped = counts.T.groupby(pd.Index(labels, name="taxon")).sum().T
    grouped = grouped.astype(np.int64)
    taxa = list(grouped.columns)

    var = pd.DataFrame(
        {"genus": [t if t == unclassified_label else _genus_of(t) for t in taxa]},
        index=pd.Index(taxa, name="taxon"),
    )
    obs = pd.DataFrame(
        {"read_count": grouped.sum(axis=1).to_numpy()},
        index=pd.Index(grouped.index, name="sample"),
    )
    taxon_counts = grouped.to_numpy()
    adata = ad.AnnData(X=taxon_counts.astype(np.float32), obs=obs, var=var)
    adata.layers["counts"] = taxon_counts
    return adata
