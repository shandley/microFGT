"""VIRGO importer — functional profiling (gene counts).

REAL shape (FORMATS.md, validated against ``virgo_sub*.out``): VIRGO writes
**one file per sample**, named ``<sample>.out``, **TSV, NO header, 3 columns**
``geneID \\t read_count \\t gene_length``. Only nonzero genes appear in a file;
the sample identity is the filename.

GLUE we own (so the user doesn't): stack the per-sample files into one
gene x sample matrix, zero-filling genes absent from a given sample, and orient to
MuData convention (obs = samples, var = genes).
"""

from __future__ import annotations

from pathlib import Path

import anndata as ad
import numpy as np
import pandas as pd

_COLS = ["gene_id", "read_count", "gene_length"]


def import_virgo(directory, pattern: str = "*.out", taxon_table=None) -> ad.AnnData:
    """Stack a directory of VIRGO per-sample ``.out`` files into an AnnData.

    Parameters
    ----------
    directory:
        Directory containing one ``<sample>.out`` file per sample.
    pattern:
        Glob for the per-sample files (default ``"*.out"``). The sample name is the
        filename with this suffix stripped.
    taxon_table:
        Optional path to VIRGO's ``1.taxon.tbl.txt`` (``Cluster\\tgeneID\\ttaxon\\tlength``).
        If given, a ``taxon`` column is added to ``var`` (catalog annotation join).

    Returns
    -------
    anndata.AnnData
        ``obs`` = samples, ``var`` = genes (with ``gene_length``; ``taxon`` if a catalog
        was supplied). ``X`` holds read counts; ``layers["counts"]`` is the integer copy.
    """
    directory = Path(directory)
    files = sorted(directory.glob(pattern))
    if not files:
        raise FileNotFoundError(
            f"No VIRGO per-sample files matching {pattern!r} in {directory}. "
            "Expected one '<sample>.out' file per sample (3-column TSV, no header)."
        )

    suffix_len = len(pattern.lstrip("*"))  # "*.out" -> strip ".out"
    counts_by_sample: dict[str, pd.Series] = {}
    gene_length: dict[str, int] = {}
    for f in files:
        sample = f.name[:-suffix_len] if suffix_len else f.name
        df = pd.read_csv(f, sep="\t", header=None, names=_COLS)
        if sample in counts_by_sample:
            raise ValueError(f"Duplicate sample name {sample!r} from file {f}")
        counts_by_sample[sample] = df.set_index("gene_id")["read_count"]
        gene_length.update(zip(df["gene_id"], df["gene_length"]))

    # genes x samples, zero-filled; sorted for deterministic ordering.
    wide = pd.DataFrame(counts_by_sample).fillna(0).astype(np.int64).sort_index()
    samples = list(wide.columns)
    genes = list(wide.index)

    var = pd.DataFrame(
        {"gene_id": genes, "gene_length": [gene_length[g] for g in genes]},
        index=pd.Index(genes, name="gene"),
    )
    if taxon_table is not None:
        tax = pd.read_csv(
            taxon_table, sep="\t", header=None,
            names=["cluster", "gene_id", "taxon", "length"],
        )
        gene2taxon = dict(zip(tax["gene_id"], tax["taxon"]))
        var["taxon"] = [gene2taxon.get(g, "Unannotated") for g in genes]

    obs = pd.DataFrame(index=pd.Index(samples, name="sample"))
    counts = wide.T.to_numpy()  # samples x genes
    adata = ad.AnnData(X=counts.astype(np.float32), obs=obs, var=var)
    adata.layers["counts"] = counts
    return adata
