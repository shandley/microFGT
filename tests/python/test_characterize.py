"""Augment descriptors — per-sample summaries that augment (never replace) CST."""

import anndata as ad
import numpy as np
import pandas as pd

from microfgt.characterize import describe_composition


def _taxon_assay(counts_by_sample):
    """counts_by_sample: {sample: {taxon: count}} -> taxon x sample AnnData."""
    df = pd.DataFrame(counts_by_sample).T.fillna(0.0)  # samples x taxa
    adata = ad.AnnData(
        X=df.to_numpy().astype(np.float32),
        obs=pd.DataFrame(index=pd.Index(df.index, name="sample")),
        var=pd.DataFrame(index=pd.Index(df.columns, name="taxon")),
    )
    adata.layers["counts"] = df.to_numpy().astype(np.int64)
    return adata


def test_descriptors_capture_dominance_and_diffuseness():
    comp = _taxon_assay(
        {
            # dominated by one taxon: 90% Lactobacillus, one taxon over 10%.
            "dominated": {"Lactobacillus_iners": 90, "Gardnerella_vaginalis": 10},
            # diffuse: four taxa at 25% each -> four over 10%, dominant is a tie (first max).
            "diffuse": {"a": 25, "b": 25, "c": 25, "d": 25},
        }
    )
    out = describe_composition(comp)

    assert out.loc["dominated", "dominant_taxon"] == "Lactobacillus_iners"
    assert out.loc["dominated", "dominance_pct"] == 90.0
    assert out.loc["dominated", "n_taxa_over_10pct"] == 1

    assert out.loc["diffuse", "dominance_pct"] == 25.0
    assert out.loc["diffuse", "n_taxa_over_10pct"] == 4


def test_unclassified_bucket_counts_as_a_taxon():
    # An Unclassified-dominated sample is a real signal, surfaced not hidden.
    comp = _taxon_assay({"s": {"Unclassified": 80, "Lactobacillus_iners": 20}})
    out = describe_composition(comp)
    assert out.loc["s", "dominant_taxon"] == "Unclassified"
    assert out.loc["s", "dominance_pct"] == 80.0


def test_zero_count_sample_has_no_dominant_taxon():
    comp = _taxon_assay({"empty": {"a": 0, "b": 0}, "ok": {"a": 10, "b": 0}})
    out = describe_composition(comp)
    assert pd.isna(out.loc["empty", "dominant_taxon"])
    assert out.loc["empty", "dominance_pct"] == 0.0
    assert out.loc["empty", "n_taxa_over_10pct"] == 0
    assert out.loc["ok", "dominant_taxon"] == "a"
