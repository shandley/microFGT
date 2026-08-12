"""speciateIT importer (ASV grain) + the taxon roll-up.

NOT real-output validated: no genuine MC_order7_results.txt ships with the fixtures
(speciateIT inputs only). These tests exercise the join against a SYNTHETIC classification
file whose VALUES are the genuine ASV1-ASV10 assignments from the speciateIT README, joined
to the genuine ASV count table (169 samples x 1514 ASVs) and the trimmed genuine FASTA
(ASV1-ASV10). Real-output validation is an IOU for the first real HTCF run.
"""

import numpy as np
import pandas as pd
import pytest

from microfgt.io import collapse_to_taxon, import_speciateit

RESULTS = "speciateit_MC_order7_results.synthetic.txt"


def _count_table(real_fixtures):
    return pd.read_csv(real_fixtures / "speciateit_test_count_table.csv", index_col=0)


# --- import: ASV grain, sequences retained --------------------------------------------------
def test_import_is_asv_grain_and_conserves_counts(real_fixtures, test_data):
    ct = _count_table(real_fixtures)
    adata = import_speciateit(test_data / RESULTS, real_fixtures / "speciateit_test_count_table.csv")

    # One feature per ASV (no read-time collapse); samples preserved; counts conserved.
    assert list(adata.var_names) == [str(c) for c in ct.columns]
    assert adata.n_vars == ct.shape[1]  # 1514 ASVs
    assert list(adata.obs_names) == [str(s) for s in ct.index]
    assert adata.layers["counts"].sum() == int(ct.to_numpy().sum())


def test_import_keeps_classification_and_genus_per_asv(real_fixtures, test_data):
    adata = import_speciateit(test_data / RESULTS, real_fixtures / "speciateit_test_count_table.csv")

    assert adata.var.loc["ASV2", "classification"] == "Lactobacillus_crispatus"
    assert adata.var.loc["ASV2", "genus"] == "Lactobacillus"
    # Unclassified ASVs keep their identity as features, with a missing classification.
    assert pd.isna(adata.var.loc["ASV11", "classification"])


def test_import_retains_sequences_when_fasta_given(real_fixtures, test_data):
    adata = import_speciateit(
        test_data / RESULTS,
        real_fixtures / "speciateit_test_count_table.csv",
        fasta=real_fixtures / "speciateit_test.fasta",
    )
    # ASV1-ASV10 are in the trimmed FASTA; the rest are not (missing sequence, honestly).
    assert adata.var.loc["ASV1", "sequence"].startswith("TAGGGAAT")
    assert pd.isna(adata.var.loc["ASV11", "sequence"])


def test_import_without_fasta_has_no_sequence_column(real_fixtures, test_data):
    adata = import_speciateit(test_data / RESULTS, real_fixtures / "speciateit_test_count_table.csv")
    assert "sequence" not in adata.var


# --- roll-up: ASV -> taxon ------------------------------------------------------------------
def test_collapse_aggregates_asvs_sharing_a_classification(real_fixtures, test_data):
    ct = _count_table(real_fixtures)
    comp = import_speciateit(test_data / RESULTS, real_fixtures / "speciateit_test_count_table.csv")
    taxon = collapse_to_taxon(comp)

    # ASV2 + ASV6 are both Lactobacillus_crispatus -> their columns aggregate.
    expected = (ct["ASV2"] + ct["ASV6"]).to_numpy()
    ti = list(taxon.var_names).index("Lactobacillus_crispatus")
    np.testing.assert_array_equal(taxon.layers["counts"][:, ti], expected)
    assert taxon.var.loc["Lactobacillus_crispatus", "genus"] == "Lactobacillus"
    # Roll-up conserves total counts.
    assert taxon.layers["counts"].sum() == comp.layers["counts"].sum()


def test_collapse_buckets_unclassified_by_default(real_fixtures, test_data):
    ct = _count_table(real_fixtures)
    comp = import_speciateit(test_data / RESULTS, real_fixtures / "speciateit_test_count_table.csv")
    taxon = collapse_to_taxon(comp)

    assert "Unclassified" in taxon.var_names
    classified_asvs = [f"ASV{i}" for i in range(1, 11)]
    expected_unclassified = ct.drop(columns=classified_asvs).to_numpy().sum()
    ti = list(taxon.var_names).index("Unclassified")
    assert taxon.layers["counts"][:, ti].sum() == expected_unclassified


def test_collapse_keeps_unclassified_per_asv_when_disabled(real_fixtures, test_data):
    # bucket_unclassified=False matches speciateIT's own count_table.py behaviour.
    comp = import_speciateit(test_data / RESULTS, real_fixtures / "speciateit_test_count_table.csv")
    taxon = collapse_to_taxon(comp, bucket_unclassified=False)

    assert "Unclassified" not in taxon.var_names
    assert "ASV11" in taxon.var_names  # unclassified ASV kept as its own "taxon"


def test_collapse_requires_asv_grain():
    import anndata as ad

    bare = ad.AnnData(
        X=np.ones((2, 2), dtype=np.float32),
        obs=pd.DataFrame(index=["s1", "s2"]),
        var=pd.DataFrame(index=["taxonA", "taxonB"]),  # no 'classification'
    )
    with pytest.raises(ValueError, match="classification"):
        collapse_to_taxon(bare)


def test_header_autodetect_matches_headerless(real_fixtures, test_data):
    # The headed and headerless variants must produce identical ASV-grain results.
    ct = real_fixtures / "speciateit_test_count_table.csv"
    a = import_speciateit(test_data / RESULTS, ct)
    b = import_speciateit(test_data / "speciateit_MC_order7_results.headed.txt", ct)
    assert list(a.var_names) == list(b.var_names)
    np.testing.assert_array_equal(a.layers["counts"], b.layers["counts"])
    pd.testing.assert_series_equal(a.var["classification"], b.var["classification"])
