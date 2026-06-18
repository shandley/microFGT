"""speciateIT importer.

NOT real-output validated: no genuine MC_order7_results.txt ships with the fixtures
(speciateIT inputs only). These tests exercise the join/aggregation against a SYNTHETIC
classification file whose VALUES are the genuine ASV1-ASV10 assignments from the
speciateIT README, joined to the genuine ASV count table. Real-output validation is an
IOU for P3 (run speciateIT on test.fasta, then re-check here).
"""

import numpy as np
import pandas as pd
import pytest

from microfgt.io import import_speciateit


def _count_table(real_fixtures):
    return pd.read_csv(real_fixtures / "speciateit_test_count_table.csv", index_col=0)


def test_join_and_aggregate_to_taxon_by_sample(real_fixtures, test_data):
    results = test_data / "speciateit_MC_order7_results.synthetic.txt"
    ct_path = real_fixtures / "speciateit_test_count_table.csv"
    ct = _count_table(real_fixtures)

    adata = import_speciateit(results, ct_path)

    # Samples preserved (obs = samples), counts conserved (no drop/double-count).
    assert list(adata.obs_names) == [str(s) for s in ct.index]
    assert adata.layers["counts"].sum() == int(ct.to_numpy().sum())

    # ASV2 + ASV6 are both Lactobacillus_crispatus -> their columns aggregate.
    expected = (ct["ASV2"] + ct["ASV6"]).to_numpy()
    ti = list(adata.var_names).index("Lactobacillus_crispatus")
    np.testing.assert_array_equal(adata.layers["counts"][:, ti], expected)

    # Genus is derived from the classification.
    assert adata.var.loc["Lactobacillus_crispatus", "genus"] == "Lactobacillus"


def test_unclassified_bucketed_by_default(real_fixtures, test_data):
    # Only ASV1-ASV10 are classified; ASV11.. -> one Unclassified bucket.
    ct = _count_table(real_fixtures)
    adata = import_speciateit(
        test_data / "speciateit_MC_order7_results.synthetic.txt",
        real_fixtures / "speciateit_test_count_table.csv",
    )
    assert "Unclassified" in adata.var_names
    classified_asvs = [f"ASV{i}" for i in range(1, 11)]
    expected_unclassified = ct.drop(columns=classified_asvs).to_numpy().sum()
    ti = list(adata.var_names).index("Unclassified")
    assert adata.layers["counts"][:, ti].sum() == expected_unclassified


def test_unclassified_kept_per_asv_when_disabled(real_fixtures, test_data):
    # bucket_unclassified=False matches speciateIT's own count_table.py behaviour.
    adata = import_speciateit(
        test_data / "speciateit_MC_order7_results.synthetic.txt",
        real_fixtures / "speciateit_test_count_table.csv",
        bucket_unclassified=False,
    )
    assert "Unclassified" not in adata.var_names
    assert "ASV11" in adata.var_names  # unclassified ASV kept as its own "taxon"


def test_header_autodetect_matches_headerless(real_fixtures, test_data):
    # The headed and headerless variants must produce identical results.
    ct = real_fixtures / "speciateit_test_count_table.csv"
    a = import_speciateit(test_data / "speciateit_MC_order7_results.synthetic.txt", ct)
    b = import_speciateit(test_data / "speciateit_MC_order7_results.headed.txt", ct)
    assert list(a.var_names) == list(b.var_names)
    np.testing.assert_array_equal(a.layers["counts"], b.layers["counts"])
