"""Integrated object (MuData) assembly + honest sample reconciliation."""

import numpy as np
import pandas as pd
import pytest

from microfgt.io import build_mudata, import_valencia, import_virgo
from microfgt.io.speciateit import import_speciateit


def _aligned_composition(samples, taxa):
    import anndata as ad

    X = np.arange(len(samples) * len(taxa), dtype=np.float32).reshape(len(samples), len(taxa))
    return ad.AnnData(
        X=X,
        obs=pd.DataFrame(index=pd.Index(samples, name="sample")),
        var=pd.DataFrame(index=pd.Index(taxa, name="taxon")),
    )


def test_build_mudata_aligned_samples_attaches_cst():
    # Synthetic aligned cohort: assays + CST share sample ids -> clean join.
    comp = _aligned_composition(["s1", "s2", "s3"], ["Lactobacillus_iners", "Gardnerella_vaginalis"])
    func = _aligned_composition(["s1", "s2", "s3"], ["geneA", "geneB", "geneC"])
    cst = pd.DataFrame(
        {"CST": ["I", "IV-A", "III"], "subCST": ["I-B", "IV-A", "III-A"], "score": [0.99, 0.7, 0.8]},
        index=pd.Index(["s1", "s2", "s3"], name="sample"),
    )

    mdata = build_mudata(composition=comp, function=func, cst=cst)

    assert set(mdata.mod.keys()) == {"composition", "function"}
    assert mdata["composition"].n_obs == 3
    assert list(mdata.obs["CST"]) == ["I", "IV-A", "III"]
    recon = mdata.uns["reconciliation"]
    assert recon["shared_all"] == 3
    assert recon["cst_matched"] == 3
    assert recon["cst_unmatched"] == []


def test_reconciliation_reports_mismatch_without_dropping():
    # Honest reconciliation: a CST sample with no assay is reported, not silently dropped;
    # an assay sample with no CST gets NaN, not removed.
    comp = _aligned_composition(["s1", "s2"], ["Lactobacillus_iners"])
    cst = pd.DataFrame(
        # s2 has an assay (matches); s_ghost has CST but no assay (unmatched).
        {"CST": ["IV-B", "I"], "subCST": ["IV-B", "I-A"], "score": [0.6, 0.9]},
        index=pd.Index(["s2", "s_ghost"], name="sample"),
    )

    mdata = build_mudata(composition=comp, cst=cst)

    assert mdata["composition"].n_obs == 2          # s1 kept despite no CST
    assert pd.isna(mdata.obs.loc["s1", "CST"])       # NaN, not dropped
    assert mdata.obs.loc["s2", "CST"] == "IV-B"
    recon = mdata.uns["reconciliation"]
    assert recon["cst_matched"] == 1
    assert recon["cst_unmatched"] == ["s_ghost"]     # surfaced, not hidden


def test_build_mudata_requires_a_modality():
    with pytest.raises(ValueError):
        build_mudata()


def test_real_fixtures_are_not_a_co_assayed_cohort(real_fixtures, test_data):
    # Documents the honest-scope point: the three real fixtures come from different repos
    # and their sample ids DO NOT overlap, so a true cross-modality MuData can't be built
    # from them. The importers still each validate; integration needs a co-assayed dataset.
    comp = import_speciateit(
        test_data / "speciateit_MC_order7_results.synthetic.txt",
        real_fixtures / "speciateit_test_count_table.csv",
    )
    import tempfile, pathlib
    d = pathlib.Path(tempfile.mkdtemp())
    for name in ("virgo_sub1.out", "virgo_sub2.out"):
        (d / name).write_bytes((real_fixtures / name).read_bytes())
    func = import_virgo(d)
    cst = import_valencia(real_fixtures / "valencia_genuine_output_head.csv")

    comp_ids, func_ids, cst_ids = set(comp.obs_names), set(func.obs_names), set(cst.index)
    assert comp_ids.isdisjoint(func_ids)   # sample1.. vs sub1/sub2
    assert comp_ids.isdisjoint(cst_ids)    # sample1.. vs 1,2,..

    mdata = build_mudata(composition=comp, function=func, cst=cst)
    assert mdata.uns["reconciliation"]["shared_all"] == 0  # reported, honestly
