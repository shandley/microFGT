"""The classify_mgcst method seam — structurally parallel to the CST seam.

The blessed VISTA method ships with the shotgun orchestration layer (a later increment), so out
of the box no method is registered and the seam must fail helpfully rather than silently.
"""

import anndata as ad
import numpy as np
import pandas as pd
import pytest

from microfgt import mgcst


def _function(samples, genes):
    X = np.arange(len(samples) * len(genes), dtype=np.float32).reshape(len(samples), len(genes))
    return ad.AnnData(
        X=X,
        obs=pd.DataFrame(index=pd.Index(samples, name="sample")),
        var=pd.DataFrame(index=pd.Index(genes, name="gene")),
    )


def test_no_method_registered_by_default():
    # VISTA is orchestration; until it lands there is nothing to dispatch to.
    assert mgcst.available_methods() == []


def test_classify_mgcst_raises_on_unknown_method():
    func = _function(["s1", "s2"], ["g1", "g2", "g3"])
    with pytest.raises(ValueError, match="Unknown mgCST method"):
        mgcst.classify_mgcst(func, method="does-not-exist")


def test_classify_mgcst_default_method_raises_until_vista_registered():
    func = _function(["s1"], ["g1"])
    with pytest.raises(ValueError, match="import_mgcst"):
        mgcst.classify_mgcst(func)  # default "vista", not yet registered


def test_register_method_makes_it_dispatchable():
    func = _function(["s1", "s2"], ["g1", "g2"])
    sentinel = pd.DataFrame({"mgCST": [1, 2]}, index=["s1", "s2"])
    try:
        mgcst.register_method("fake", lambda function, **kw: sentinel)
        assert "fake" in mgcst.available_methods()
        out = mgcst.classify_mgcst(func, method="fake")
        assert list(out["mgCST"]) == [1, 2]
    finally:
        mgcst._METHODS.pop("fake", None)  # keep the global registry clean for other tests
