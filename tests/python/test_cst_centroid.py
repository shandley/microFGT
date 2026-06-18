"""Centroid CST — validated against GENUINE Valencia.py output + the swappable seam.

Two validation strengths:
* Exact reproduction of genuine Valencia.py output on the 6-sample head fixture
  (always-on; uses the bundled centroids). This is real-output validation.
* The full 99.9%-vs-VALENCIA gate on the 13k published samples lives in
  test_cst_validation_gate.py (skips if the large dataset isn't staged locally).
"""

import numpy as np
import pandas as pd
import pytest

from microfgt.cst import CST_ORDER, available_methods, classify_cst, register_method
from microfgt.cst.centroid import classify_centroid

_APPENDED = {"subCST", "score", "CST"}


def _reconstruct_input(genuine: pd.DataFrame):
    """Recover (taxa counts, read_count) from a genuine VALENCIA output CSV."""
    taxa_cols = [
        c for c in genuine.columns
        if c not in ("sampleID", "read_count") and not c.endswith("_sim") and c not in _APPENDED
    ]
    counts = genuine[taxa_cols].copy()
    counts.index = genuine["sampleID"].astype(str)
    rc = pd.Series(genuine["read_count"].to_numpy(), index=counts.index)
    return counts, rc


def test_centroid_reproduces_genuine_valencia_output(real_fixtures):
    genuine = pd.read_csv(real_fixtures / "valencia_genuine_output_head.csv")
    counts, rc = _reconstruct_input(genuine)

    out = classify_centroid(counts, read_count=rc)

    # Labels reproduced exactly.
    gi = genuine.set_index(genuine["sampleID"].astype(str))
    assert list(out["subCST"]) == list(gi["subCST"])
    assert list(out["CST"]) == list(gi["CST"])
    np.testing.assert_allclose(out["score"].to_numpy(), gi["score"].to_numpy(), atol=1e-6)

    # Every one of the 13 similarity columns reproduced to numerical precision.
    for cst in CST_ORDER:
        col = f"{cst}_sim"
        np.testing.assert_allclose(
            out[col].to_numpy(), gi[col].to_numpy(), atol=1e-6,
            err_msg=f"{col} mismatch vs genuine Valencia.py output",
        )


def test_classify_cst_dispatches_to_centroid(real_fixtures):
    genuine = pd.read_csv(real_fixtures / "valencia_genuine_output_head.csv")
    counts, rc = _reconstruct_input(genuine)
    via_iface = classify_cst(counts, method="centroid", read_count=rc)
    direct = classify_centroid(counts, read_count=rc)
    pd.testing.assert_frame_equal(via_iface, direct)


def test_seam_is_swappable():
    # The interface is the point: a new method registers and is callable by name.
    assert "centroid" in available_methods()

    def _dummy(composition, **kw):
        return pd.DataFrame({"CST": ["IV-C"]}, index=["s1"])

    register_method("dummy", _dummy)
    assert "dummy" in available_methods()
    assert classify_cst(pd.DataFrame(index=["s1"]), method="dummy").loc["s1", "CST"] == "IV-C"


def test_unknown_method_raises():
    with pytest.raises(ValueError, match="Unknown CST method"):
        classify_cst(pd.DataFrame(index=["s1"]), method="nope")
