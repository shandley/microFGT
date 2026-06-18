"""CST method layer — a swappable ``classify_cst(composition, method=...)`` interface.

The seam exists from day one because we know >=2 implementations are coming. ``centroid``
(faithful VALENCIA) is the first plugin and the validated baseline; alternatives register
behind the same interface (P5) and are always diffed against the centroid baseline.
"""

from __future__ import annotations

from typing import Callable

from microfgt.cst.centroid import CST_ORDER, classify_centroid, load_reference_centroids

_METHODS: dict[str, Callable] = {}


def register_method(name: str, fn: Callable) -> None:
    """Register a CST method behind the ``classify_cst`` interface."""
    _METHODS[name] = fn


def available_methods() -> list[str]:
    return sorted(_METHODS)


def classify_cst(composition, method: str = "centroid", **kwargs):
    """Assign samples to CSTs from their taxonomic composition.

    Parameters
    ----------
    composition:
        The ``composition`` modality (AnnData) or a samples x taxa DataFrame.
    method:
        CST method name (default ``"centroid"`` — faithful VALENCIA, the baseline).
    **kwargs:
        Passed through to the method (e.g. ``reference``, ``read_count`` for centroid).

    Returns
    -------
    pandas.DataFrame indexed by sample with ``<subCST>_sim`` columns, ``subCST``, ``score``,
    ``CST`` — ready to attach as sample-level annotation via ``build_mudata(cst=...)``.
    """
    if method not in _METHODS:
        raise ValueError(
            f"Unknown CST method {method!r}; available: {available_methods()}"
        )
    return _METHODS[method](composition, **kwargs)


register_method("centroid", classify_centroid)

__all__ = [
    "classify_cst",
    "classify_centroid",
    "load_reference_centroids",
    "register_method",
    "available_methods",
    "CST_ORDER",
]
