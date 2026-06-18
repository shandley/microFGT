"""Minimal matplotlib helpers. Each returns an Axes so callers can compose/save."""

from __future__ import annotations

import numpy as np


def _plt():
    try:
        import matplotlib.pyplot as plt
    except ImportError as e:  # pragma: no cover - exercised only without matplotlib
        raise ImportError(
            "Plotting requires matplotlib. Install it with: pip install 'microfgt[viz]'."
        ) from e
    return plt


def ordination_scatter(adata, key: str = "X_pcoa", color: str | None = None, ax=None):
    """Scatter of the first two ordination axes, optionally colored by an ``obs`` column."""
    plt = _plt()
    coords = np.asarray(adata.obsm[key])
    if ax is None:
        _, ax = plt.subplots()
    if color is not None and color in adata.obs:
        labels = adata.obs[color].astype(str).to_numpy()
        for lab in sorted(set(labels)):
            m = labels == lab
            ax.scatter(coords[m, 0], coords[m, 1], label=lab, s=18)
        ax.legend(title=color, fontsize="small")
    else:
        ax.scatter(coords[:, 0], coords[:, 1], s=18)
    ax.set_xlabel(f"{key}[0]")
    ax.set_ylabel(f"{key}[1]")
    return ax


def alpha_boxplot(adata, alpha_key: str, group_key: str, ax=None):
    """Boxplot of an alpha-diversity ``obs`` column grouped by another ``obs`` column."""
    plt = _plt()
    if ax is None:
        _, ax = plt.subplots()
    groups = adata.obs[group_key].astype(str)
    vals = adata.obs[alpha_key].to_numpy()
    labels = sorted(set(groups))
    ax.boxplot([vals[(groups == g).to_numpy()] for g in labels])
    ax.set_xticks(range(1, len(labels) + 1))
    ax.set_xticklabels(labels)            # matplotlib 3.9+ dropped boxplot(labels=)
    ax.set_xlabel(group_key)
    ax.set_ylabel(alpha_key)
    return ax
