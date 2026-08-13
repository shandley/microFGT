"""The uniform return type for every analysis *verb* — the contract the dashboard binds to.

A statistical analysis in microFGT is ``(object + which obs variables play which role + a
subset) -> AnalysisResult``. Keeping the return shape uniform is what lets Layer 3 (the
dashboard) be a *thin presenter*: it builds a spec from the user's dropdowns, calls the same
verb a power user calls, and renders ``.table`` + ``.plot`` — no separate "dashboard logic".

An ``AnalysisResult`` carries:

* ``table``  — the tidy per-term / per-feature stats (what you'd paste into a paper)
* ``stats``  — the headline numbers (test name, statistic, p-value, effect size, model)
* ``spec``   — exactly what was asked (outcome / predictors / subject / subset / modality)
* ``plot``   — a *declarative* plot spec (kind + which fields), rendered by ``viz`` or the app
* ``notes``  — honest bookkeeping (n used, n dropped for missing/zero-count, warnings)

It is deliberately plain data (JSON-friendly) so it can cross a web boundary unchanged.
"""

from __future__ import annotations

from dataclasses import dataclass, field

import pandas as pd


@dataclass
class AnalysisResult:
    """One analysis verb's result: a tidy table + headline stats + a declarative plot spec."""

    verb: str
    table: pd.DataFrame
    stats: dict = field(default_factory=dict)
    spec: dict = field(default_factory=dict)
    plot: dict = field(default_factory=dict)
    notes: dict = field(default_factory=dict)

    @property
    def pvalue(self):
        """The headline p-value, if the verb produced one."""
        return self.stats.get("pvalue")

    def summary(self) -> str:
        """One-line human summary (what the app would show as a caption)."""
        bits = [self.verb]
        if self.stats.get("test"):
            bits.append(str(self.stats["test"]))
        preds = self.spec.get("predictors")
        if preds:
            bits.append("~ " + " + ".join(preds))
        if self.pvalue is not None:
            bits.append(f"p={self.pvalue:.3g}")
        for k in ("effect_size", "R2", "pseudo_F"):
            if k in self.stats and self.stats[k] is not None:
                bits.append(f"{k}={self.stats[k]:.3g}")
        n = self.notes.get("n_used")
        if n is not None:
            bits.append(f"n={n}")
        return " | ".join(bits)

    def to_dict(self) -> dict:
        """JSON-friendly view (``table`` becomes records) for a web boundary / provenance."""
        return {
            "verb": self.verb,
            "table": self.table.reset_index().to_dict(orient="list"),
            "stats": self.stats,
            "spec": self.spec,
            "plot": self.plot,
            "notes": self.notes,
        }
