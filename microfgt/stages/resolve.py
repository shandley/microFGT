"""The resolver — make-semantics over the stage registry.

Given the target artifact and the artifacts already present (the entry point), return the
ordered list of stages to run, skipping any whose output is already supplied. This one
mechanism IS the multi-entry-point ladder: FASTQs, ASV table, existing outputs, or a
composition all "just work" by entering wherever the provided inputs land.
"""

from __future__ import annotations

from microfgt.stages.model import Stage
from microfgt.stages.registry import PRODUCERS, STAGES


class StageResolutionError(Exception):
    """No registered stage can produce a needed artifact from the provided inputs."""


def _choose_producer(artifact: str, provided: set[str]) -> Stage | None:
    candidates = PRODUCERS.get(artifact, [])
    if not candidates:
        return None
    # Prefer a producer whose input is directly provided (e.g. cst_valencia when a VALENCIA
    # output is supplied; otherwise cst_classify from composition).
    for stage in candidates:
        if any(i in provided for i in stage.inputs):
            return stage
    return candidates[0]


def resolve(target: str, provided) -> list[Stage]:
    """Stages needed to produce ``target`` given ``provided`` artifact keys, in run order."""
    provided = set(provided)
    order: list[Stage] = []
    placed: set[str] = set()

    def need(artifact: str) -> None:
        if artifact in provided:
            return
        stage = _choose_producer(artifact, provided)
        if stage is None:
            raise StageResolutionError(
                f"Cannot produce artifact {artifact!r} from the provided inputs "
                f"{sorted(provided)}. Supply it directly or an upstream input."
            )
        if stage.id in placed:
            return
        for inp in stage.inputs:
            need(inp)
        if stage.id not in placed:           # re-check: deps may have placed nothing new
            placed.add(stage.id)
            order.append(stage)

    need(target)
    return order


def all_stage_ids() -> list[str]:
    return [s.id for s in STAGES]
