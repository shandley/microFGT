"""Orchestration layer (optional) — actually run the external tools.

The user can ingest existing tool outputs (the importers) OR have microFGT run the tools.
microFGT does NOT bundle the tools or their large reference data; it locates an installed
tool (configurable path / PATH), runs it with recorded provenance, and hands the output to
the matching importer.
"""

from microfgt.orchestrate._run import (
    RunRecord,
    ToolNotFoundError,
    resolve_executable,
    run_command,
)
from microfgt.orchestrate.speciateit import run_speciateit
from microfgt.orchestrate.virgo import run_virgo, run_virgo_samples

__all__ = [
    "RunRecord",
    "ToolNotFoundError",
    "resolve_executable",
    "run_command",
    "run_speciateit",
    "run_virgo",
    "run_virgo_samples",
]
