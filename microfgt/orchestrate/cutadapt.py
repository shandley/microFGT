"""Orchestrate cutadapt — primer trimming, the first 16S front-end stage.

Removes amplicon primers from paired FASTQs before DADA2. Like the other wrappers, cutadapt
is located (PATH / explicit path), run with recorded provenance, and not bundled.
"""

from __future__ import annotations

from pathlib import Path

from microfgt.orchestrate._run import resolve_executable, run_command


def discover_pairs(fastq_dir):
    """Find (sample, R1, R2) FASTQ pairs in a directory by the ``_R1``/``_R2`` convention."""
    fastq_dir = Path(fastq_dir)
    pairs = []
    for r1 in sorted(fastq_dir.glob("*_R1*.fastq*")):
        r2 = Path(str(r1).replace("_R1", "_R2"))
        sample = r1.name.split("_R1")[0]
        pairs.append((sample, r1, r2 if r2.exists() else None))
    return pairs


def run_cutadapt(
    input_dir,
    output_dir,
    *,
    fwd_primer: str | None = None,
    rev_primer: str | None = None,
    executable: str = "cutadapt",
    extra_args=None,
    timeout: float | None = None,
):
    """Trim primers from every FASTQ pair in ``input_dir`` into ``output_dir``.

    Returns one RunRecord per sample.
    """
    exe, fingerprint = resolve_executable(executable, tool="cutadapt")
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    pairs = discover_pairs(input_dir)
    if not pairs:
        raise FileNotFoundError(f"No '*_R1*.fastq*' files found in {input_dir}.")

    records = []
    for sample, r1, r2 in pairs:
        argv = [exe]
        if fwd_primer:
            argv += ["-g", fwd_primer]
        if r2 and rev_primer:
            argv += ["-G", rev_primer]
        argv += ["-o", str(output_dir / r1.name)]
        if r2:
            argv += ["-p", str(output_dir / r2.name)]
        argv += list(extra_args or [])
        argv += [str(r1)] + ([str(r2)] if r2 else [])
        records.append(
            run_command(
                argv, tool="cutadapt", params={"sample": sample},
                exe_fingerprint=fingerprint, timeout=timeout,
            )
        )
    return records
