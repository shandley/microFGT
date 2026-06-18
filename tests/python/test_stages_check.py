"""`microfgt check` — preflight doctor over the resolved entry point."""

import os
import stat

from microfgt.stages import check


def _make_exe(path, body="#!/usr/bin/env bash\nexit 0\n"):
    path.write_text(body)
    path.chmod(path.stat().st_mode | stat.S_IEXEC | stat.S_IXGRP | stat.S_IXOTH)
    return path


def test_existing_outputs_entry_needs_no_external_tools():
    cfg = {"composition": {"speciateit": {"results": "r.txt", "count_table": "c.csv"}}}
    results = check(cfg)
    assert all(r.ok for r in results)           # those stages have no requirements


def test_missing_cutadapt_reported_for_fastq_entry(tmp_path):
    cfg = {"composition": {"reads": {"fastq_dir": "raw/", "cutadapt": "definitely_missing_xyz"},
                           "speciateit": {"db": str(tmp_path)}}}
    results = check(cfg)
    miss = [r for r in results if not r.ok]
    assert any("definitely_missing_xyz" in r.message for r in miss)


def test_region_db_mismatch_flagged(tmp_path):
    db = tmp_path / "vSpeciateIT_V4V4"           # V4 model...
    db.mkdir()
    classify = _make_exe(tmp_path / "classify")
    cfg = {"composition": {
        "reads": {"fastq_dir": "raw/", "region": "V3V4",   # ...but config says V3V4
                  "cutadapt": str(_make_exe(tmp_path / "cutadapt")),
                  "rscript": str(_make_exe(tmp_path / "Rscript"))},
        "speciateit": {"db": str(db), "classify": str(classify)},
    }}
    results = check(cfg)
    assert any("does not match" in r.message for r in results if not r.ok)
