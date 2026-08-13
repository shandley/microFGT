"""The 16S stage registry — the one place the workflow DAG is defined.

Both executors and ``microfgt check`` consume this. Stage run functions reuse the existing
importers / CST / analysis / orchestration code, so there is no logic duplicated between the
in-memory API and the file-artifact pipeline — only the orchestration differs.

Metagenomic (QC/host-removal -> VIRGO) front-end mirrors this and is the next pass.
"""

from __future__ import annotations

from microfgt.stages.model import Requirement, Stage, StageContext, artifact_paths

# Region-aware DADA2 defaults, keyed to speciateIT's own model regions. Starting points
# only — truncation genuinely depends on run quality, so these are overridable and the
# quality profile is emitted to inform overrides.
REGION_DEFAULTS = {
    "V1V3": {"trunc_len": [0, 0], "trim_left": [0, 0]},
    "V3V4": {"trunc_len": [280, 230], "trim_left": [17, 21]},
    "V4": {"trunc_len": [230, 180], "trim_left": [19, 20]},
}


# --- config -> provided artifacts (the entry point is just which inputs are present) -------
def provided_artifacts(config: dict) -> dict[str, str]:
    provided: dict[str, str] = {}
    comp = config.get("composition", {})
    if "reads" in comp and comp["reads"].get("fastq_dir"):
        provided["fastq_dir"] = comp["reads"]["fastq_dir"]
    if comp.get("asv_table"):
        provided["asv_table"] = comp["asv_table"]
    if comp.get("asv_seqs"):
        provided["asv_seqs"] = comp["asv_seqs"]
    if comp.get("composition_h5ad"):
        provided["composition"] = comp["composition_h5ad"]
    sit = comp.get("speciateit", {})
    if sit.get("results"):
        provided["speciateit_results"] = sit["results"]
    if sit.get("count_table"):
        provided["asv_table"] = sit["count_table"]
    cst_cfg = config.get("cst", {})
    if comp.get("phyloseq"):
        provided["phyloseq_rds"] = comp["phyloseq"]
        # A phyloseq entry supplies its own CST by default; an explicit cst source
        # (method / valencia) overrides it, so only offer phyloseq as the CST producer
        # when neither is set.
        if not cst_cfg.get("method") and not cst_cfg.get("valencia"):
            provided["phyloseq_cst"] = comp["phyloseq"]
    if cst_cfg.get("valencia"):
        provided["valencia_output"] = cst_cfg["valencia"]
    return provided


# --- stage run functions (reuse existing code) ---------------------------------------------
def _run_primer_trim(ctx: StageContext) -> None:
    from microfgt.orchestrate.cutadapt import run_cutadapt

    reads = ctx.config.get("composition", {}).get("reads", {})
    primers = reads.get("primers", {})
    run_cutadapt(
        ctx.path("fastq_dir"), ctx.path("trimmed_reads"),
        fwd_primer=primers.get("fwd"), rev_primer=primers.get("rev"),
        executable=reads.get("cutadapt", "cutadapt"),
    )


def _run_denoise(ctx: StageContext) -> None:
    from microfgt.orchestrate.dada2 import run_dada2

    reads = ctx.config.get("composition", {}).get("reads", {})
    defaults = REGION_DEFAULTS.get(reads.get("region"), {})
    d = reads.get("dada2", {})
    run_dada2(
        ctx.path("trimmed_reads"), ctx.path("asv_table"), ctx.path("asv_seqs"),
        ctx.path("quality_profile"),
        trunc_len=d.get("trunc_len", defaults.get("trunc_len")),
        trim_left=d.get("trim_left", defaults.get("trim_left")),
        rscript=reads.get("rscript", "Rscript"),
    )


def _run_assign(ctx: StageContext) -> None:
    from microfgt.orchestrate import run_speciateit

    sit = ctx.config.get("composition", {}).get("speciateit", {})
    run_speciateit(
        fasta=ctx.path("asv_seqs"), db=sit["db"],
        outdir=ctx.path("speciateit_results").parent,
        executable=sit.get("classify", "classify"),
        skip_err_thld=sit.get("skip_err_thld", False),
    )


def _run_import_composition(ctx: StageContext) -> None:
    from microfgt.io import import_speciateit

    # Carry ASV sequences when the FASTA is available (dada2-emitted or user-provided).
    asv_seqs = ctx.path("asv_seqs")
    adata = import_speciateit(
        ctx.path("speciateit_results"), ctx.path("asv_table"),
        fasta=asv_seqs if asv_seqs.exists() else None,
    )
    adata.write(ctx.path("composition"))


def _run_import_phyloseq(ctx: StageContext) -> None:
    from microfgt.io import import_phyloseq

    rscript = ctx.config.get("composition", {}).get("phyloseq_rscript", "Rscript")
    adata = import_phyloseq(ctx.path("phyloseq_rds"), rscript=rscript)
    adata.write(ctx.path("composition"))


def _run_cst_phyloseq(ctx: StageContext) -> None:
    import anndata as ad

    from microfgt.io import existing_cst

    # The composition h5ad written by import_phyloseq already carries CST in its obs;
    # pull it out rather than re-reading the .rds.
    adata = ad.read_h5ad(ctx.path("composition"))
    cst = existing_cst(adata)
    if cst is None:
        raise ValueError(
            "cst source is the phyloseq object, but it carried no CST/subCST/score. "
            "Set cst.method to classify from the composition instead."
        )
    cst.to_csv(ctx.path("cst"))


def _run_cst_classify(ctx: StageContext) -> None:
    import anndata as ad

    from microfgt.cst import classify_cst
    from microfgt.io import collapse_to_taxon

    adata = ad.read_h5ad(ctx.path("composition"))
    # CST reads the taxon roll-up; collapse ASV-grain composition first.
    taxon = collapse_to_taxon(adata) if "classification" in adata.var else adata
    method = ctx.config.get("cst", {}).get("method", "centroid")
    classify_cst(taxon, method=method).to_csv(ctx.path("cst"))


def _run_cst_valencia(ctx: StageContext) -> None:
    from microfgt.io import import_valencia

    import_valencia(ctx.path("valencia_output")).to_csv(ctx.path("cst"))


def _run_integrate(ctx: StageContext) -> None:
    import anndata as ad
    import pandas as pd

    from microfgt.io import build_mudata, import_virgo
    from microfgt.workflow import apply_analysis

    comp = ad.read_h5ad(ctx.path("composition"))
    cst = pd.read_csv(ctx.path("cst"), index_col=0)
    cst.index = cst.index.astype(str)

    func = None
    fcfg = ctx.config.get("function", {})
    if "virgo" in fcfg:
        func = import_virgo(fcfg["virgo"]["dir"])

    if ctx.config.get("analysis"):
        apply_analysis(comp, ctx.config["analysis"])

    build_mudata(composition=comp, function=func, cst=cst).write(ctx.path("mudata"))


# --- requirements (entry-dependent; checked by the doctor) ---------------------------------
def _req_primer_trim(cfg):
    return [Requirement("binary", cfg.get("composition", {}).get("reads", {}).get("cutadapt", "cutadapt"),
                        "install cutadapt and put it on PATH")]


def _req_denoise(cfg):
    rscript = cfg.get("composition", {}).get("reads", {}).get("rscript", "Rscript")
    return [Requirement("binary", rscript, "install R"),
            Requirement("rpackage", "dada2", "install the Bioconductor dada2 package")]


def _req_import_phyloseq(cfg):
    rscript = cfg.get("composition", {}).get("phyloseq_rscript", "Rscript")
    return [Requirement("binary", rscript, "install R"),
            Requirement("rpackage", "phyloseq", "install the Bioconductor phyloseq package")]


def _req_assign(cfg):
    sit = cfg.get("composition", {}).get("speciateit", {})
    reqs = [Requirement("binary", sit.get("classify", "classify"),
                        "install speciateIT (the classify binary)")]
    if sit.get("db"):
        reqs.append(Requirement("path", sit["db"], "path to the vSpeciateDB model directory"))
    return reqs


# --- the registry --------------------------------------------------------------------------
STAGES = [
    Stage("primer_trim", ("fastq_dir",), ("trimmed_reads",), _run_primer_trim, _req_primer_trim),
    Stage("denoise", ("trimmed_reads",), ("asv_table", "asv_seqs", "quality_profile"),
          _run_denoise, _req_denoise),
    Stage("assign", ("asv_seqs",), ("speciateit_results",), _run_assign, _req_assign),
    Stage("import_composition", ("speciateit_results", "asv_table"), ("composition",),
          _run_import_composition),
    Stage("import_phyloseq", ("phyloseq_rds",), ("composition",),
          _run_import_phyloseq, _req_import_phyloseq),
    Stage("cst_classify", ("composition",), ("cst",), _run_cst_classify),
    Stage("cst_valencia", ("valencia_output",), ("cst",), _run_cst_valencia),
    # phyloseq CST reads the composition h5ad's obs; phyloseq_cst is the selection gate
    # (present only when the phyloseq object is the chosen CST source).
    Stage("cst_phyloseq", ("composition", "phyloseq_cst"), ("cst",), _run_cst_phyloseq),
    Stage("integrate", ("composition", "cst"), ("mudata",), _run_integrate),
]
STAGE_BY_ID = {s.id: s for s in STAGES}

# artifact key -> stages that can produce it (an artifact may have >1 producer, e.g. cst).
PRODUCERS: dict[str, list[Stage]] = {}
for _s in STAGES:
    for _out in _s.outputs:
        PRODUCERS.setdefault(_out, []).append(_s)


def execute_stage(stage_id: str, workdir, config: dict, output: str | None = None) -> None:
    """Run one stage by id (used by the local executor and the ``_run-stage`` CLI alike)."""
    from pathlib import Path

    stage = STAGE_BY_ID[stage_id]
    paths = artifact_paths(workdir, config, output)
    for out_key in stage.outputs:
        Path(paths[out_key]).parent.mkdir(parents=True, exist_ok=True)
    stage.run(StageContext(Path(workdir), paths, config))
