"""Import layer — real tool outputs in, integrated object out.

Each importer does only the FGT-specific reshape (the "glue" the user should never
have to write); the container is a generic ``anndata.AnnData`` / ``mudata.MuData``.
Formats are grounded in ``prototype/real_fixtures/FORMATS.md`` and the tools' own code,
NOT guessed.
"""

from microfgt.io.integrate import build_mudata
from microfgt.io.speciateit import collapse_to_taxon, import_speciateit
from microfgt.io.valencia import import_valencia
from microfgt.io.virgo import import_virgo

__all__ = [
    "build_mudata",
    "collapse_to_taxon",
    "import_speciateit",
    "import_valencia",
    "import_virgo",
]
