"""Analysis layer — commodity stats bought from scikit-bio, operating on the modalities.

The FGT-specific value is in the import + CST layers; diversity/transforms/ordination/
differential-abundance are validated, commodity wheels we lean on rather than reinvent.
"""

from microfgt.analysis.diffabund import differential_abundance
from microfgt.analysis.diversity import alpha_diversity, beta_diversity, ordinate
from microfgt.analysis.transforms import clr_transform, relative_abundance

__all__ = [
    "relative_abundance",
    "clr_transform",
    "alpha_diversity",
    "beta_diversity",
    "ordinate",
    "differential_abundance",
]
