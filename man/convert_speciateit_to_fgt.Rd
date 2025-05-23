% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mock_data.R
\name{convert_speciateit_to_fgt}
\alias{convert_speciateit_to_fgt}
\title{Convert mock SpeciateIT results to FGTMicrobiome format}
\usage{
convert_speciateit_to_fgt(speciateit_results)
}
\arguments{
\item{speciateit_results}{Data frame from \code{generate_mock_speciateit()}}
}
\value{
A list with an \code{assignments} component containing Feature and Species columns
}
\description{
Converts the output from \code{generate_mock_speciateit()} to a format compatible
with the \code{add_speciateit()} function.
}
\examples{
\donttest{
# Generate mock data
mock_data <- generate_mock_speciateit(n_sequences = 20)

# Convert to format for add_speciateit()
fgt_compatible <- convert_speciateit_to_fgt(mock_data)
}
}
