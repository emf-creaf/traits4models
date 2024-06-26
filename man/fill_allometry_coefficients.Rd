% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fill_allometry_coefficients.R
\encoding{UTF-8}
\name{fill_allometry_coefficients}
\alias{fill_allometry_coefficients}
\title{Populate tree species allometries}
\usage{
fill_allometry_coefficients(
  SpParams,
  allom_table,
  allom_type = "foliarbiomass",
  progress = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{SpParams}{A data frame of medfate species parameters to be populated}

\item{allom_table}{A data frame of allometric parameters in columns and taxonomic entities (species or genus) as row names.}

\item{allom_type}{A string with the type of allometry to be filled, either "foliarbiomass", "barkthickness", "crownwidth" or "crownratio".}

\item{progress}{A boolean flag to prompt progress.}

\item{verbose}{A boolean flag to indicate extra console output.}
}
\value{
A modified data frame of medfate species parameters
}
\description{
Internal functions to populates allometric coefficients for tree species and genus of an input parameter table on the basis of their
accepted name.
}
\seealso{
\code{\link{init_spparams}}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
}
