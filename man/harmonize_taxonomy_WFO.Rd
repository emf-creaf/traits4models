% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harmonize_taxonomy_WFO.R
\name{harmonize_taxonomy_WFO}
\alias{harmonize_taxonomy_WFO}
\title{Harmonizes taxonomy}
\usage{
harmonize_taxonomy_WFO(db, WFO_backbone_file, progress = TRUE, verbose = FALSE)
}
\arguments{
\item{db}{Data frame to harmonize, with species names in a column called 'originalName'. The data frame will normally
include other columns which are transferred unaltered to the output data frame.}

\item{WFO_backbone_file}{Path to file containing the backbone of World Flora Online.}

\item{progress}{A boolean flag to prompt progress.}

\item{verbose}{A boolean flag to print console output with matching information.}
}
\value{
A data frame with columns:
 \itemize{
   \item{\code{originalName}: Original taxon name given in the input data frame.}
   \item{\code{acceptedName}: Accepted taxon name according to World Flora Online.}
   \item{\code{acceptedNameAuthorship}: Accepted taxon name with authorship according to World Flora Online.}
   \item{\code{family}: Taxonomic family of the accepted taxon.}
   \item{\code{genus}: Taxonomic genus of the accepted taxon.}
   \item{\code{specificEpithet}: Specific epithet of the accepted taxon.}
   \item{\code{taxonRank}: Taxonomic rank of the accepted taxon (e.g. "species", "subspecies", "genus", ...).}
 }
 Additional columns may be present, coming from the input data frame. These are left unmodified.
}
\description{
Harmonizes plant taxonomy according to World Flora Online
}
