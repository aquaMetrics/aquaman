% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iqi.R
\name{iqi}
\alias{iqi}
\title{Predict IQI score}
\usage{
iqi(predictors)
}
\arguments{
\item{predictors}{Data frame of family-level bacteria taxa.}
}
\value{
dataframe of IQI scores
}
\description{
WORK IN PROGRESS - DO NOT USE IN PRODUCTION - Using family-level bacteria
taxa as predictors, calculate benthic invert IQI as an outcome. Model created
by Tom Wilding (SAMS), based on training data from SEPA And MOWI.
}
\examples{
\dontrun{
iqi_scores <- iqi(demo_taxa)
}
}
