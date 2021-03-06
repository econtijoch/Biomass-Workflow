% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ELISA_Analysis.R
\name{predictor}
\alias{predictor}
\title{Helper function for IgA Elisa data generation from standard curve parameters}
\usage{
predictor(params, y)
}
\arguments{
\item{params}{parameters for model from nls fit}

\item{y}{data to be fit}
}
\value{
estimates of IgA based on OD measurements from the fitted model
}
\description{
Helper function for IgA Elisa data generation from standard curve parameters
}
