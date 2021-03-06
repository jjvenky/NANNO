% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recreate_NANNO_fit.R
\name{recreate_NANNO_fit}
\alias{recreate_NANNO_fit}
\title{NANNO Inputs Function}
\usage{
recreate_NANNO_fit(NANNOmodel, filename)
}
\arguments{
\item{NANNOmodel}{S4 object of the NANNO model with fitted parameters (Formal class odeModel)}

\item{filename}{The base name of the bounds file to be used.}
}
\description{
This function takes inputs for lower and upper bounds from a csv file and converts them for the fitting routine
}
\examples{
recreate_NANNO_fit(NANNOmodel, filename)
}
\keyword{NANNO}
