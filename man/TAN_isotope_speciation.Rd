% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TAN_speciation.R
\name{TAN_isotope_speciation}
\alias{TAN_isotope_speciation}
\title{TAN Isotope Speciation Function}
\usage{
TAN_isotope_speciation(TAN, isoTAN, pH)
}
\arguments{
\item{TAN}{The TAN concentration.}

\item{isoTAN}{The isoTAN value (TAN isotopic ratio * TAN concentration)}

\item{pH}{The pH.}
}
\description{
This function takes TAN concentration and pH and calculates/returns the iso values of NH3 and NH4+ as a vector.
}
\examples{
TAN_isotope_speciation(TAN, isoTAN, pH)
}
\keyword{NANNO}
