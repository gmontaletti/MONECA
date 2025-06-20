% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptive_functions.R
\name{print.descriptive.moneca}
\alias{print.descriptive.moneca}
\title{Print Method for Descriptive MONECA Statistics}
\usage{
\method{print}{descriptive.moneca}(x, ...)
}
\arguments{
\item{x}{A descriptive.moneca object containing calculated statistics.}

\item{...}{Additional arguments (currently unused).}
}
\value{
NULL (called for side effects - printing formatted output).
}
\description{
Internal function that formats and displays the detailed statistics calculated
by the \code{print.moneca} method. This function creates formatted tables showing
network properties and mobility statistics across hierarchical levels.
}
\details{
This internal function is responsible for the formatted display of MONECA
analysis results. It creates several summary tables:
\itemize{
  \item Degree distributions (all, in, out) for each level
  \item Edge weight distributions for each level
  \item Diagonal mobility percentages
  \item Network density and connectivity statistics
}
}
\keyword{internal}
