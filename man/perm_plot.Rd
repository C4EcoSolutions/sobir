% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perm_plot.R
\name{perm_plot}
\alias{perm_plot}
\title{Perm Plot}
\usage{
perm_plot(perm, histogram = TRUE)
}
\arguments{
\item{perm}{a perm object generated by the perm_area() function.}

\item{histogram}{TRUE or FALSE. TRUE plots histograms while FALSE plots density plots.}
}
\value{
a ggplot2 histogram and p-value for each no-data zone
}
\description{
perm_plot calculates the significance of the observed no-data zones and plots them relative to the simulations.
}
\examples{
a = rnorm(100,0,1)
b = rnorm(100,0,1)
permExample = perm_area(a,b,10)
perm_plot(permExample, 100)
}
