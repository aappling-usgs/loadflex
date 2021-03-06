% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariance.R
\name{getCormatTaoBand}
\alias{getCormatTaoBand}
\title{Same idea as getCormatCustom(rho1DayBand, dates) but runs faster.}
\usage{
getCormatTaoBand(max.tao = as.difftime(1, units = "days"))
}
\arguments{
\item{max.tao}{length of the covariance band, defaults to 1 day}
}
\value{
matrix of the covariances
}
\description{
getcormatTaoBand
}
\details{
calculate the covariance  1 if dates are within the tao band, 0 if they are
not Same idea as cormatrix(rho1DayBand, dates) but runs in linear instead of 
quadratic time - a big and much-needed improvement.
}
