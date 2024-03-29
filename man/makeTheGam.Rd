% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gam.R
\name{makeTheGam}
\alias{makeTheGam}
\title{makeTheGam}
\usage{
makeTheGam(Data, dates = NULL, zetas = 0.5)
}
\arguments{
\item{Data}{drift}

\item{dates}{dates}

\item{zetas}{zetas}
}
\value{
a list with predicted values
}
\description{
function for fitting a gam with a custom link function
}
\details{
This function fit a gam with a custom link function. It allows to
get estimates of Drift rate at any given time, provided that the
seal is drifting. This function may break the drifting trajectory
into sub-units on the basis of lack of Drift dives.
}
