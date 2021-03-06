% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genContours.R
\name{prob_map}
\alias{prob_map}
\title{Get probabilities on a grid from contours}
\usage{
prob_map(merged, nX = 304, nY = 448)
}
\arguments{
\item{merged}{list of contours organized as a list of years by a list of
samples}

\item{nX}{dimension in the x (defaults to value for Northern Polar
stereographic grid: 304)}

\item{nY}{dimension in the y (defaults to value for Northern Polar
stereographic grid: 448)}
}
\value{
array of dimension number of years by longitude by latitude that
gives the proportion of contours in which the grid box is ice-covered
}
\description{
Takes in list of polygon objects from merged function and produces a
map of probabilities
}
\examples{
\dontrun{ probs <- prob_map(merged) }

}
