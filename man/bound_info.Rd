% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitContours.R
\name{bound_info}
\alias{bound_info}
\title{Get boundary info}
\usage{
bound_info(y_obs, dist, loop)
}
\arguments{
\item{y_obs}{matrix of observed distances (dimension: number of lines
by number of years)}

\item{dist}{a list of the lengths for the corresponding \code{lines}}

\item{loop}{boolean which if true \code{TRUE} indicates that the \code{lines}
extend outward from a single point forming a circle and if
\code{FALSE} indicates that the lines are mapped along a fixed
contour such as a land boundary}
}
\value{
list of 3 matrices each of dimension number of lines by number of
        years giving the lower bound for hidden x values, the upper bound
        for hidden x values, and an indicator of whether the
        value is bounded at all. The values in the list are named
        \code{ub}, \code{lb}, and \code{xUnObs} respectively.
}
\description{
Determine which y values are on the boundaries and what the corresponding
bounds of those y values are
}
