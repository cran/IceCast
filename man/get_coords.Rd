% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{get_coords}
\alias{get_coords}
\title{Extract coordinates from a spatial object of lines and points}
\usage{
get_coords(my_points)
}
\arguments{
\item{my_points}{spatial object of type \code{SpatialCollections},
\code{SpatialPoints}, or \code{SpatialLines}}
}
\value{
n x 2 matrix of coordinates
}
\description{
Get coordinates from a spatial object of lines and points. There is no
ordering of points returned. Note: This differs from \code{extract_coords} in
that the ordering of the points is NOT considered.
}
\examples{
#Load sample line
ex_line <- as(rm_holes(bg_water[2]), "SpatialLines")
get_coords(ex_line)
}
