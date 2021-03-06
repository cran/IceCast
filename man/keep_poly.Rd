% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{keep_poly}
\alias{keep_poly}
\title{Keep only spatial polygons}
\usage{
keep_poly(my_poly)
}
\arguments{
\item{my_poly}{\code{SpatialCollections}, \code{SpatialPolygons},
\code{SpatialPoints}, or \code{SpatialLines} object}
}
\value{
\code{SpatialPolygons} object
}
\description{
Keep only \code{SpatialPolygons} from a spatial object.
}
\examples{
par(mfrow = c(1, 2))
plot(spatialCollEx, col = "blue", main = "Spatial Collections Object")
poly_only <- keep_poly(spatialCollEx)
plot(poly_only, col = "blue", main = "Spatial Polygon Only")
}
