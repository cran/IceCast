% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mappings.R
\name{get_map}
\alias{get_map}
\title{Map one observation or prediction}
\usage{
get_map(ice, reg_info, plotting = FALSE, main = "", my_land = land)
}
\arguments{
\item{ice}{\code{SpatialPolygons} object corresponding to the region of ice}

\item{reg_info}{\code{reg_info} list (see \code{reg_info} documentation)}

\item{plotting}{boolean indicating if map should be plotted}

\item{main}{string specifying the name for the plot}

\item{my_land}{\code{SpatialPolygons} object corresponding to the land}
}
\value{
List of the length of the number of regions. Each item in the list is
        a matrix. Each row of each matrix corresponds to a point in the
        region's line. The six columns give the fixed point's x-coordinate,
        the fixed point's y-coordinate, the mapped point's x-coordinate, the
        mapped point's y-coordinate, the length of the mapping fvectors in the
        x-direction, and the length of the vectors in the y-direction.
}
\description{
Find the mapping vectors for one observation or prediction.
}
\examples{
\dontrun{
obs <- get_region(dat = obsFeb19811982[1,,], dat_type = "bootstrap",
               level = 15)
obs_map <- get_map(ice = obs, plotting = TRUE, reg_info,
                 main = "Observed Mapping \\n February 1985")
}
}
