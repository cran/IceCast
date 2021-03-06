% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mappings.R
\name{create_mapping}
\alias{create_mapping}
\title{Map a set of observations and predictions}
\usage{
create_mapping(start_year, end_year, obs_start_year, pred_start_year,
  observed, predicted, reg_info, month, level, dat_type_obs, dat_type_pred,
  plotting = FALSE, obs_only = FALSE, pred_only = FALSE, nX = 304,
  nY = 448, xmn = -3850, xmx = 3750, ymn = -5350, ymx = 5850)
}
\arguments{
\item{start_year}{first year to be mapped}

\item{end_year}{last year to be mapped}

\item{obs_start_year}{year in which observation array starts}

\item{pred_start_year}{year in which prediction array starts}

\item{observed}{array of observed values of dimension year x longitude x
latitude}

\item{predicted}{array of predicted values of dimension year x longitude x
latitude}

\item{reg_info}{a \code{reg_info} list (see documentation for \code{reg_info})}

\item{month}{month under consideration}

\item{level}{concentration level for which to build contour}

\item{dat_type_obs}{string of either "bootstrap" or "simple" indicating the
file type of the observation (see details)}

\item{dat_type_pred}{string of either "gfdl" or "simple" indicating the file
type of the prediction (see details)}

\item{plotting}{boolean indicatng whether maps should be plotted (defaults to
false)}

\item{obs_only}{indicator to run mapping only for observations}

\item{pred_only}{indicator to run mapping only for predictions}

\item{nX}{dimension in the x (defaults to value for Northern Polar
stereographic grid: 304)}

\item{nY}{dimension in the y (defaults to value for Northern Polar
stereographic grid: 448)}

\item{xmn}{min x value (defaults to value for Northern Polar stereographic
grid: -3850)}

\item{xmx}{max x value (defaults to value for Northern Polar
stereographic grid: 3750)}

\item{ymn}{min y value (defaults to value for Northern Polar
stereographic grid: -5350)}

\item{ymx}{max y value (defaults to value for Northern Polar
stereographic grid: 5850)}
}
\value{
\code{map} object (see details)
}
\description{
Finds all the mappings for a set of observations and predictions often over
multiple years
}
\details{
The object \code{maps} is obtained from running the
         \code{create_mapping} function. It is a list of four objects. The
         first two items in the list, \code{start_year} and \code{end_year},
         give the first and last year that were mapped. The second two items,
         \code{obs_list} and \code{pred_list}, are lists of arrays with one
         3-dimensional array for each region. The first dimension is for the
         year. The other two dimensions are for the fixed points'
         y-coordinates, the mapped points' x-coordinates, the mapped points'
         y-coordinates, the length of the mapping vectors in the x-direction,
         the length of the vectors in the y-direction, and the angles of the
         mapping vectors.


        For \code{dat_type_obs = "simple"} and \code{dat_type_pred = "simple"}
        the values in the \code{observed} and \code{predicted} arrays are
        indicators of whether the grid box contains ice (1: ice-covered,
        0: no ice, NA: land). If \code{datTypePred = "gfdl"} or
        \code{dat_type_obs = "bootstrap"}, the values in the \code{observed}
        and \code{predicted} arrays correspond to the raw ice concentrations
        values observed or predicted (including indicators for missing data,
        land etc.). If \code{datTypePred = "gfdl"}, the predictions are
        formatted as in the CM2.5 Forecast-oriented Low-Ocean Resolution
        (FLOR) model produced by the National Oceanic and Atmospheric
        Administration’s Geophysical Fluid Dynamics Laboratory and converted
        to a Polar Stereographic grid (Vecchi et al. 2014; Msadek et al. 2014).
        If \code{datTypeObs = "bootstrap"} the array values are assumed to be
        from the monthly sea ice concentration  obtained from the National
        Aeronautics and Space Administration (NASA) satellites Nimbus-7 SMMR
        and DMSP SSM/I-SSMIS and processed by the bootstrap algorithm.
        Weights for converting to a polar stereograhic grid were obtained
        from the spherical coordinate remapping and interpolation package
        (SCRIP) (Jones 1997).
}
\examples{
\dontrun{
create_mapping(start_year = 1981, end_year = 1981, obs_start_year = 1981,
               pred_start_year = 1980, observed = obsFeb19811982,
               predicted = emFeb19811982, reg_info = reg_info, month = 2,
               level = 15, dat_type_obs = "bootstrap", dat_type_pred = "gfdl",
               plotting = TRUE) }
}
\references{
Comiso, J., 2017: Bootstrap sea ice concentrations
            from Nimbus-7 SMMR and DMSP SSM/I-SSMIS. version 3.
            Boulder, Colorado USA: NASA National Snow and Ice Data Center
            Distributed Active Archive Center.
            doi: \url{https://doi.org/10.5067/7Q8HCCWS4I0R}

           CM2.5 Forecast-oriented Low-Ocean Resolution (FLOR) model:
           Vecchi, Gabriel A., et al.
           \href{http://journals.ametsoc.org/doi/abs/10.1175/JCLI-D-14-00158.1}{"On the seasonal forecasting of regional tropical}
           cyclone activity." Journal of Climate 27.21 (2014): 7994-8016.

           Msadek, R., et al.
          \href{http://onlinelibrary.wiley.com/doi/10.1002/2014GL060799/full}{"Importance of initial conditions in seasonal predictions of
           Arctic sea ice extent."} Geophysical Research Letters
           41.14 (2014): 5208-5215.

          National Center for Atmospheric Research, 2017: Earth system grid
          at NCAR. \url{https://www.earthsystemgrid.org/home.html}.

          Jones, P.W. "A user’s guide for SCRIP: A spherical coordinate
          remapping and interpolation package." Los Alamos National
          Laboratory, Los Alamos, NM (1997).
}
