% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readBootstrap.R
\name{read_monthly_BS}
\alias{read_monthly_BS}
\title{Read in a set of bootstrap observations over a set of year}
\usage{
read_monthly_BS(start_year, end_year, file_folder, version, nX = 304,
  nY = 448)
}
\arguments{
\item{start_year}{first year to read in}

\item{end_year}{last year to read in}

\item{file_folder}{folder in which binary files are stored}

\item{version}{either 2 or 3 indicating which version of the bootstrap data
you are using}

\item{nX}{longitude dimension}

\item{nY}{latitude dimension}
}
\value{
bootstrap observations sorted into array of dimension: year x month x
        lon x lat
}
\description{
Function to process monthly bootstrap data over multiple years. The
observations are from the monthly sea ice concentration obtained from the
National Aeronautics and Space Administration (NASA) satellites Nimbus-7
SMMR and DMSP SSM/I-SSMIS and processed by the bootstrap algorithm. The
resultsare distributed by the National Snow and Ice Data Center (NSIDC)
(Comiso 2017). Functions assume file name conventions are the same as used
by NSIDC.
}
\details{
raw binary files for 2012-2013 are included in the package as an
         example
}
\examples{
\dontrun{
#my_file_path should be a file path where the 1983 binary files are stored
observed_demo <- read_monthly_BS(start_year = 1983, end_year = 1983,
                             file_folder = my_file_path)
}
}
\references{
Bootstrap sea ice concentration:
            Comiso, J., 2017: Bootstrap sea ice concentrations
            from Nimbus-7 SMMR and DMSP SSM/I-SSMIS. version 3.
            {Boulder, Colorado USA: NASA National Snow and Ice Data Center
            Distributed Active Archive Center}
}
