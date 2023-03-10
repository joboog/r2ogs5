% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ogs5_get_output.R
\name{ogs5_get_output_all}
\alias{ogs5_get_output_all}
\title{ogs5_get_output_all}
\usage{
ogs5_get_output_all(ogs5, out_filepath = NULL)
}
\arguments{
\item{ogs5}{\emph{ogs5} simulation object.}

\item{out_filepath}{\emph{character} Path to output files. Default:
'attributes(ogs5)$sim_path'.}
}
\value{
Updated \emph{ogs5} object.
}
\description{
Import all output of a \strong{ogs5} simulation to a \emph{ogs5} object.
}
\examples{
\dontrun{
sim1 <- ogs5_get_output_all(sim1)
}
}
