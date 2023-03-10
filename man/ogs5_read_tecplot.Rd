% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ogs5_read_output_tec.R
\name{ogs5_read_tecplot}
\alias{ogs5_read_tecplot}
\title{ogs5_read_tecplot}
\usage{
ogs5_read_tecplot(filepath = character(), geo_object = character())
}
\arguments{
\item{filepath}{\emph{character} Path to *\strong{.tec} file.}

\item{geo_object}{\emph{character} Associated geometry type: c("domain", "POINT", "POLYLINE",
"SURFACE").}
}
\value{
Dataframe.
}
\description{
Wrapper to read *\strong{.tec} file. Calls
\code{\link[=ogs5_read_tecplot_domain]{ogs5_read_tecplot_domain()}}, \code{\link[=ogs5_read_tecplot_polyline]{ogs5_read_tecplot_polyline()}},
\code{\link[=ogs5_read_tecplot_point]{ogs5_read_tecplot_point()}}.
}
