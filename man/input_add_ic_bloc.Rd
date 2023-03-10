% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_add_ic_bloc.R
\name{input_add_ic_bloc}
\alias{input_add_ic_bloc}
\title{input_add_ic_bloc}
\usage{
input_add_ic_bloc(
  x = list(),
  ic_name = NULL,
  COMP_NAME = NULL,
  DIS_TYPE = NULL,
  GEO_TYPE = NULL,
  PCS_TYPE = NULL,
  PRIMARY_VARIABLE = NULL
)
}
\arguments{
\item{x}{Simulation object of class \emph{ogs5}.}

\item{ic_name}{Name of the initial condition.}

\item{COMP_NAME}{Name of the component.}

\item{DIS_TYPE}{Distribution type of intial condition.}

\item{GEO_TYPE}{Name of a geometry defined in \strong{gml} to be apply the IC on.}

\item{PCS_TYPE}{Apply IC for the process defined in \strong{pcs}.}

\item{PRIMARY_VARIABLE}{Apply IC for the primary variable defined in \strong{pcs}.}
}
\value{
Updated \emph{ogs5} object.
}
\description{
Adds a sub-bloc to \strong{ic} bloc of \emph{ogs5} for defining an initial
condition (IC). For additional documentatoin of the input parameters see
the \href{https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/ic.html}{ogs5 keyword docs}
or have a look at the input scripts from the \href{https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks}{r2ogs5-benchmarks repository}.
All arguments except \strong{x} have to be of class \emph{character}.
}
\examples{
tmp <- tempdir()
ogs5_obj <- create_ogs5(sim_name = "ex1", sim_id = 1L,
                        sim_path = paste0(tmp, "/ex1"))

ogs5_obj <- input_add_ic_bloc(ogs5_obj, ic_name = "INITIAL_CONDITION1",
                              PCS_TYPE = "GROUNDWATER_FLOW",
                              PRIMARY_VARIABLE = "HEAD",
                              GEO_TYPE = "DOMAIN",
                              DIS_TYPE = "CONSTANT 1.0")
}
