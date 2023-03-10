% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_add_mfp_bloc.R
\name{input_add_mfp_bloc}
\alias{input_add_mfp_bloc}
\title{input_add_mfp_bloc}
\usage{
input_add_mfp_bloc(
  x = list(),
  COMPONENTS = NULL,
  COMPRESSIBILITY = NULL,
  DAT_TYPE = NULL,
  DECAY = NULL,
  DENSITY = NULL,
  DIFFUSION = NULL,
  DRHO_DT_UNSATURATED = NULL,
  EOS_TYPE = NULL,
  FLUID_NAME = NULL,
  FLUID_TYPE = NULL,
  GRAVITY = NULL,
  HEAT_CONDUCTIVITY = NULL,
  ISOTHERM = NULL,
  JTC = NULL,
  NON_GRAVITY = NULL,
  PHASE_DIFFUSION = NULL,
  SPECIFIC_HEAT_CAPACITY = NULL,
  SPECIFIC_HEAT_SOURCE = NULL,
  TEMPERATURE = NULL,
  VISCOSITY = NULL
)
}
\arguments{
\item{x}{Simulation object of class \emph{ogs5}.}

\item{COMPONENTS}{ogs5 \strong{mfp} bloc sub key word.}

\item{COMPRESSIBILITY}{ogs5 \strong{mfp} bloc sub key word.}

\item{DAT_TYPE}{ogs5 \strong{mfp} bloc sub key word.}

\item{DECAY}{ogs5 \strong{mfp} bloc sub key word.}

\item{DENSITY}{ogs5 \strong{mfp} bloc sub key word.}

\item{DIFFUSION}{ogs5 \strong{mfp} bloc sub key word.}

\item{DRHO_DT_UNSATURATED}{ogs5 \strong{mfp} bloc sub key word.}

\item{EOS_TYPE}{ogs5 \strong{mfp} bloc sub key word.}

\item{FLUID_NAME}{ogs5 \strong{mfp} bloc sub key word.}

\item{FLUID_TYPE}{ogs5 \strong{mfp} bloc sub key word.}

\item{GRAVITY}{ogs5 \strong{mfp} bloc sub key word.}

\item{HEAT_CONDUCTIVITY}{ogs5 \strong{mfp} bloc sub key word.}

\item{ISOTHERM}{ogs5 \strong{mfp} bloc sub key word.}

\item{JTC}{ogs5 \strong{mfp} bloc sub key word.}

\item{NON_GRAVITY}{ogs5 \strong{mfp} bloc sub key word.}

\item{PHASE_DIFFUSION}{ogs5 \strong{mfp} bloc sub key word.}

\item{SPECIFIC_HEAT_CAPACITY}{ogs5 \strong{mfp} bloc sub key word.}

\item{SPECIFIC_HEAT_SOURCE}{ogs5 \strong{mfp} bloc sub key word.}

\item{TEMPERATURE}{ogs5 \strong{mfp} bloc sub key word.}

\item{VISCOSITY}{ogs5 \strong{mfp} bloc sub key word.}
}
\value{
Updated \emph{ogs5} object.
}
\description{
Adds a sub-bloc to \strong{mfp} bloc of \emph{ogs5} for defining fluid
properties. For additional documentatoin of the input parameters see
the \href{https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/mfp.html}{ogs5 keyword docs}
or have a look at the input scripts from the \href{https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks}{r2ogs5-benchmarks repository}.
All arguments except \strong{x} have to be of class \emph{character}.
}
\examples{
tmp <- tempdir()
ogs5_obj <- create_ogs5(sim_name = "ex1", sim_id = 1L,
                        sim_path = paste0(tmp, "/ex1"))

ogs5_obj <- input_add_mfp_bloc(ogs5_obj, FLUID_NAME = "FLUID_PROPERTIES1",
                               FLUID_TYPE = "LIQUID",
                               DENSITY = "1 1000.0",
                               VISCOSITY = "1 1.0e-3",
                               HEAT_CONDUCTIVITY = "1 0.0")
}
