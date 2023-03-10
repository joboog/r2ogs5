% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_add_rei_bloc.R
\name{input_add_rei_bloc}
\alias{input_add_rei_bloc}
\title{input_add_rei_bloc}
\usage{
input_add_rei_bloc(
  x = list(),
  rei_name = NULL,
  ALL_PCS_DUMP = FALSE,
  DISSOLVED_NEUTRAL_CO2_SPECIES_NAME = NULL,
  HEATPUMP_2DH_TO_2DV = NULL,
  INITIAL_CONDITION_OUTPUT = FALSE,
  MOL_PER = NULL,
  PCS_RENAME_INIT = FALSE,
  PCS_RENAME_POST = FALSE,
  PCS_RENAME_PRE = FALSE,
  POROSITY_RESTART = NULL,
  PRESSURE = NULL,
  P_VLE = NULL,
  RESIDUAL = NULL,
  SODIUM_SPECIES_NAME = NULL,
  SOLID_SPECIES_DUMP_MOLES = NULL,
  TEMPERATURE = NULL,
  UPDATE_INITIAL_SOLID_COMPOSITION = FALSE,
  VLE = NULL,
  WATER_CONCENTRATION = NULL,
  WATER_SATURATION_LIMIT = NULL,
  WATER_SPECIES_NAME = NULL
)
}
\arguments{
\item{x}{Simulation object of class \emph{ogs5}.}

\item{rei_name}{Name of the \strong{rei} sub-bloc.}

\item{ALL_PCS_DUMP}{\emph{logical}}

\item{DISSOLVED_NEUTRAL_CO2_SPECIES_NAME}{ogs5 \strong{rei} bloc sub key word.}

\item{HEATPUMP_2DH_TO_2DV}{ogs5 \strong{rei} bloc sub key word.}

\item{INITIAL_CONDITION_OUTPUT}{\emph{logical}}

\item{MOL_PER}{ogs5 \strong{rei} bloc sub key word.}

\item{PCS_RENAME_INIT}{\emph{logical}}

\item{PCS_RENAME_POST}{\emph{logical}}

\item{PCS_RENAME_PRE}{\emph{logical}}

\item{POROSITY_RESTART}{ogs5 \strong{rei} bloc sub key word.}

\item{PRESSURE}{ogs5 \strong{rei} bloc sub key word.}

\item{P_VLE}{ogs5 \strong{rei} bloc sub key word.}

\item{RESIDUAL}{ogs5 \strong{rei} bloc sub key word.}

\item{SODIUM_SPECIES_NAME}{ogs5 \strong{rei} bloc sub key word.}

\item{SOLID_SPECIES_DUMP_MOLES}{ogs5 \strong{rei} bloc sub key word.}

\item{TEMPERATURE}{ogs5 \strong{rei} bloc sub key word.}

\item{UPDATE_INITIAL_SOLID_COMPOSITION}{\emph{logical}}

\item{VLE}{ogs5 \strong{rei} bloc sub key word.}

\item{WATER_CONCENTRATION}{ogs5 \strong{rei} bloc sub key word.}

\item{WATER_SATURATION_LIMIT}{ogs5 \strong{rei} bloc sub key word.}

\item{WATER_SPECIES_NAME}{ogs5 \strong{rei} bloc sub key word.}
}
\value{
Updated \emph{ogs5} object.
}
\description{
Adds a sub-bloc to \strong{rei} bloc of \emph{ogs5} for defining a
reaction interface. For additional documentatoin of the input parameters see
the \href{https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/rei.html}{ogs5 keyword docs}
or have a look at the input scripts from the \href{https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks}{r2ogs5-benchmarks repository}.
Most arguments have to be of class \emph{character}.
}
