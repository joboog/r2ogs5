% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_add_gli_points.R
\name{input_add_gli_surface}
\alias{input_add_gli_surface}
\title{input_add_gli_surface}
\usage{
input_add_gli_surface(
  x = list(),
  EPSILON = character(),
  ID = NULL,
  MAT_GROUP = NULL,
  srf_name = character(),
  POLYLINES = character(),
  TYPE = NULL,
  TIN = NULL
)
}
\arguments{
\item{x}{Simulation object of class \emph{ogs5}.}

\item{EPSILON}{Error tolerance of the specified points to the mesh nodes.}

\item{ID}{ogs5 \strong{gli} bloc sub key word.}

\item{MAT_GROUP}{ogs5 \strong{gli} bloc sub key word.}

\item{srf_name}{Name of the surface. \emph{character}}

\item{POLYLINES}{Names of the polylines that create the surface. \emph{character}}

\item{TYPE}{ogs5 \strong{gli} bloc sub key word.}

\item{TIN}{ogs5 \strong{gli} bloc sub key word.}
}
\value{
Updated \emph{ogs5} object.
}
\description{
Adds a sub-bloc to \strong{gli} bloc of \emph{ogs5} for defining a
surface based on plylines. For additional documentatoin of the input
parameters see the \href{https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/gli/h_surface}{ogs5 keyword docs}.
}
