% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cal_bayesOpt.R
\name{cal_bayesOpt}
\alias{cal_bayesOpt}
\title{Bayesian Optimization for OGS5 models}
\usage{
cal_bayesOpt(
  par_init,
  max_it,
  kappa = "log_t",
  exp_data,
  ogs5_obj,
  outbloc_names,
  ogs_exe = NULL,
  objective_function = NULL,
  ensemble_path,
  ensemble_cores,
  ensemble_name,
  gp_control = list(kernel = list(type = "exponential", power = 1.95), nug_thres = 20,
    maxit = 100),
  scale_which = NULL,
  scale_fun = I,
  unscale_fun = I,
  BO_init = NULL,
  quiet = FALSE
)
}
\arguments{
\item{par_init}{\emph{tibble} with initial parameters to start calibration.
It needs to follow a strict format and is best created with the function
\code{\link[=cal_sample_parameters]{cal_sample_parameters()}}. See details for more.}

\item{max_it}{\emph{numeric} maximum number of BO iterations.}

\item{kappa}{\emph{character}, \emph{numeric} or \emph{function} that determines the tuning
parameter that weights exploitation versus exploration depending on dimension
and iteration (see details).}

\item{exp_data}{calibration data that will be used in the objective function.}

\item{ogs5_obj}{\emph{ogs5} base simulation object.}

\item{outbloc_names}{\emph{character vector} names of the blocs specified in the
*\emph{.out} file that should be used for calibration. The argument will be passed
to the function \code{\link[=ogs5_get_output_specific]{ogs5_get_output_specific()}}.}

\item{ogs_exe}{\emph{character} path to the ogs executable.}

\item{objective_function}{\emph{function} specified by the user that should exist in
the global environment and be of the form
\verb{f <- function(ogs_obj, exp_data) \{ ... return(sim_error) \}} and return a
single \emph{numeric} value that represents the error from the current \code{ogs_obj}
and \code{exp_data}.}

\item{ensemble_path}{\emph{character} path where ensemble for initial parameters
should be written and run.}

\item{ensemble_cores}{\emph{numeric} integer number for the number of cores used
during the initial ensemble run.}

\item{ensemble_name}{\emph{character} name for the ensemble run to distinguish from
other (potentially running) ensembles. The name will appear in numbered folders
for every run of the ensemble.}

\item{gp_control}{A named \emph{list} with settings for \code{\link[=GP_fit]{GP_fit()}}. Default values
of \code{kernel}, \code{nug_thres} and \code{maxit} correspond to the default settings in
\code{GP_fit}.}

\item{scale_which}{(optional) \emph{character} that identifies the parameters in
\code{par_init} that should be scaled. Default is \emph{NULL}, then all parameters
will be scaled according to \emph{scale_fun}.}

\item{scale_fun}{(optional) \emph{function} that allows sampling from a scaled
distribution, e.g. \code{scale_fun = log10} if the values are on a \emph{log} scale.
Default is \code{I()} i.e. no transformation.}

\item{unscale_fun}{(optional) \emph{function} inverse of the prev\verb{ious function (i.e. }unscale_fun = function(x) 10**x\verb{) to transform parameters back after sampling. Default is }I()` as well.}

\item{BO_init}{(optional) object of class \emph{BO} from a previous run to continue
optimization. No further arguments except \code{kappa}, \code{max_it}, \code{ogs_exe} and
the scaling functions (if necessary) have to be supplied, as everything else
will be taken from the \code{BO_init} class attributes.}

\item{quiet}{\emph{locigal} to indicate if progress of the algorithm
(iteration, queried objective function value, )}
}
\value{
A \emph{list} with
\describe{
\item{gp_model}{The final Gaussian-Process model object (library \code{GPfit})}
\item{values}{Parameters from \emph{(0, 1)} used as dependent variables in \emph{gp_model}}
\item{objective_values}{Simulation errors used as independent variable in \emph{gp_model}}
\item{min}{Parameter where the smallest simulation error was found.}
\item{pred_mu}{Vector of predictions by the meta-model cast for every
element in \code{objective_values} previous to the objective function evaluation.}
\item{pred_mse}{Vector of prediction variance for every element in
\code{pred_mu}.}
}
The \emph{BO}-class \emph{attributes} include all the relevant further info and data
such as the \emph{ogs5} simulation object, experimental data and the target
function.
}
\description{
Implementation of the Bayesian Optimization algorithm to find an
approximate minimum of a user specified function, usually the mean squared error
between simulation results and experiment.
}
\details{
Bayesian Optimization as proposed by \emph{Mockus et al. (1978)} with the
Lower Confidence Bound acquisition function to balance Exploration and
Exploitation as in \emph{P. Auer (2002)}. In brief, a Gaussian Process model
implementation by \emph{Macdonald et al. (2015)} is used as a surrogate model to
explore the parameter space in search for points where the model is either
uncertain and/or predicts a minimum. The Algorithm stops, if the specified
maximum number of iteration (\code{max_it}) is reached.

The uncertainty paramter \code{kappa} is by default set to \code{"log_t"} which results in
\eqn{\kappa_i = (2 log(d \pi^2 i^2 / (6 * 0.1)))^1/2} which ensures the
unceratainty bound from the meta-model to be greater than the prediction error
with a probability of 0.9 after (\emph{Sirinivas et al. (2010)}).
Setting \code{kappa = "cooling"} results in
\eqn{\kappa_i = (2log(400d^22)) / (1 + exp(-4.6 + 9.2 * i / N))^1/2}
yielding a logistic decreasing function with turning point at \eqn{i = N/2}
converging to 0 towards the end of iterations (\eqn{N}) ensuring exploitation
of the meta-model. \code{kappa} can be set to any user specified function that
includes d and i as arguments, a positive constant or 0.

The \code{par_init} data frame should look like this:\tabular{lllllllll}{
   file_ext \tab mkey \tab skey \tab spec \tab min \tab max \tab 1 \tab 2 \tab ... \cr
   mmp \tab MEDIUM_PROPERTIES1 \tab POROSITY \tab 1 \tab 0.25 \tab 0.45 \tab 0.4 \tab 0.3 \tab ... \cr
   mmp \tab Medium_PROPERTIES2 \tab ... \tab  \tab  \tab  \tab  \tab  \tab  \cr
}


If a parameter has two values that should be calibrated, simply two identical
parameter specifications (with different min/max values) should be provided
in adjacent rows.

The initial parameter set can be created with the functions
\code{\link[=cal_create_calibration_set]{cal_create_calibration_set()}} and \code{\link[=cal_sample_parameters]{cal_sample_parameters()}}.

Analogous to the \code{calibration_set}, every row is a parameter with its location
in the \code{ogs5} object specified in the first few columns.
Additional specifications to the parameter can be added in \emph{spec},
but it can also left blank. Following, \emph{min} and \emph{max} should be indicated
and thereafter  the starting values for the parameter.
}
\examples{
\dontrun{
# For examples please refer to
vignettes("cal_bayesOpt")
}

}
\references{
Using Confidence Bounds for Exploitation-Exploration Trade-offs
\emph{P. Auer} \strong{2002} Journal of Machine Learning Research 3, 397-422

GPfit: An R package for fitting a Gaussian process model to deterministic
simulator outputs
\emph{Macdonald, Blake and Ranjan, Pritam and Chipman, Hugh}
\strong{2012} Journal of Statistical Software, 64, issue 12

Toward global optimization
\emph{Mockus, Jonas and Tiesis, Vytautas and Zilinskas, Antanas,}
\strong{1978} volume 2, chapter Bayesian Methods for Seeking the Extremum,
pp. 117???128.

Gaussian process optimization in the bandit setting: No regret and
experimental design
\emph{Sirinivas N., Krause A., Kakade S. et al.} \strong{2010} ICML 2010-Proceedings,
27th International Conference on Machine Learning, 1015-1022.
}
