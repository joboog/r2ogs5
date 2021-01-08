
#' create_ogs5
#' @description Constructor for the *ogs5* base-class
#' @param sim_name Name of the simulation. character
#' @param sim_id ID of the simulation. integer
#' @param sim_path Path where to export the input files and to run the
#'   simulation. character
#'
#' @return Object of class *og5*
#' @export
#'
#' @examples
#' ogs5_obj <- create_ogs5(sim_name = "ogs5_obj", sim_id = 1L,
#'               sim_path = "examples/benchmarks/Engesgaard/2Kin/slow_kin_pqc",)
create_ogs5 <- function(
   #x = list(input=list(), output=list(mod)),
   sim_name = character(NULL),
   sim_id = integer(NULL),
   sim_path = character(NULL)
) {

   # validation
   if (!is.character(sim_name)) {
      stop("'sim_name' has to be of type character", call. = FALSE)
   }

   if (!is.character(sim_path)) {
      stop("'sim_path' has to be of type character", call. = FALSE)
   }

   if (!is.integer(sim_id)) {
      stop("'sim_id' has to be of type integer", call. = FALSE)
   }

   # define ogs5-obj
   x <- list(input=list(), output=list())

   structure(
      x,
      class = "ogs5",
      sim_name = sim_name,
      sim_id = sim_id,
      sim_path = sim_path
   )
}

#' valid_ogs5
#' @description  Validator for *ogs5* base class
#' @param x Object of class *ogs5* to validate.
valid_ogs5 <- function(x){

   if (!class(x)=="ogs5") {
      stop("x is not of class 'ogs5' ", call. = FALSE)
   }
   if (is.null(x$input)) {
     stop("'input' list missing.", call. = FALSE)
   }
   if (is.null(x$output)) {
    stop("'output' list missing.", call. = FALSE)
   }

}
