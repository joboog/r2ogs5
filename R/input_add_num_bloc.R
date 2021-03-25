
#' input_add_num_bloc
#' @description Adds a sub-bloc to **num** bloc of *ogs5* for defining numerical
#'   solver properties. For additional documentatoin of the input parameters see
#'   the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/num.html)
#'   or have a look at the input scripts from the [r2ogs5-benchmarks repository](
#'   https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks).
#'   All arguments except **x** have to be of class *character*.
#' @param x Simulation object of class *ogs5*.
#' @param num_name Name of the sub-bloc.
#' @param COUPLED_PROCESS ogs5 **num** bloc sub key word.
#' @param COUPLING_CONTROL ogs5 **num** bloc sub key word.
#' @param COUPLING_ITERATIONS ogs5 **num** bloc sub key word.
#' @param DYNAMIC_DAMPING ogs5 **num** bloc sub key word.
#' @param ELE_GAUSS_POINTS ogs5 **num** bloc sub key word.
#' @param ELE_MASS_LUMPING ogs5 **num** bloc sub key word.
#' @param ELE_SUPG ogs5 **num** bloc sub key word.
#' @param ELE_UPWINDING ogs5 **num** bloc sub key word.
#' @param EXTERNAL_SOLVER_OPTION ogs5 **num** bloc sub key word.
#' @param FEM_FCT ogs5 **num** bloc sub key word.
#' @param GRAVITY_PROFILE ogs5 **num** bloc sub key word.
#' @param LINEAR_SOLVER ogs5 **num** bloc sub key word.
#' @param LOCAL_PICARD1 ogs5 **num** bloc sub key word.
#' @param NON_LINEAR_ITERATION ogs5 **num** bloc sub key word.
#' @param NON_LINEAR_SOLVER ogs5 **num** bloc sub key word.
#' @param NON_LINEAR_UPDATE_VELOCITY ogs5 **num** bloc sub key word.
#' @param OVERALL_COUPLING ogs5 **num** bloc sub key word.
#' @param PCS_TYPE ogs5 **num** bloc sub key word.
#' @param PLASTICITY_TOLERANCE ogs5 **num** bloc sub key word.
#' @param RENUMBER ogs5 **num** bloc sub key word.
#' @param TIME_STEPS ogs5 **num** bloc sub key word.
#'
#' @return Updated *ogs5* object.
#' @export
#' @examples
#' tmp <- tempdir()
#' ogs5_obj <- create_ogs5(sim_name = "ex1", sim_id = 1L,
#'                         sim_path = paste0(tmp, "/ex1"))
#'
#' ogs5_obj <- input_add_num_bloc(ogs5_obj, num_name = "NUMERICS1",
#'                                PCS_TYPE = "GROUNDWATER_FLOW",
#'                                ELE_GAUSS_POINTS = "3",
#'                                LINEAR_SOLVER = "2 6 1.e-014 1000 1.0 1 2")
input_add_num_bloc <-

   function(
      x = list(),
      num_name = NULL,
      COUPLED_PROCESS = NULL,
      COUPLING_CONTROL = NULL,
      COUPLING_ITERATIONS = NULL,
      DYNAMIC_DAMPING = NULL,
      ELE_GAUSS_POINTS = NULL,
      ELE_MASS_LUMPING = NULL,
      ELE_SUPG = NULL,
      ELE_UPWINDING = NULL,
      EXTERNAL_SOLVER_OPTION = NULL,
      FEM_FCT = NULL,
      GRAVITY_PROFILE = NULL,
      LINEAR_SOLVER = NULL,
      LOCAL_PICARD1 = NULL,
      NON_LINEAR_ITERATION = NULL,
      NON_LINEAR_SOLVER = NULL,
      NON_LINEAR_UPDATE_VELOCITY = NULL,
      OVERALL_COUPLING = NULL,
      PCS_TYPE = NULL,
      PLASTICITY_TOLERANCE = NULL,
      RENUMBER = NULL,
      TIME_STEPS  = NULL

   ){
      # validate input
      valid_ogs5(x)

      # look if ogs5-objinput$num exists and valid, otherwise create
      if (!("num" %in% names(x$input))) {
         x$input$num <- create_ogs5_num()
      } else {

         valid_ogs5_num(x$input$num)

         if (num_name %in% names(x$input$num)) {
            stop("num_name does already exist", call. = FALSE)
         }

         if (PCS_TYPE %in% sapply(x$input$bc, "[[", 1)) {
            stop("PCS_TYPE does already exist", call. = FALSE)
         }

      }

      # create and add sublist to num-list

      x$input$num[[paste(num_name)]] <- list(

         "COUPLED_PROCESS" = COUPLED_PROCESS,
         "COUPLING_CONTROL" = COUPLING_CONTROL,
         "COUPLING_ITERATIONS" = COUPLING_ITERATIONS,
         "DYNAMIC_DAMPING" = DYNAMIC_DAMPING,
         "ELE_GAUSS_POINTS" = ELE_GAUSS_POINTS,
         "ELE_MASS_LUMPING" = ELE_MASS_LUMPING,
         "ELE_SUPG" = ELE_SUPG,
         "ELE_UPWINDING" = ELE_UPWINDING,
         "EXTERNAL_SOLVER_OPTION" = EXTERNAL_SOLVER_OPTION,
         "FEM_FCT" = FEM_FCT,
         "GRAVITY_PROFILE" = GRAVITY_PROFILE,
         "LINEAR_SOLVER"= LINEAR_SOLVER ,
         "LOCAL_PICARD1" = LOCAL_PICARD1,
         "NON_LINEAR_ITERATION" = NON_LINEAR_ITERATION,
         "NON_LINEAR_SOLVER" = NON_LINEAR_SOLVER,
         "NON_LINEAR_UPDATE_VELOCITY" = NON_LINEAR_UPDATE_VELOCITY,
         "OVERALL_COUPLING" = OVERALL_COUPLING,
         "PCS_TYPE" = PCS_TYPE,
         "PLASTICITY_TOLERANCE" = PLASTICITY_TOLERANCE,
         "RENUMBER" = RENUMBER,
         "TIME_STEPS"  = TIME_STEPS

      ) %>%
         purrr::discard(is.null) %>%
         purrr::discard(BBmisc::isFALSE) %>%
         structure(class = "ogs5_num_bloc")

      valid_ogs5_num_bloc(x$input$num[[paste(num_name)]])

      return(x)

   }
