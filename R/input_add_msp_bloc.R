
#' input_add_msp_bloc
#' @description Adds a sub-bloc to **msp** bloc of *ogs5* for defining solid
#'   properties. For additional documentatoin of the input parameters see
#'   the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/msp.html)
#'   or have a look at the input scripts from the [r2ogs5-benchmarks repository](
#'   https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks).
#'   All arguments except **x** have to be of class *character*.
#'   Note the syntax with line breaks "\\n" for ELASTICITY and THERMAL.
#' @param x Simulation object of class *ogs5*.
#' @param BIOT_CONSTANT ogs5 **msp** bloc sub key word.
#' @param CREEP ogs5 **msp** bloc sub key word.
#' @param DENSITY ogs5 **msp** bloc sub key word.
#' @param ELASTICITY ogs5 **msp** bloc sub key word.
#' @param EXCAVATION ogs5 **msp** bloc sub key word.
#' @param E_Function ogs5 **msp** bloc sub key word.
#' @param GRAVITY_CONSTANT ogs5 **msp** bloc sub key word.
#' @param MICRO_STRUCTURE_PLAS ogs5 **msp** bloc sub key word.
#' @param NAME Name of the solid. *character*
#' @param NON_REACTIVE_FRACTION ogs5 **msp** bloc sub key word.
#' @param PLASTICITY ogs5 **msp** bloc sub key word.
#' @param REACTIVE_SYSTEM ogs5 **msp** bloc sub key word.
#' @param SOLID_BULK_MODULUS ogs5 **msp** bloc sub key word.
#' @param SPECIFIC_HEAT_SOURCE ogs5 **msp** bloc sub key word.
#' @param STRESS_INTEGRATION_TOLERANCE ogs5 **msp** bloc sub key word.
#' @param STRESS_UNIT ogs5 **msp** bloc sub key word.
#' @param SWELLING_PRESSURE_TYPE ogs5 **msp** bloc sub key word.
#' @param THERMAL ogs5 **msp** bloc sub key word.
#' @param THRESHOLD_DEV_STR ogs5 **msp** bloc sub key word.
#' @param TIME_DEPENDENT_YOUNGS_POISSON ogs5 **msp** bloc sub key word.
#' @return Updated *ogs5* object.
#' @export
#' @examples
#' tmp <- tempdir()
#' ogs5_obj <- create_ogs5(sim_name = "ex1", sim_id = 1L,
#'                         sim_path = paste0(tmp, "/ex1"))
#'
#' ogs5_obj <- input_add_msp_bloc(ogs5_obj, NAME = "SOLID_PROPERTIES1",
#'                                DENSITY = "1 1.80000e+003")
input_add_msp_bloc <-

   function(
    x = list(),
    BIOT_CONSTANT = NULL,
    CREEP = NULL,
    DENSITY = NULL,
    ELASTICITY = NULL,
    EXCAVATION = NULL,
    E_Function = NULL,
    GRAVITY_CONSTANT = NULL,
    MICRO_STRUCTURE_PLAS = NULL,
    NAME = NULL,
    NON_REACTIVE_FRACTION = NULL,
    PLASTICITY = NULL,
    REACTIVE_SYSTEM = NULL,
    SOLID_BULK_MODULUS = NULL,
    SPECIFIC_HEAT_SOURCE = NULL,
    STRESS_INTEGRATION_TOLERANCE = NULL,
    STRESS_UNIT = NULL,
    SWELLING_PRESSURE_TYPE = NULL,
    THERMAL = NULL,
    THRESHOLD_DEV_STR = NULL,
    TIME_DEPENDENT_YOUNGS_POISSON = NULL

   ){

      msp_name <- NAME

      # validate input
      valid_ogs5(x)

      # look if ogs5-objinput$msp exists and valid, otherwise create
      if (!("msp" %in% names(x$input))) {
         x$input$msp <- create_ogs5_msp()
      } else {

         valid_ogs5_msp(x$input$msp)

         if (msp_name %in% names(x$input$msp)) {
            stop("msp_name does already exist", call. = FALSE)
         }

      }

      # automatic line break for PLASTICITY
      if (!is.null(PLASTICITY)) {
        PLASTICITY <- PLASTICITY %>%
          stringr::str_split(" ") %>%
          unlist %>%
          paste0(collapse = "\n ")
      }

      # create and add sublist to msp-list

      x$input$msp[[paste(msp_name)]] <- list(

         "BIOT_CONSTANT" = BIOT_CONSTANT,
         "CREEP" = CREEP,
         "DENSITY" = DENSITY,
         "ELASTICITY" = ELASTICITY,
         "EXCAVATION" = EXCAVATION,
         "E_Function" = E_Function,
         "GRAVITY_CONSTANT" = GRAVITY_CONSTANT,
         "MICRO_STRUCTURE_PLAS" = MICRO_STRUCTURE_PLAS,
         "NAME" = NAME,
         "NON_REACTIVE_FRACTION" = NON_REACTIVE_FRACTION,
         "PLASTICITY" = PLASTICITY,
         "REACTIVE_SYSTEM" = REACTIVE_SYSTEM,
         "SOLID_BULK_MODULUS" = SOLID_BULK_MODULUS,
         "SPECIFIC_HEAT_SOURCE" = SPECIFIC_HEAT_SOURCE,
         "STRESS_INTEGRATION_TOLERANCE" = STRESS_INTEGRATION_TOLERANCE,
         "STRESS_UNIT" = STRESS_UNIT,
         "SWELLING_PRESSURE_TYPE" = SWELLING_PRESSURE_TYPE,
         "THERMAL" = THERMAL,
         "THRESHOLD_DEV_STR" = THRESHOLD_DEV_STR,
         "TIME_DEPENDENT_YOUNGS_POISSON" = TIME_DEPENDENT_YOUNGS_POISSON

      ) %>%
         purrr::discard(is.null) %>%
         purrr::discard(BBmisc::isFALSE) %>%
         structure(class = "ogs5_msp_bloc")

      valid_ogs5_msp_bloc(x$input$msp[[paste(msp_name)]])

      return(x)

   }
