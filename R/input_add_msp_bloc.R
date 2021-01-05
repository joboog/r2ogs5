
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
#' @param BIOT_CONSTANT
#' @param CREEP
#' @param DENSITY
#' @param ELASTICITY
#' @param EXCAVATION
#' @param E_Function
#' @param GRAVITY_CONSTANT
#' @param MICRO_STRUCTURE_PLAS
#' @param NAME Name of the solid. *character*
#' @param NON_REACTIVE_FRACTION
#' @param PLASTICITY
#' @param REACTIVE_SYSTEM
#' @param SOLID_BULK_MODULUS
#' @param SPECIFIC_HEAT_SOURCE
#' @param STRESS_INTEGRATION_TOLERANCE
#' @param STRESS_UNIT
#' @param SWELLING_PRESSURE_TYPE
#' @param THERMAL
#' @param THRESHOLD_DEV_STR
#' @param TIME_DEPENDENT_YOUNGS_POISSON
#'
#' @return
#' @export
#'
#' @examples
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
