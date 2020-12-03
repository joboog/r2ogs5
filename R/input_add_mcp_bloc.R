# function to add mcp_component to ogs5-obj
# input: ogs5-obj
# output: updated ogs5-obj

#' input_add_mcp_bloc
#' @description Adds a sub-bloc to **mcp** bloc of *ogs5* for defining component
#'  properties. For additional documentatoin of the input parameters
#'  see the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/mcp.html)
#' @param x *ogs5* simulation object.
#' @param ACENTRIC_FACTOR
#' @param A_ZERO
#' @param BUBBLE_VELOCITY
#' @param CRITICAL_PRESSURE
#' @param CRITICAL_TEMPERATURE
#' @param DECAY
#' @param DIFFUSION
#' @param FLUID_ID
#' @param FLUID_PHASE
#' @param FORMULA
#' @param ISOTHERM
#' @param MAXIMUM_AQUEOUS_SOLUBILITY
#' @param MINERAL_DENSITY
#' @param MOBILE
#' @param MOLAR_DENSITY
#' @param MOLAR_VOLUME
#' @param MOLAR_WEIGHT
#' @param MOL_MASS
#' @param NAME Component name. *character*
#' @param OutputMassOfComponentInModel
#' @param TRANSPORT_PHASE
#' @param VALENCE
#' @param VOLUME_DIFFUSION
#'
#' @return Updated *ogs5* object.
#' @export
#'
#' @examples
#' ogs5_obj <- input_add_mcp_bloc(ogs5_obj, NAME = "C(4)",
#'                                MOBILE = "1",
#'                                DIFFUSION = "1 0.0e-9",
#'                                VALENCE = "-2")
input_add_mcp_bloc <-

   function(
      x = list(),
      ACENTRIC_FACTOR = NULL,
      A_ZERO = NULL,
      BUBBLE_VELOCITY = NULL,
      CRITICAL_PRESSURE = NULL,
      CRITICAL_TEMPERATURE = NULL,
      DECAY = NULL,
      DIFFUSION = NULL,
      FLUID_ID = NULL,
      FLUID_PHASE = NULL,
      FORMULA = NULL,
      ISOTHERM = NULL,
      MAXIMUM_AQUEOUS_SOLUBILITY = NULL,
      MINERAL_DENSITY = NULL,
      MOBILE = character(NULL),
      MOLAR_DENSITY = NULL,
      MOLAR_VOLUME = NULL,
      MOLAR_WEIGHT = NULL,
      MOL_MASS = NULL,
      NAME = character(NULL),
      OutputMassOfComponentInModel = NULL,
      TRANSPORT_PHASE = NULL,
      VALENCE = NULL,
      VOLUME_DIFFUSION = NULL

   ){
      mcp_name <- NAME

      # validate input
      valid_ogs5(x)

      # look if ogs5-obj$input$mcp eximcps and valid, otherwise create
      if (!("mcp" %in% names(x$input))) {
         x$input$mcp <- create_ogs5_mcp()
      } else {

         valid_ogs5_mcp(x$input$mcp)

         if (mcp_name %in% names(x$input$mcp)) {
            stop("mcp_name does already exist", call. = FALSE)
         }

      }

      # create and add sublist to mcp-list

      x$input$mcp[[paste(mcp_name)]] <- list(

         "ACENTRIC_FACTOR" = ACENTRIC_FACTOR,
         "A_ZERO" = A_ZERO,
         "BUBBLE_VELOCITY" = BUBBLE_VELOCITY,
         "CRITICAL_PRESSURE" = CRITICAL_PRESSURE,
         "CRITICAL_TEMPERATURE" = CRITICAL_TEMPERATURE,
         "DECAY" = DECAY,
         "DIFFUSION" = DIFFUSION,
         "FLUID_ID" = FLUID_ID,
         "FLUID_PHASE" = FLUID_PHASE,
         "FORMULA" = FORMULA,
         "ISOTHERM" = ISOTHERM,
         "MAXIMUM_AQUEOUS_SOLUBILITY" = MAXIMUM_AQUEOUS_SOLUBILITY,
         "MINERAL_DENSITY" = MINERAL_DENSITY,
         "MOBILE" = MOBILE,
         "MOLAR_DENSITY" = MOLAR_DENSITY,
         "MOLAR_VOLUME" = MOLAR_VOLUME,
         "MOLAR_WEIGHT" = MOLAR_WEIGHT,
         "MOL_MASS" = MOL_MASS,
         "NAME" = NAME,
         "OutputMassOfComponentInModel" = OutputMassOfComponentInModel,
         "TRANSPORT_PHASE" = TRANSPORT_PHASE,
         "VALENCE" = VALENCE,
         "VOLUME_DIFFUSION" = VOLUME_DIFFUSION

      ) %>%
         purrr::discard(is.null) %>%
         purrr::discard(BBmisc::isFALSE) %>%
         structure(class = "ogs5_mcp_component")

      valid_ogs5_mcp_component(x$input$mcp[[paste(mcp_name)]])

      return(x)

   }
