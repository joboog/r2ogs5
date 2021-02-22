
#' input_add_mcp_bloc
#' @description Adds a sub-bloc to **mcp** bloc of *ogs5* for defining component
#'  properties. For additional documentatoin of the input parameters
#'  see the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/mcp.html)
#'   or have a look at the input scripts from the [r2ogs5-benchmarks repository](
#'   https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks).
#'   All arguments except **x** have to be of class *character*.
#' @param x *ogs5* simulation object.
#' @param ACENTRIC_FACTOR ogs5 **mcp** bloc sub key word.
#' @param A_ZERO ogs5 **mcp** bloc sub key word.
#' @param BUBBLE_VELOCITY ogs5 **mcp** bloc sub key word.
#' @param CRITICAL_PRESSURE ogs5 **mcp** bloc sub key word.
#' @param CRITICAL_TEMPERATURE ogs5 **mcp** bloc sub key word.
#' @param DECAY ogs5 **mcp** bloc sub key word.
#' @param DIFFUSION ogs5 **mcp** bloc sub key word.
#' @param FLUID_ID ogs5 **mcp** bloc sub key word.
#' @param FLUID_PHASE ogs5 **mcp** bloc sub key word.
#' @param FORMULA ogs5 **mcp** bloc sub key word.
#' @param ISOTHERM ogs5 **mcp** bloc sub key word.
#' @param MAXIMUM_AQUEOUS_SOLUBILITY ogs5 **mcp** bloc sub key word.
#' @param MINERAL_DENSITY ogs5 **mcp** bloc sub key word.
#' @param MOBILE ogs5 **mcp** bloc sub key word.
#' @param MOLAR_DENSITY ogs5 **mcp** bloc sub key word.
#' @param MOLAR_VOLUME ogs5 **mcp** bloc sub key word.
#' @param MOLAR_WEIGHT ogs5 **mcp** bloc sub key word.
#' @param MOL_MASS ogs5 **mcp** bloc sub key word.
#' @param NAME Component name.
#' @param OutputMassOfComponentInModel ogs5 **mcp** bloc sub key word.
#' @param TRANSPORT_PHASE ogs5 **mcp** bloc sub key word.
#' @param VALENCE ogs5 **mcp** bloc sub key word.
#' @param VOLUME_DIFFUSION ogs5 **mcp** bloc sub key word.
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
      MOBILE = NULL,
      MOLAR_DENSITY = NULL,
      MOLAR_VOLUME = NULL,
      MOLAR_WEIGHT = NULL,
      MOL_MASS = NULL,
      NAME = NULL,
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
