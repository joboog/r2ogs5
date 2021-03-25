
#' input_add_mfp_bloc
#' @description Adds a sub-bloc to **mfp** bloc of *ogs5* for defining fluid
#'   properties. For additional documentatoin of the input parameters see
#'   the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/mfp.html)
#'   or have a look at the input scripts from the [r2ogs5-benchmarks repository](
#'   https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks).
#'   All arguments except **x** have to be of class *character*.
#' @param x Simulation object of class *ogs5*.
#' @param COMPONENTS ogs5 **mfp** bloc sub key word.
#' @param COMPRESSIBILITY ogs5 **mfp** bloc sub key word.
#' @param DAT_TYPE ogs5 **mfp** bloc sub key word.
#' @param DECAY ogs5 **mfp** bloc sub key word.
#' @param DENSITY ogs5 **mfp** bloc sub key word.
#' @param DIFFUSION ogs5 **mfp** bloc sub key word.
#' @param DRHO_DT_UNSATURATED ogs5 **mfp** bloc sub key word.
#' @param EOS_TYPE ogs5 **mfp** bloc sub key word.
#' @param FLUID_NAME ogs5 **mfp** bloc sub key word.
#' @param FLUID_TYPE ogs5 **mfp** bloc sub key word.
#' @param GRAVITY ogs5 **mfp** bloc sub key word.
#' @param HEAT_CONDUCTIVITY ogs5 **mfp** bloc sub key word.
#' @param ISOTHERM ogs5 **mfp** bloc sub key word.
#' @param JTC ogs5 **mfp** bloc sub key word.
#' @param NON_GRAVITY ogs5 **mfp** bloc sub key word.
#' @param PHASE_DIFFUSION ogs5 **mfp** bloc sub key word.
#' @param SPECIFIC_HEAT_CAPACITY ogs5 **mfp** bloc sub key word.
#' @param SPECIFIC_HEAT_SOURCE ogs5 **mfp** bloc sub key word.
#' @param TEMPERATURE ogs5 **mfp** bloc sub key word.
#' @param VISCOSITY ogs5 **mfp** bloc sub key word.
#' @return Updated *ogs5* object.
#' @export
#' @examples
#' tmp <- tempdir()
#' ogs5_obj <- create_ogs5(sim_name = "ex1", sim_id = 1L,
#'                         sim_path = paste0(tmp, "/ex1"))
#'
#' ogs5_obj <- input_add_mfp_bloc(ogs5_obj, FLUID_NAME = "FLUID_PROPERTIES1",
#'                                FLUID_TYPE = "LIQUID",
#'                                DENSITY = "1 1000.0",
#'                                VISCOSITY = "1 1.0e-3",
#'                                HEAT_CONDUCTIVITY = "1 0.0")
input_add_mfp_bloc <-

   function(
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

   ){

      mfp_name <- FLUID_NAME

      # validate input
      valid_ogs5(x)

      # look if ogs5-obj$input$mfp eximfps and valid, otherwise create
      if (!("mfp" %in% names(x$input))) {
         x$input$mfp <- create_ogs5_mfp()
      } else {

         valid_ogs5_mfp(x$input$mfp)

         if (mfp_name %in% names(x$input$mfp)) {
            stop("mfp_name does already exist", call. = FALSE)
         }

      }

      # create and add sublist to mfp-list

      x$input$mfp[[paste(mfp_name)]] <- list(

         "COMPONENTS" = COMPONENTS,
         "COMPRESSIBILITY" = COMPRESSIBILITY,
         "DAT_TYPE" = DAT_TYPE,
         "DECAY" = DECAY,
         "DENSITY" = DENSITY,
         "DIFFUSION" = DIFFUSION,
         "DRHO_DT_UNSATURATED" = DRHO_DT_UNSATURATED,
         "EOS_TYPE" = EOS_TYPE,
         "FLUID_NAME" = FLUID_NAME,
         "FLUID_TYPE" = FLUID_TYPE,
         "GRAVITY" = GRAVITY,
         "HEAT_CONDUCTIVITY" = HEAT_CONDUCTIVITY,
         "ISOTHERM" = ISOTHERM,
         "JTC" = JTC,
         "NON_GRAVITY" = NON_GRAVITY,
         "PHASE_DIFFUSION" = PHASE_DIFFUSION,
         "SPECIFIC_HEAT_CAPACITY" = SPECIFIC_HEAT_CAPACITY,
         "SPECIFIC_HEAT_SOURCE" = SPECIFIC_HEAT_SOURCE,
         "TEMPERATURE" = TEMPERATURE,
         "VISCOSITY" = VISCOSITY

      ) %>%
         purrr::discard(is.null) %>%
         purrr::discard(BBmisc::isFALSE) %>%
         structure(class = "ogs5_mfp_bloc")

      valid_ogs5_mfp_bloc(x$input$mfp[[paste(mfp_name)]])

      return(x)

   }
