
#' input_add_st_bloc
#' @description Adds a sub-bloc to **st** bloc of *ogs5* for defining a source
#'   term. For additional documentatoin of the input parameters see
#'   the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/st.html).
#' @param x Simulation object of class *ogs5*.
#' @param st_name Name of the source term.
#' @param AIR_BREAKING
#' @param CHANNEL
#' @param COMP_NAME
#' @param CONSTRAINED
#' @param DISTRIBUTE_VOLUME_FLUX
#' @param DIS_TYPE
#' @param EXPLICIT_SURFACE_WATER_PRESSURE
#' @param FCT_TYPE
#' @param GEO_TYPE
#' @param MSH_TYPE
#' @param NEGLECT_SURFACE_WATER_PRESSURE
#' @param NODE_AVERAGING
#' @param PCS_TYPE
#' @param PRIMARY_VARIABLE
#' @param TIME_INTERPOLATION
#' @param TIM_TYPE
#'
#' @return Updated *ogs5* object.
#' @export
#' @examples
#' ogs5_obj <- input_add_st_bloc(ogs5_obj, st_name = "SOURCE_TERM1",
#'                               PCS_TYPE = "GROUNDWATER_FLOW",
#'                               PRIMARY_VARIABLE = "HEAD",
#'                               GEO_TYPE = "POINT POINT1",
#'                               DIS_TYPE = "CONSTANT -2.9976852e-006")
input_add_st_bloc <-

   function(
      x = list(),
      st_name = character(NULL),

      AIR_BREAKING = NULL,
      CHANNEL = NULL,
      COMP_NAME = NULL,
      CONSTRAINED = NULL,
      DISTRIBUTE_VOLUME_FLUX = NULL,
      DIS_TYPE = character(NULL),
      EXPLICIT_SURFACE_WATER_PRESSURE = NULL,
      FCT_TYPE = NULL,
      GEO_TYPE = character(NULL),
      MSH_TYPE = NULL,
      NEGLECT_SURFACE_WATER_PRESSURE = NULL,
      NODE_AVERAGING = NULL,
      PCS_TYPE = character(NULL),
      PRIMARY_VARIABLE = character(NULL),
      TIME_INTERPOLATION = NULL,
      TIM_TYPE = NULL

   ){

      # validate input
      valid_ogs5(x)

      # look if ogs5-obj$input$st exists and valid, otherwise create
      if (!("st" %in% names(x$input))) {
         x$input$st <- create_ogs5_st()
      } else {

         valid_ogs5_st(x$input$st)

         if (st_name %in% names(x$input$st)) {
            stop("st_name does already exist", call. = FALSE)
         }

      }

      # create and add sublist to st-list

      x$input$st[[paste(st_name)]] <- list(

         "AIR_BREAKING" = AIR_BREAKING,
         "CHANNEL" = CHANNEL,
         "COMP_NAME" = COMP_NAME,
         "CONSTRAINED" = CONSTRAINED,
         "DISTRIBUTE_VOLUME_FLUX" = DISTRIBUTE_VOLUME_FLUX,
         "DIS_TYPE" = DIS_TYPE,
         "EXPLICIT_SURFACE_WATER_PRESSURE" = EXPLICIT_SURFACE_WATER_PRESSURE,
         "FCT_TYPE" = FCT_TYPE,
         "GEO_TYPE" = GEO_TYPE,
         "MSH_TYPE" = MSH_TYPE,
         "NEGLECT_SURFACE_WATER_PRESSURE" = NEGLECT_SURFACE_WATER_PRESSURE,
         "NODE_AVERAGING" = NODE_AVERAGING,
         "PCS_TYPE" = PCS_TYPE,
         "PRIMARY_VARIABLE" =  PRIMARY_VARIABLE,
         "TIME_INTERPOLATION" = TIME_INTERPOLATION,
         "TIM_TYPE" = TIM_TYPE

      ) %>%
         purrr::discard(is.null) %>%
         purrr::discard(BBmisc::isFALSE) %>%
         structure(class = "ogs5_st_condition")

      valid_ogs5_st_condition(x$input$st[[paste(st_name)]])

      return(x)

   }
