
#' input_add_st_bloc
#' @description Adds a sub-bloc to **st** bloc of *ogs5* for defining a source
#'   term. For additional documentatoin of the input parameters see
#'   the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/st.html)
#'   or have a look at the input scripts from the [r2ogs5-benchmarks repository](
#'   https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks).
#'   All arguments except **x** have to be of class *character*.
#' @param x Simulation object of class *ogs5*.
#' @param st_name Name of the source term.
#' @param AIR_BREAKING ogs5 **st** bloc sub key word.
#' @param CHANNEL ogs5 **st** bloc sub key word.
#' @param COMP_NAME ogs5 **st** bloc sub key word.
#' @param CONSTRAINED ogs5 **st** bloc sub key word.
#' @param DISTRIBUTE_VOLUME_FLUX ogs5 **st** bloc sub key word.
#' @param DIS_TYPE ogs5 **st** bloc sub key word.
#' @param EXPLICIT_SURFACE_WATER_PRESSURE ogs5 **st** bloc sub key word.
#' @param FCT_TYPE ogs5 **st** bloc sub key word.
#' @param GEO_TYPE ogs5 **st** bloc sub key word.
#' @param MSH_TYPE ogs5 **st** bloc sub key word.
#' @param NEGLECT_SURFACE_WATER_PRESSURE ogs5 **st** bloc sub key word.
#' @param NODE_AVERAGING ogs5 **st** bloc sub key word.
#' @param PCS_TYPE ogs5 **st** bloc sub key word.
#' @param PRIMARY_VARIABLE ogs5 **st** bloc sub key word.
#' @param TIME_INTERPOLATION ogs5 **st** bloc sub key word.
#' @param TIM_TYPE ogs5 **st** bloc sub key word.
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
      st_name = NULL,

      AIR_BREAKING = NULL,
      CHANNEL = NULL,
      COMP_NAME = NULL,
      CONSTRAINED = NULL,
      DISTRIBUTE_VOLUME_FLUX = NULL,
      DIS_TYPE = NULL,
      EXPLICIT_SURFACE_WATER_PRESSURE = NULL,
      FCT_TYPE = NULL,
      GEO_TYPE = NULL,
      MSH_TYPE = NULL,
      NEGLECT_SURFACE_WATER_PRESSURE = NULL,
      NODE_AVERAGING = NULL,
      PCS_TYPE = NULL,
      PRIMARY_VARIABLE = NULL,
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
