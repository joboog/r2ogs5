# function to add bc_condition-bloc to ogs5-obj
# input: ogs5-obj
# output: updated ogs5-obj

#' input_add_bc_bloc
#' @description Adds a sub-bloc to **bc** bloc of *ogs5* for defining boundary
#'   conditions. For additional documentatoin of the input parameters see
#'   the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/bc.html).
#' @param x Simulation object of class *ogs5*.
#' @param bc_name Name of the boundary condition.
#' @param COMP_NAME Component name.
#' @param CONSTRAINED
#' @param COPY_VALUE
#' @param DIS_TYPE Distribution type of boundary condition.
#' @param DIS_TYPE_CONDITION
#' @param EPSILON
#' @param EXCAVATION
#' @param FCT_TYPE Name of a function defined in **fct** bloc to be used as BC.
#' @param GEO_TYPE Name of a geometry defined in **gml** to be apply the BC on.
#' @param MSH_TYPE
#' @param NO_DIPS_INCREMENT
#' @param PCS_TYPE Apply bc for the process defined in **pcs**.
#' @param PRESSURE_AS_HEAD
#' @param PRIMARY_VARIABLE Apply bc for the primary variable defined in **pcs**.
#' @param TIME_CONTROLLED_ACTIVE
#' @param TIM_TYPE Set name of a **CURVE** defined in **rfd** if bc should be
#'   time dependend.
#' @return Updated *ogs5* simulation object.
#' @export
#'
#' @examples
#' ogs5_obj <- input_add_bc_bloc(ogs5_obj, bc_name = "BOUNDARY_CONDITION1",
#'                               PCS_TYPE = "GROUNDWATER_FLOW",
#'                               PRIMARY_VARIABLE = "HEAD",
#'                               GEO_TYPE = "POINT POINT0",
#'                               DIS_TYPE = "CONSTANT 1.0e5")
input_add_bc_bloc <-

   function(
      x = list(),
      bc_name = character(NULL),

      COMP_NAME = NULL,
      CONSTRAINED = NULL,
      COPY_VALUE = NULL,
      DIS_TYPE = character(NULL),
      DIS_TYPE_CONDITION = NULL,
      EPSILON = NULL,
      EXCAVATION = NULL,
      FCT_TYPE = NULL,
      GEO_TYPE = character(NULL),
      MSH_TYPE = NULL,
      NO_DIPS_INCREMENT = NULL,
      PCS_TYPE = character(NULL),
      PRESSURE_AS_HEAD = NULL,
      PRIMARY_VARIABLE = character(NULL),
      TIME_CONTROLLED_ACTIVE = NULL,
      TIM_TYPE = NULL

   ){

      # validate input
      valid_ogs5(x)

      # look if ogs5-obj$input$bc exists and valid, otherwise create
      if (!("bc" %in% names(x$input))) {
         x$input$bc <- create_ogs5_bc()
      } else {

         valid_ogs5_bc(x$input$bc)

         if (bc_name %in% names(x$input$bc)) {
            stop("bc_name does already exist", call. = FALSE)
         }

         if (PCS_TYPE!="MASS_TRANSPORT" &&
             PCS_TYPE %in% sapply(x$input$bc, "[[", 1)
         ) {
            stop("PCS_TYPE does already exist", call. = FALSE)
         }
      }

      # create and add sublist to bc-list

      x$input$bc[[paste(bc_name)]] <- list(

         "COMP_NAME" = COMP_NAME,
         "CONSTRAINED" = CONSTRAINED,
         "COPY_VALUE" = COPY_VALUE,
         "DIS_TYPE" = DIS_TYPE,
         "DIS_TYPE_CONDITION" = DIS_TYPE_CONDITION,
         "EPSILON" = EPSILON,
         "EXCAVATION" = EXCAVATION,
         "FCT_TYPE" = FCT_TYPE,
         "GEO_TYPE" = GEO_TYPE,
         "MSH_TYPE" = MSH_TYPE,
         "NO_DIPS_INCREMENT" = NO_DIPS_INCREMENT,
         "PCS_TYPE" = PCS_TYPE,
         "PRESSURE_AS_HEAD" = PRESSURE_AS_HEAD,
         "PRIMARY_VARIABLE" =  PRIMARY_VARIABLE,
         "TIME_CONTROLLED_ACTIVE" = TIME_CONTROLLED_ACTIVE,
         "TIM_TYPE" = TIM_TYPE

      ) %>%
         purrr::discard(is.null) %>%
         purrr::discard(BBmisc::isFALSE) %>%
         structure(class = "ogs5_bc_condition")

      valid_ogs5_bc_condition(x$input$bc[[paste(bc_name)]])

      return(x)

   }
