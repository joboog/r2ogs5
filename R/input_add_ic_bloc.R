# function to add ic_condition-bloc to ogs5-obj
# input: ogs5-obj
# output: updated ogs5-obj

#' input_add_ic_bloc
#' @description Adds a sub-bloc to **ic** bloc of *ogs5* for defining an initial
#'   condition (IC). For additional documentatoin of the input parameters see
#'   the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/ic.html)
#'   or have a look at the input scripts from the [r2ogs5-benchmarks repository](
#'   https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks).
#'   All arguments except **x** have to be of class *character*.
#' @param x Simulation object of class *ogs5*.
#' @param ic_name Name of the initial condition.
#' @param COMP_NAME Name of the component.
#' @param DIS_TYPE Distribution type of intial condition.
#' @param GEO_TYPE Name of a geometry defined in **gml** to be apply the IC on.
#' @param PCS_TYPE Apply IC for the process defined in **pcs**.
#' @param PRIMARY_VARIABLE Apply IC for the primary variable defined in **pcs**.
#'
#' @return Updated *ogs5* object.
#' @export
#'
#' @examples
#' ogs5_obj <- input_add_ic_bloc(ogs5_obj, ic_name = "INITIAL_CONDITION1",
#'  PCS_TYPE = "GROUNDWATER_FLOW",
#' PRIMARY_VARIABLE = "HEAD",
#' GEO_TYPE = "DOMAIN",
#' DIS_TYPE = "CONSTANT 1.0")
input_add_ic_bloc <-

   function(
      x = list(),
      ic_name = NULL,
      COMP_NAME = NULL,
      DIS_TYPE = NULL,
      GEO_TYPE = NULL,
      PCS_TYPE = NULL,
      PRIMARY_VARIABLE = NULL

   ){

      # validate input
      valid_ogs5(x)

      # look if ogs5-obj$input$ic exists and valid, otherwise create
      if (!("ic" %in% names(x$input))) {
         x$input$ic <- create_ogs5_ic()
      } else {

         valid_ogs5_ic(x$input$ic)

         if (ic_name %in% names(x$input$ic)) {
            stop("ic_name does already exist", call. = FALSE)
         }

         if (PCS_TYPE!="MASS_TRANSPORT" &&
             PCS_TYPE %in% sapply(x$input$ic, "[[", 1)
         ) {
            stop("PCS_TYPE does already exist", call. = FALSE)
         }
      }

      # create and add sublist to ic-list

      x$input$ic[[paste(ic_name)]] <- list(

         "COMP_NAME" = COMP_NAME,
         "DIS_TYPE" = DIS_TYPE,
         "GEO_TYPE" = GEO_TYPE,
         "PCS_TYPE" = PCS_TYPE,
         "PRIMARY_VARIABLE" =  PRIMARY_VARIABLE

      ) %>%
         purrr::discard(is.null) %>%
         purrr::discard(BBmisc::isFALSE) %>%
         structure(class = "ogs5_ic_condition")

      valid_ogs5_ic_condition(x$input$ic[[paste(ic_name)]])

      return(x)

   }
