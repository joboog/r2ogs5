
#' input_add_tim_bloc
#' @description Adds a sub-bloc to **tim** bloc of *ogs5* for defining numerical
#'   time discritization. For additional documentatoin of the input parameters
#'   see the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/tim.html)
#'   or have a look at the input scripts from the [r2ogs5-benchmarks repository](
#'   https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks).
#'   Most arguments have to be of class *character*.
#' @param x Simulation object of class *ogs5*.
#' @param tim_name Name of the **tim** sub-bloc.
#' @param CRITICAL_TIME
#' @param INDEPENDENT
#' @param PCS_TYPE
#' @param SUBSTEPS
#' @param TIME_CONTROL
#' @param TIME_END *numeric*
#' @param TIME_FIXED_POINTS
#' @param TIME_SPLITS
#' @param TIME_START *numeric*
#' @param TIME_STEPS
#' @param TIME_UNIT
#'
#' @return Updated *ogs5* object.
#' @export
#'
#' @examples
#' ogs5_obj <- input_add_tim_bloc(ogs5_obj, tim_name = "TIME_STEPPING1",
#'                               PCS_TYPE = "GROUNDWATER_FLOW",
#'                               TIME_STEPS = "210 100.0",
#'                               TIME_END = "21000.0",
#'                               TIME_START = "0.0")
input_add_tim_bloc <-

  function(
    x = list(),
    tim_name = NULL,

    #skey
    CRITICAL_TIME = NULL,
    INDEPENDENT = NULL,
    PCS_TYPE = NULL,
    SUBSTEPS = NULL,
    TIME_END = NULL,
    TIME_START = NULL,
    TIME_STEPS = NULL,
    TIME_CONTROL = NULL,
    TIME_FIXED_POINTS = NULL,
    TIME_SPLITS = NULL,
    TIME_UNIT  = NULL

  ){

    # validate input
    valid_ogs5(x)

    # look if ogs5-obj$input$tim exists and valid, otherwise create
    if (!("tim" %in% names(x$input))) {
      x$input$tim <- create_ogs5_tim()
    } else {

      valid_ogs5_tim(x$input$tim)

      if (tim_name %in% names(x$input$tim)) {
        stop("tim_name does already exist", call. = FALSE)
      }

      if (PCS_TYPE %in% sapply(x$input$tim, "[[", 1)) {
        warning("PCS_TYPE does already exist", call. = FALSE)
      }
    }

    # create and add sublist to tim-list
    x$input$tim[[paste(tim_name)]] <- list(

      "CRITICAL_TIME" = CRITICAL_TIME,
      "INDEPENDENT" = INDEPENDENT,
      "PCS_TYPE" = PCS_TYPE,
      "SUBSTEPS" = SUBSTEPS,
      "TIME_END" = TIME_END,
      "TIME_FIXED_POINTS" = TIME_FIXED_POINTS,
      "TIME_SPLITS" = TIME_SPLITS,
      "TIME_START" = TIME_START,
      "TIME_STEPS" = TIME_STEPS,
      "TIME_UNIT"  = TIME_UNIT,
      "TIME_CONTROL" = TIME_CONTROL


    ) %>%
      purrr::discard(is.null) %>%
      structure(class = "ogs5_tim_bloc")

    valid_ogs5_tim_bloc(x$input$tim[[paste(tim_name)]])

    return(x)

  }
