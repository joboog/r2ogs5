# function to add fct-bloc to ogs5-obj
# input: ogs5-obj
# output: updated ogs5-obj

#' input_add_fct_bloc
#' @description Adds a sub-bloc to **fct** bloc of *ogs5* for defining
#'   functional relationships. For additional documentatiin of the input
#'   parameters see the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/fct.html)
#'   or have a look at the input scripts from the [r2ogs5-benchmarks repository](
#'   https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks).
#'   All arguments except **x** and **data_values** have to be of class
#'   *character*.
#' @param x Simulation object of class *ogs5*.
#' @param data_type 'DATA' or 'MATRIX'. *character*
#' @param data_values A *tibble* with data values
#' @param DIMENSION Dimension if *data_type* is 'Matrix'.
#' @param DIS_TYPE ogs5 **fct** bloc sub key word.
#' @param GEO_TYPE Name of a geometry defined in **gml** to relate the function.
#' @param TYPE ogs5 **fct** bloc sub key word.
#' @return Updated *ogs5* object.
#' @export
input_add_fct_bloc <-

  function(
    x = list(),
    data_type = c("DATA", "MATRIX"),
    data_values =tibble::tibble(NULL),

    #skey
    DIMENSION = NULL,
    DIS_TYPE = NULL,
    GEO_TYPE = NULL,
    TYPE = NULL

  ){

    fct_name <- TYPE

    # validate input
    valid_ogs5(x)

    # look if ogs5-obj$input$fct exists and valid, otherwise create
    if (!("fct" %in% names(x$input))) {
      x$input$fct <- create_ogs5_fct()
    } else {

      valid_ogs5_fct(x$input$fct)

      if (fct_name %in% names(x$input$fct)) {
        stop("fct_name does already exist", call. = FALSE)
      }
    }

    if (data_type == "MATRIX" &
        DIMENSION %>% is.null){
      stop("DIMENSION is missing", call. = FALSE)
    }

    # create and add sublist to st-list

    x$input$fct[[paste(fct_name)]] <- list(

      "data_type" = data_type,
      "data_values" = data_values,
      "DIMENSION" = DIMENSION,
      "DIS_TYPE" = DIS_TYPE,
      "GEO_TYPE" = GEO_TYPE,
      "TYPE" = TYPE

    ) %>%
      purrr::discard(is.null) %>%
      structure(class = "ogs5_fct_bloc")

    valid_ogs5_fct_bloc(x$input$fct[[paste(fct_name)]])

    return(x)

  }
