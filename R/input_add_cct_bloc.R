
#' input_add_cct_bloc
#' @description Adds a sub-bloc to **cct** bloc of *ogs5* for defining a
#'   communication table to execute parallel runs with PETS library. For
#'   additional documentation of the input parameters see
#'   the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/cct.html)
#'   or have a look at the input scripts from the [r2ogs5-benchmarks repository](
#'   https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks).
#'   All arguments except **x** have to be of class *character*.
#' @param x Simulation object. *ogs*
#' @param cct_name Name of the **cct** bloc.
#' @param MYRANK ogs5 **cct** bloc sub keword.
#' @param NEIGHBOR ogs5 **cct** bloc sub keword.
#' @param NNEIGHBORS ogs5 **cct** bloc sub keword.
#' @return Updated *ogs5* object.
#' @export
input_add_cct_bloc <-

  function(
    x = list(),
    cct_name = character(NULL),

    MYRANK = NULL,
    NEIGHBOR = NULL,
    NNEIGHBORS = NULL

  ){

    # validate input
    valid_ogs5(x)

    # look if ogs5-obj$input$cct exists and valid, otherwise create
    if (!("cct" %in% names(x$input))) {
      x$input$cct <- create_ogs5_cct()
    } else {

      valid_ogs5_cct(x$input$cct)

      if (cct_name %in% names(x$input$cct)) {
        stop("cct_name does already exist", call. = FALSE)
      }
    }

    # create and add sublist to st-list

    x$input$cct[[paste(cct_name)]] <- list(

      "MYRANK" = MYRANK,
      "NEIGHBOR" = NEIGHBOR,
      "NNEIGHBORS" = NNEIGHBORS

    ) %>%
      purrr::discard(is.null) %>%
      structure(class = "ogs5_cct_bloc")

    valid_ogs5_cct_bloc(x$input$cct[[paste(cct_name)]])

    return(x)

  }
