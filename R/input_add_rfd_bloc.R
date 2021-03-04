
#' input_add_rfd_bloc
#' @description Adds a sub-bloc to **rfd** bloc of *ogs5* for defining time
#'   dependent functions. For additional documentation of the input parameters
#'   see the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/rfd.html)
#'   or have a look at the input scripts from the [r2ogs5-benchmarks repository](
#'   https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks).
#' @param x Simulation object of class *ogs5*.
#' @param rfd_name *character* Name of the **rfd** sub-bloc.
#' @param mkey *character* ogs5 #-keyword: c("CURVE", "CURVES").
#' @param data *Tibble* with two *numeric* columns. One colum has to be named
#'   "time".
#' @param INTERPOLATION ogs5 **rfd** bloc sub key word.
#' @param MSH_TYPE  ogs5 **rfd** bloc sub key word.
#'
#' @return Updated *ogs5* object.
#' @export
#' @example
#' tmp <- tempdir()
#' ogs5_obj <- create_ogs5(sim_name = "ex1", sim_id = 1L,
#'                         sim_path = paste0(tmp, "/ex1"))
#'
#' ogs5_obj <- input_add_rfd_bloc(x = ogs5_obj,
#'              rfd_name = "tracer", mkey = "CURVES",
#'              data = tibble::tibble(time=c(0, 3600, 3600.1, 720, 36000000),
#'                                    conc=c(1,1,0,0,0)))
input_add_rfd_bloc <-

  function(
    x = list(),
    rfd_name = NULL,
    mkey = NULL,
    data = tibble::tibble(NULL),

    #skey
    INTERPOLATION = NULL,
    MSH_TYPE = NULL

  ){

    # validate input
    valid_ogs5(x)

    # look if ogs5-obj$input$rfd exists and valid, otherwise create
    if (!("rfd" %in% names(x$input))) {
      x$input$rfd <- create_ogs5_rfd()
    } else {

      valid_ogs5_rfd(x$input$rfd)

      if (rfd_name %in% names(x$input$rfd)) {
        stop("rfd_name does already exist", call. = FALSE)
      }
    }

    # check data
    if (!(data %>% names %in% "time") ||
           !(data[[1]] %>% is.numeric) ||
           !(data[[2]] %>% is.numeric)
        ){
      stop("data has the wrong format", call. = FALSE)
    }

    # create and add sublist to rfd-list

    x$input$rfd[[paste(rfd_name)]] <- list(

      "mkey" = mkey,
      "data" = data,
      "INTERPOLATION" = INTERPOLATION,
      "MSH_TYPE" = MSH_TYPE

    ) %>%
      purrr::discard(is.null) %>%
      structure(class = "ogs5_rfd_bloc")

    valid_ogs5_rfd_bloc(x$input$rfd[[paste(rfd_name)]])

    return(x)

  }
