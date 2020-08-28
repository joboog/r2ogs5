# function to add rfd-bloc to ogs5-obj
# input: ogs5-obj
# output: updated ogs5-obj

input_add_rfd_bloc <-

  function(
    x = list(),
    rfd_name = character(NULL),
    mkey = character(NULL),
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
