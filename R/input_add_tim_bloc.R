
input_add_tim_bloc <-

  function(
    x = list(),
    tim_name = character(NULL),

    #skey
    CRITICAL_TIME = NULL,
    INDEPENDENT = NULL,
    PCS_TYPE = character(NULL),
    SUBSTEPS = NULL,
    TIME_CONTROL = NULL,
    TIME_END = numeric(NULL),
    TIME_FIXED_POINTS = NULL,
    TIME_SPLITS = NULL,
    TIME_START = numeric(NULL),
    TIME_STEPS = NULL,
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
      "TIME_CONTROL" = TIME_CONTROL,
      "TIME_END" = TIME_END,
      "TIME_FIXED_POINTS" = TIME_FIXED_POINTS,
      "TIME_SPLITS" = TIME_SPLITS,
      "TIME_START" = TIME_START,
      "TIME_STEPS" = TIME_STEPS,
      "TIME_UNIT"  = TIME_UNIT

    ) %>%
      purrr::discard(is.null) %>%
      structure(class = "ogs5_tim_bloc")

    valid_ogs5_tim_bloc(x$input$tim[[paste(tim_name)]])

    return(x)

  }
