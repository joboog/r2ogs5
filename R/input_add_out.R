# function to add out-bloc to ogs5-obj
# input: ogs5-obj
# output: updated ogs5-obj

input_add_out_bloc <-

  function(
    x = list(),
    out_name = character(NULL),

    AMPLIFIER = NULL,
    DAT_TYPE = character(NULL),
    DIS_TYPE = NULL,
    ELE_VALUES = NULL,
    GEO_TYPE = character(NULL),
    MFP_VALUES = NULL,
    MMP_VALUES = NULL,
    MSH_TYPE = NULL,
    NOD_VALUES = NULL,
    PCON_VALUES = NULL,
    PCS_TYPE = NULL,
    RWPT_VALUES = NULL,
    TECPLOT_ZONE_SHARE = FALSE,
    TIM_TYPE = character(NULL),
    VARIABLESHARING = FALSE

  ){

    # validate input
    valid_ogs5(x)

    # look if ogs5-obj$input$out exists and valid, otherwise create
    if (!("out" %in% names(x$input))) {
      x$input$out <- create_ogs5_out()
    } else {

      valid_ogs5_out(x$input$out)

      if (out_name %in% names(x$input$out)) {
        stop("out_name does already exist", call. = FALSE)
      }

    }

    # create and add sublist to st-list

    x$input$out[[paste(out_name)]] <- list(

      "AMPLIFIER" = AMPLIFIER,
      "DAT_TYPE" = DAT_TYPE,
      "DIS_TYPE" = DIS_TYPE,
      "ELE_VALUES" = ELE_VALUES,
      "GEO_TYPE" = GEO_TYPE,
      "MFP_VALUES" = MFP_VALUES,
      "MMP_VALUES" = MMP_VALUES,
      "MSH_TYPE" = MSH_TYPE,
      "NOD_VALUES" = NOD_VALUES,
      "PCON_VALUES" = PCON_VALUES,
      "PCS_TYPE" = PCS_TYPE,
      "RWPT_VALUES" = RWPT_VALUES,
      "TECPLOT_ZONE_SHARE" = TECPLOT_ZONE_SHARE,
      "TIM_TYPE" = TIM_TYPE,
      "VARIABLESHARING" = VARIABLESHARING

      ) %>%
      purrr::discard(is.null) %>%
      purrr::discard(BBmisc::isFALSE) %>%
      structure(class = "ogs5_out_bloc")

    valid_ogs5_out_bloc(x$input$out[[paste(out_name)]])

    return(x)

  }
