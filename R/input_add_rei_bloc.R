# function to add rei-bloc to ogs5-obj
# input: ogs5-obj
# output: updated ogs5-obj

input_add_rei_bloc <-

  function(
    x = list(),
    rei_name = character(NULL),

    ALL_PCS_DUMP = FALSE,
    DISSOLVED_NEUTRAL_CO2_SPECIES_NAME = NULL,
    HEATPUMP_2DH_TO_2DV = NULL,
    INITIAL_CONDITION_OUTPUT = FALSE,
    MOL_PER = NULL,
    PCS_RENAME_INIT = FALSE,
    PCS_RENAME_POST = FALSE,
    PCS_RENAME_PRE = FALSE,
    POROSITY_RESTART = NULL,
    PRESSURE = NULL,
    P_VLE = NULL,
    RESIDUAL = NULL,
    SODIUM_SPECIES_NAME = NULL,
    SOLID_SPECIES_DUMP_MOLES = NULL,
    TEMPERATURE = NULL,
    UPDATE_INITIAL_SOLID_COMPOSITION = FALSE,
    VLE = NULL,
    WATER_CONCENTRATION = NULL,
    WATER_SATURATION_LIMIT = NULL,
    WATER_SPECIES_NAME = NULL

  ){

    # validate input
    valid_ogs5(x)

    # look if ogs5-obj$input$rei exists and valid, otherwise create
    if (!("rei" %in% names(x$input))) {
      x$input$rei <- create_ogs5_rei()
    } else {

      valid_ogs5_rei(x$input$rei)

      if (rei_name %in% names(x$input$rei)) {
        stop("rei_name does already exist", call. = FALSE)
      }

      if ( (names(x$input$rei) %>% length) > 1) {
        warning("you should give only one rei_bloc", call. = FALSE)
      }

    }

    # create and add sublist to st-list

    x$input$rei[[paste(rei_name)]] <- list(

      "ALL_PCS_DUMP" = ALL_PCS_DUMP,
      "DISSOLVED_NEUTRAL_CO2_SPECIES_NAME" = DISSOLVED_NEUTRAL_CO2_SPECIES_NAME,
      "HEATPUMP_2DH_TO_2DV" = HEATPUMP_2DH_TO_2DV,
      "INITIAL_CONDITION_OUTPUT" = INITIAL_CONDITION_OUTPUT,
      "MOL_PER" = MOL_PER,
      "PCS_RENAME_INIT" = PCS_RENAME_INIT,
      "PCS_RENAME_POST" = PCS_RENAME_POST,
      "PCS_RENAME_PRE" = PCS_RENAME_PRE,
      "POROSITY_RESTART" = POROSITY_RESTART,
      "PRESSURE" = PRESSURE,
      "P_VLE" = P_VLE,
      "RESIDUAL" = RESIDUAL,
      "SODIUM_SPECIES_NAME" = SODIUM_SPECIES_NAME,
      "SOLID_SPECIES_DUMP_MOLES" = SOLID_SPECIES_DUMP_MOLES,
      "TEMPERATURE" = TEMPERATURE,
      "UPDATE_INITIAL_SOLID_COMPOSITION" = UPDATE_INITIAL_SOLID_COMPOSITION,
      "VLE" = VLE,
      "WATER_CONCENTRATION" = WATER_CONCENTRATION,
      "WATER_SATURATION_LIMIT" = WATER_SATURATION_LIMIT,
      "WATER_SPECIES_NAME" = WATER_SPECIES_NAME

    ) %>%
      purrr::discard(is.null) %>%
      purrr::discard(BBmisc::isFALSE) %>%
      structure(class = "ogs5_rei_bloc")

    valid_ogs5_rei_bloc(x$input$rei[[paste(rei_name)]])

    return(x)

  }
