# add krc-bloc to ogs5-obj

# steps:
# look if sub-list names krc exists in input list
# if not, define a sub-list in input names krc
# add krc-blocs as additional list


#' input_add_krc_bloc
#' @description Adds a sub-bloc to **krc** bloc of *ogs5* for defining kinetic
#'   reactions. For additional documentatoin of the input parameters see
#'   the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/krc.html)
#'   or have a look at the input scripts from the [r2ogs5-benchmarks repository](
#'   https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks).
#'   Most arguments have to be of class *character*.
#' @param x *ogs5* simulation object.
#' @param krc_name Name of the **krc** sub bloc.
#' @param mkey Main keyword of the *ogs5_krc_bloc*: c("BLOB_PROPERTIES",
#' "KINREACTIONDATA", "MICROBE_PROPERTIES", "REACTION").
#' @param ACTIVITY_MODEL ogs5 **krc** bloc sub key word.
#' @param ALLOW_REACTIONS ogs5 **krc** bloc sub key word.
#' @param BACTERIACAPACITY ogs5 **krc** bloc sub key word.
#' @param BACTERIAL_YIELD ogs5 **krc** bloc sub key word.
#' @param BACTERIANAME ogs5 **krc** bloc sub key word.
#' @param BACTERIA_SPECIFIC_CAPACITY ogs5 **krc** bloc sub key word.
#' @param BASETERM ogs5 **krc** bloc sub key word.
#' @param CALC_SHERWOOD ogs5 **krc** bloc sub key word.
#' @param CALC_SHERWOOD_MODIFIED ogs5 **krc** bloc sub key word.
#' @param CHEMAPPNAME ogs5 **krc** bloc sub key word.
#' @param COPY_CONCENTRATIONS ogs5 **krc** bloc sub key word.
#' @param D50 ogs5 **krc** bloc sub key word.
#' @param DEBUG_OUTPUT ogs5 **krc** bloc sub key word.
#' @param DM ogs5 **krc** bloc sub key word.
#' @param DS ogs5 **krc** bloc sub key word.
#' @param EQUATION ogs5 **krc** bloc sub key word.
#' @param EQUILIBRIUM_CONSTANT ogs5 **krc** bloc sub key word.
#' @param EXCHANGE_PARAMETERS ogs5 **krc** bloc sub key word.
#' @param GAS_DISSOLUTION ogs5 **krc** bloc sub key word.
#' @param GEOMETRY ogs5 **krc** bloc sub key word.
#' @param GRAIN_SPHERE_RATIO ogs5 **krc** bloc sub key word.
#' @param GROWTH ogs5 **krc** bloc sub key word.
#' @param INHIBITIONTERMS ogs5 **krc** bloc sub key word.
#' @param INITIAL_TIMESTEP ogs5 **krc** bloc sub key word.
#' @param INTERFACIAL_AREA ogs5 **krc** bloc sub key word.
#' @param ISOTOPE_FRACTIONATION ogs5 **krc** bloc sub key word.
#' @param LAGNEAU_BENCHMARK ogs5 **krc** bloc sub key word.
#' @param LENGTH ogs5 **krc** bloc sub key word.
#' @param MECHANISMTERM ogs5 **krc** bloc sub key word.
#' @param MICROBENAME ogs5 **krc** bloc sub key word.
#' @param MINERALNAME ogs5 **krc** bloc sub key word.
#' @param MIN_BACTERIACONC ogs5 **krc** bloc sub key word.
#' @param MIN_CONCENTRATION_REPLACE ogs5 **krc** bloc sub key word.
#' @param MIN_TIMESTEP ogs5 **krc** bloc sub key word.
#' @param MONODTERMS ogs5 **krc** bloc sub key word.
#' @param MONOD_REACTION_NAME ogs5 **krc** bloc sub key word.
#' @param NAME ogs5 **krc** bloc sub key word.
#' @param NAPL_CONTENT_INI ogs5 **krc** bloc sub key word.
#' @param NAPL_CONTENT_RES ogs5 **krc** bloc sub key word.
#' @param NAPL_PROPERTIES ogs5 **krc** bloc sub key word.
#' @param NO_REACTIONS ogs5 **krc** bloc sub key word.
#' @param OMEGA_THRESHOLD ogs5 **krc** bloc sub key word.
#' @param PRECIPITATION_BY_BASETERM_ONLY ogs5 **krc** bloc sub key word.
#' @param PRECIPITATION_EXPONENT ogs5 **krc** bloc sub key word.
#' @param PRECIPITATION_FACTOR ogs5 **krc** bloc sub key word.
#' @param PRODUCTIONSTOCH ogs5 **krc** bloc sub key word.
#' @param PRODUCTIONTERMS ogs5 **krc** bloc sub key word.
#' @param RATECONSTANT ogs5 **krc** bloc sub key word.
#' @param RATE_EXPONENTS ogs5 **krc** bloc sub key word.
#' @param REACTION_DEACTIVATION ogs5 **krc** bloc sub key word.
#' @param REACTION_ORDER ogs5 **krc** bloc sub key word.
#' @param REACTIVE_SURFACE_AREA ogs5 **krc** bloc sub key word.
#' @param RELATIVE_ERROR ogs5 **krc** bloc sub key word.
#' @param SCALE_DCDT ogs5 **krc** bloc sub key word.
#' @param SHERWOOD_MODEL ogs5 **krc** bloc sub key word.
#' @param SOLVER_TYPE ogs5 **krc** bloc sub key word.
#' @param SORPTION_TYPE ogs5 **krc** bloc sub key word.
#' @param SORT_NODES ogs5 **krc** bloc sub key word.
#' @param STANDARD_GIBBS_ENERGY ogs5 **krc** bloc sub key word.
#' @param SURFACES ogs5 **krc** bloc sub key word.
#' @param SWITCH_OFF_GEOMETRY ogs5 **krc** bloc sub key word.
#' @param TEMPERATURE_DEPENDENCE ogs5 **krc** bloc sub key word.
#' @param THRESHHOLDTERMS ogs5 **krc** bloc sub key word.
#' @param TORTUOSITY ogs5 **krc** bloc sub key word.
#' @param TYPE ogs5 **krc** bloc sub key word.
#' @param UI ogs5 **krc** bloc sub key word.
#' @param drmc ogs5 **krc** bloc sub key word.
#' @param drmc_PARAMETERS ogs5 **krc** bloc sub key word.
#'
#' @return Updated *ogs5* object.
#' @export
input_add_krc_bloc <-

  function(
    x = list(),
    krc_name = NULL,
    mkey = NULL,
    #mkey:
      #BLOB_PROPERTIES
      #KINREACTIONDATA
      #MICROBE_PROPERTIES
      #REACTION

    #skey:
    ACTIVITY_MODEL = NULL,
    ALLOW_REACTIONS = NULL,
    BACTERIACAPACITY = NULL,
    BACTERIAL_YIELD = NULL,
    BACTERIANAME = NULL,
    BACTERIA_SPECIFIC_CAPACITY = NULL,
    BASETERM = NULL,
    CALC_SHERWOOD = NULL,
    CALC_SHERWOOD_MODIFIED = NULL,
    CHEMAPPNAME = NULL,
    COPY_CONCENTRATIONS = NULL,
    D50 = NULL,
    DEBUG_OUTPUT = FALSE,
    DM = NULL,
    DS = NULL,
    EQUATION = NULL,
    EQUILIBRIUM_CONSTANT = NULL,
    EXCHANGE_PARAMETERS = NULL,
    GAS_DISSOLUTION = NULL,
    GEOMETRY = NULL,
    GRAIN_SPHERE_RATIO = NULL,
    GROWTH = NULL,
    INHIBITIONTERMS = NULL,
    INITIAL_TIMESTEP = NULL,
    INTERFACIAL_AREA = NULL,
    ISOTOPE_FRACTIONATION = NULL,
    LAGNEAU_BENCHMARK = FALSE,
    LENGTH = NULL,
    MECHANISMTERM = NULL,
    MICROBENAME = NULL,
    MINERALNAME = NULL,
    MIN_BACTERIACONC = NULL,
    MIN_CONCENTRATION_REPLACE = NULL,
    MIN_TIMESTEP = NULL,
    MONODTERMS = NULL,
    MONOD_REACTION_NAME = NULL,
    NAME = NULL,
    NAPL_CONTENT_INI = NULL,
    NAPL_CONTENT_RES = NULL,
    NAPL_PROPERTIES = NULL,
    NO_REACTIONS = NULL,
    OMEGA_THRESHOLD = NULL,
    PRECIPITATION_BY_BASETERM_ONLY = FALSE,
    PRECIPITATION_EXPONENT = NULL,
    PRECIPITATION_FACTOR = NULL,
    PRODUCTIONSTOCH = NULL,
    PRODUCTIONTERMS = NULL,
    RATECONSTANT = NULL,
    RATE_EXPONENTS = NULL,
    REACTION_DEACTIVATION = NULL,
    REACTION_ORDER = NULL,
    REACTIVE_SURFACE_AREA = NULL,
    RELATIVE_ERROR = NULL,
    SCALE_DCDT = FALSE,
    SHERWOOD_MODEL = NULL,
    SOLVER_TYPE = NULL,
    SORPTION_TYPE = NULL,
    SORT_NODES = FALSE,
    STANDARD_GIBBS_ENERGY = NULL,
    SURFACES = NULL,
    SWITCH_OFF_GEOMETRY = NULL,
    TEMPERATURE_DEPENDENCE = NULL,
    THRESHHOLDTERMS = NULL,
    TORTUOSITY = NULL,
    TYPE = NULL,
    UI = NULL,
    drmc = NULL,
    drmc_PARAMETERS = NULL
  ){

    # validate input
    valid_ogs5(x)

    # look if ogs5-obj$input$krc exists and valid, otherwise create
    if (!("krc" %in% names(x$input))) {
      x$input$krc <- create_ogs5_krc()
    } else {

      valid_ogs5_krc(x$input$krc)

      if (krc_name %in% names(x$input$krc)) {
        stop("krc_name does already exist", call. = FALSE)
      }

      if (is.null(mkey)) {
        stop("an 'mkey' has to be specified", call. = FALSE)
      }
    }

    # create and add sublist to krc-list

    x$input$krc[[paste(krc_name)]] <- list(

      "mkey" = mkey,
      "ACTIVITY_MODEL" = ACTIVITY_MODEL,
      "ALLOW_REACTIONS" = ALLOW_REACTIONS,
      "BACTERIACAPACITY" = BACTERIACAPACITY,
      "BACTERIAL_YIELD" = BACTERIAL_YIELD,
      "BACTERIANAME" = BACTERIANAME,
      "BACTERIA_SPECIFIC_CAPACITY" = BACTERIA_SPECIFIC_CAPACITY,
      "BASETERM" = BASETERM,
      "CALC_SHERWOOD" = CALC_SHERWOOD,
      "CALC_SHERWOOD_MODIFIED" = CALC_SHERWOOD_MODIFIED,
      "CHEMAPPNAME" = CHEMAPPNAME,
      "COPY_CONCENTRATIONS" = COPY_CONCENTRATIONS,
      "D50" = D50,
      "DEBUG_OUTPUT" = DEBUG_OUTPUT,
      "DM" = DM,
      "DS" = DS,
      "EQUATION" = EQUATION,
      "EQUILIBRIUM_CONSTANT" = EQUILIBRIUM_CONSTANT,
      "EXCHANGE_PARAMETERS" = EXCHANGE_PARAMETERS,
      "GAS_DISSOLUTION" = GAS_DISSOLUTION,
      "GEOMETRY" = GEOMETRY,
      "GRAIN_SPHERE_RATIO" = GRAIN_SPHERE_RATIO,
      "GROWTH" = GROWTH,
      "INHIBITIONTERMS" = INHIBITIONTERMS,
      "INITIAL_TIMESTEP" = INITIAL_TIMESTEP,
      "INTERFACIAL_AREA" = INTERFACIAL_AREA,
      "ISOTOPE_FRACTIONATION" = ISOTOPE_FRACTIONATION,
      "LAGNEAU_BENCHMARK" = LAGNEAU_BENCHMARK,
      "LENGTH" = LENGTH,
      "MECHANISMTERM" = MECHANISMTERM,
      "MICROBENAME" = MICROBENAME,
      "MINERALNAME" = MINERALNAME,
      "MIN_BACTERIACONC" = MIN_BACTERIACONC,
      "MIN_CONCENTRATION_REPLACE" = MIN_CONCENTRATION_REPLACE,
      "MIN_TIMESTEP" = MIN_TIMESTEP,
      "MONODTERMS" = MONODTERMS,
      "MONOD_REACTION_NAME" = MONOD_REACTION_NAME,
      "NAME" = NAME,
      "NAPL_CONTENT_INI" = NAPL_CONTENT_INI,
      "NAPL_CONTENT_RES" = NAPL_CONTENT_RES,
      "NAPL_PROPERTIES" = NAPL_PROPERTIES,
      "NO_REACTIONS" = NO_REACTIONS,
      "OMEGA_THRESHOLD" = OMEGA_THRESHOLD,
      "PRECIPITATION_BY_BASETERM_ONLY" = PRECIPITATION_BY_BASETERM_ONLY,
      "PRECIPITATION_EXPONENT" = PRECIPITATION_EXPONENT,
      "PRECIPITATION_FACTOR" = PRECIPITATION_FACTOR,
      "PRODUCTIONSTOCH" = PRODUCTIONSTOCH,
      "PRODUCTIONTERMS" = PRODUCTIONTERMS,
      "RATECONSTANT" = RATECONSTANT,
      "RATE_EXPONENTS" = RATE_EXPONENTS,
      "REACTION_DEACTIVATION" = REACTION_DEACTIVATION,
      "REACTION_ORDER" = REACTION_ORDER,
      "REACTIVE_SURFACE_AREA" = REACTIVE_SURFACE_AREA,
      "RELATIVE_ERROR" = RELATIVE_ERROR,
      "SCALE_DCDT" = SCALE_DCDT,
      "SHERWOOD_MODEL" = SHERWOOD_MODEL,
      "SOLVER_TYPE" = SOLVER_TYPE,
      "SORPTION_TYPE" = SORPTION_TYPE,
      "SORT_NODES" = SORT_NODES,
      "STANDARD_GIBBS_ENERGY" = STANDARD_GIBBS_ENERGY,
      "SURFACES" = SURFACES,
      "SWITCH_OFF_GEOMETRY" = SWITCH_OFF_GEOMETRY,
      "TEMPERATURE_DEPENDENCE" = TEMPERATURE_DEPENDENCE,
      "THRESHHOLDTERMS" = THRESHHOLDTERMS,
      "TORTUOSITY" = TORTUOSITY,
      "TYPE" = TYPE,
      "UI" = UI,
      "drmc" = drmc,
      "drmc_PARAMETERS" = drmc_PARAMETERS
    ) %>%
      purrr::discard(is.null) %>%
      purrr::discard(BBmisc::isFALSE) %>%
      structure(class = "ogs5_krc_bloc")

    valid_ogs5_krc_bloc(x$input$krc[[paste(krc_name)]])

    return(x)

  }
