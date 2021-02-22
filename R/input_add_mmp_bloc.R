
#' input_add_mmp_bloc
#' @description Adds a sub-bloc to **mmp** bloc of *ogs5* for defining material
#'   properties. For additional documentatoin of the input parameters see
#'   the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/mmp.html)
#'   or have a look at the input scripts from the [r2ogs5-benchmarks repository](
#'   https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks).
#'   All arguments except **x** have to be of class *character*.
#' @param x Simulation object of class *ogs5*.
#' @param CAPILLARY_PRESSURE ogs5 **mmp** bloc sub key word.
#' @param CHANNEL ogs5 **mmp** bloc sub key word.
#' @param COMPOUND_DEPENDENT_DT ogs5 **mmp** bloc sub key word.
#' @param CONDUCTIVITY_MODEL ogs5 **mmp** bloc sub key word.
#' @param CONVERSION_FACTOR ogs5 **mmp** bloc sub key word.
#' @param DATA ogs5 **mmp** bloc sub key word.
#' @param DIFFUSION ogs5 **mmp** bloc sub key word.
#' @param DIS_TYPE ogs5 **mmp** bloc sub key word.
#' @param ELEMENT_VOLUME_MULTIPLYER ogs5 **mmp** bloc sub key word.
#' @param EVAPORATION ogs5 **mmp** bloc sub key word.
#' @param FLOWLINEARITY ogs5 **mmp** bloc sub key word.
#' @param GEOMETRY_AREA ogs5 **mmp** bloc sub key word.
#' @param GEOMETRY_DIMENSION ogs5 **mmp** bloc sub key word.
#' @param GEOMETRY_INCLINATION ogs5 **mmp** bloc sub key word.
#' @param GEO_TYPE ogs5 **mmp** bloc sub key word.
#' @param HEAT_DISPERSION ogs5 **mmp** bloc sub key word.
#' @param HEAT_TRANSFER ogs5 **mmp** bloc sub key word.
#' @param INTERPHASE_FRICTION ogs5 **mmp** bloc sub key word.
#' @param MASS_DISPERSION ogs5 **mmp** bloc sub key word.
#' @param MMP_TYPE ogs5 **mmp** bloc sub key word.
#' @param MSH_TYPE ogs5 **mmp** bloc sub key word.
#' @param NAME ogs5 **mmp** bloc sub key word.
#' @param ORGANIC_CARBON ogs5 **mmp** bloc sub key word.
#' @param PARTICLE_DIAMETER ogs5 **mmp** bloc sub key word.
#' @param PCS_TYPE ogs5 **mmp** bloc sub key word.
#' @param PERMEABILITY_DISTRIBUTION ogs5 **mmp** bloc sub key word.
#' @param PERMEABILITY_FUNCTION_DEFORMATION ogs5 **mmp** bloc sub key word.
#' @param PERMEABILITY_FUNCTION_EFFSTRESS ogs5 **mmp** bloc sub key word.
#' @param PERMEABILITY_FUNCTION_POROSITY ogs5 **mmp** bloc sub key word.
#' @param PERMEABILITY_FUNCTION_PRESSURE ogs5 **mmp** bloc sub key word.
#' @param PERMEABILITY_FUNCTION_STRAIN ogs5 **mmp** bloc sub key word.
#' @param PERMEABILITY_FUNCTION_STRESS ogs5 **mmp** bloc sub key word.
#' @param PERMEABILITY_FUNCTION_VELOCITY ogs5 **mmp** bloc sub key word.
#' @param PERMEABILITY_SATURATION ogs5 **mmp** bloc sub key word.
#' @param PERMEABILITY_TENSOR ogs5 **mmp** bloc sub key word.
#' @param POROSITY ogs5 **mmp** bloc sub key word.
#' @param POROSITY_DISTRIBUTION ogs5 **mmp** bloc sub key word.
#' @param RILL ogs5 **mmp** bloc sub key word.
#' @param SPECIFIC_STORAGE ogs5 **mmp** bloc sub key word.
#' @param STORAGE ogs5 **mmp** bloc sub key word.
#' @param STORAGE_FUNCTION_EFFSTRESS ogs5 **mmp** bloc sub key word.
#' @param SURFACE_FRICTION ogs5 **mmp** bloc sub key word.
#' @param TORTUOSITY ogs5 **mmp** bloc sub key word.
#' @param TRANSFER_COEFFICIENT ogs5 **mmp** bloc sub key word.
#' @param UNCONFINED ogs5 **mmp** bloc sub key word.
#' @param VOL_BIO ogs5 **mmp** bloc sub key word.
#' @param VOL_MAT ogs5 **mmp** bloc sub key word.
#' @param WIDTH ogs5 **mmp** bloc sub key word.
#' @return Updated *ogs5* object.
#' @export
#' @examples
#' ogs5_obj <- input_add_mmp_bloc(ogs5_obj, NAME = "MEDIUM_PROPERTIES1",
#'                                GEOMETRY_DIMENSION = "1",
#'                                GEOMETRY_AREA = "1.000000e+000",
#'                                POROSITY = "1 0.32",
#'                                VOL_MAT = "1 0.68",
#'                                TORTUOSITY = "1 1.000000e+000",
#'                                PERMEABILITY_TENSOR = "ISOTROPIC 1.15700e-12",
#'                                MASS_DISPERSION = "1 0.0067 0.1000")
input_add_mmp_bloc <-

   function(
      x = list(),
      CAPILLARY_PRESSURE = NULL,
      CHANNEL = FALSE,
      COMPOUND_DEPENDENT_DT = NULL,
      CONDUCTIVITY_MODEL = NULL,
      CONVERSION_FACTOR = NULL,
      DATA = NULL,
      DIFFUSION = NULL,
      DIS_TYPE = NULL,
      ELEMENT_VOLUME_MULTIPLYER = NULL,
      EVAPORATION = NULL,
      FLOWLINEARITY = NULL,
      GEOMETRY_AREA = NULL,
      GEOMETRY_DIMENSION = NULL,
      GEOMETRY_INCLINATION = NULL,
      GEO_TYPE = NULL,
      HEAT_DISPERSION = NULL,
      HEAT_TRANSFER = NULL,
      INTERPHASE_FRICTION = NULL,
      MASS_DISPERSION = NULL,
      MMP_TYPE = NULL,
      MSH_TYPE = NULL,
      NAME = NULL,
      ORGANIC_CARBON = NULL,
      PARTICLE_DIAMETER = NULL,
      PCS_TYPE = NULL,
      PERMEABILITY_DISTRIBUTION = NULL,
      PERMEABILITY_FUNCTION_DEFORMATION = NULL,
      PERMEABILITY_FUNCTION_EFFSTRESS = NULL,
      PERMEABILITY_FUNCTION_POROSITY = NULL,
      PERMEABILITY_FUNCTION_PRESSURE = NULL,
      PERMEABILITY_FUNCTION_STRAIN = NULL,
      PERMEABILITY_FUNCTION_STRESS = NULL,
      PERMEABILITY_FUNCTION_VELOCITY = NULL,
      PERMEABILITY_SATURATION = NULL,
      PERMEABILITY_TENSOR  = NULL,
      POROSITY = NULL,
      POROSITY_DISTRIBUTION = NULL,
      RILL = NULL,
      SPECIFIC_STORAGE = NULL,
      STORAGE = NULL,
      STORAGE_FUNCTION_EFFSTRESS = NULL,
      SURFACE_FRICTION = NULL,
      TORTUOSITY = NULL,
      TRANSFER_COEFFICIENT = NULL,
      UNCONFINED = NULL,
      VOL_BIO = NULL,
      VOL_MAT = NULL,
      WIDTH  = NULL

   ){

      mmp_name <- NAME

      # validate input
      valid_ogs5(x)

      # look if ogs5-objinput$mmp exists and valid, otherwise create
      if (!("mmp" %in% names(x$input))) {
         x$input$mmp <- create_ogs5_mmp()
      } else {

         valid_ogs5_mmp(x$input$mmp)

         if (mmp_name %in% names(x$input$mmp)) {
            stop("mmp_name does already exist", call. = FALSE)
         }

      }

      # create and add sublist to mmp-list

      x$input$mmp[[paste(mmp_name)]] <- list(

         "CAPILLARY_PRESSURE" = CAPILLARY_PRESSURE,
         "CHANNEL" = CHANNEL,
         "COMPOUND_DEPENDENT_DT" = COMPOUND_DEPENDENT_DT,
         "CONDUCTIVITY_MODEL" = CONDUCTIVITY_MODEL,
         "CONVERSION_FACTOR" = CONVERSION_FACTOR,
         "DATA" = DATA,
         "DIFFUSION" = DIFFUSION,
         "DIS_TYPE" = DIS_TYPE,
         "ELEMENT_VOLUME_MULTIPLYER" = ELEMENT_VOLUME_MULTIPLYER,
         "EVAPORATION" = EVAPORATION,
         "FLOWLINEARITY" = FLOWLINEARITY,
         "GEOMETRY_AREA" = GEOMETRY_AREA,
         "GEOMETRY_DIMENSION" = GEOMETRY_DIMENSION,
         "GEOMETRY_INCLINATION" = GEOMETRY_INCLINATION,
         "GEO_TYPE" = GEO_TYPE,
         "HEAT_DISPERSION" = HEAT_DISPERSION,
         "HEAT_TRANSFER" = HEAT_TRANSFER,
         "INTERPHASE_FRICTION" = INTERPHASE_FRICTION,
         "MASS_DISPERSION" = MASS_DISPERSION,
         "MMP_TYPE" = MMP_TYPE,
         "MSH_TYPE" = MSH_TYPE,
         "NAME" = NAME,
         "ORGANIC_CARBON" = ORGANIC_CARBON,
         "PARTICLE_DIAMETER" = PARTICLE_DIAMETER,
         "PCS_TYPE" = PCS_TYPE,
         "PERMEABILITY_DISTRIBUTION" = PERMEABILITY_DISTRIBUTION,
         "PERMEABILITY_FUNCTION_DEFORMATION" = PERMEABILITY_FUNCTION_DEFORMATION,
         "PERMEABILITY_FUNCTION_EFFSTRESS" = PERMEABILITY_FUNCTION_EFFSTRESS,
         "PERMEABILITY_FUNCTION_POROSITY" = PERMEABILITY_FUNCTION_POROSITY,
         "PERMEABILITY_FUNCTION_PRESSURE" = PERMEABILITY_FUNCTION_PRESSURE,
         "PERMEABILITY_FUNCTION_STRAIN" = PERMEABILITY_FUNCTION_STRAIN,
         "PERMEABILITY_FUNCTION_STRESS" = PERMEABILITY_FUNCTION_STRESS,
         "PERMEABILITY_FUNCTION_VELOCITY" = PERMEABILITY_FUNCTION_VELOCITY,
         "PERMEABILITY_SATURATION" = PERMEABILITY_SATURATION,
         "PERMEABILITY_TENSOR"  = PERMEABILITY_TENSOR,
         "POROSITY" = POROSITY,
         "POROSITY_DISTRIBUTION" = POROSITY_DISTRIBUTION,
         "RILL" = RILL,
         "SPECIFIC_STORAGE" = SPECIFIC_STORAGE,
         "STORAGE" = STORAGE,
         "STORAGE_FUNCTION_EFFSTRESS" = STORAGE_FUNCTION_EFFSTRESS,
         "SURFACE_FRICTION" = SURFACE_FRICTION,
         "TORTUOSITY" = TORTUOSITY,
         "TRANSFER_COEFFICIENT" = TRANSFER_COEFFICIENT,
         "UNCONFINED" = UNCONFINED,
         "VOL_BIO" = VOL_BIO,
         "VOL_MAT" = VOL_MAT,
         "WIDTH"  = WIDTH

      ) %>%
         purrr::discard(is.null) %>%
         purrr::discard(BBmisc::isFALSE) %>%
         structure(class = "ogs5_mmp_bloc")

      valid_ogs5_mmp_bloc(x$input$mmp[[paste(mmp_name)]])

      return(x)

   }
