# function to add mfp_component to ogs5-obj
# input: ogs5-obj
# output: updated ogs5-obj

input_add_mfp_bloc <- 
   
   function(
      x = list(),
      COMPONENTS = NULL,
      COMPRESSIBILITY = NULL,
      DAT_TYPE = NULL,
      DECAY = NULL,
      DENSITY = character(NULL),
      DIFFUSION = NULL,
      DRHO_DT_UNSATURATED = NULL,
      EOS_TYPE = NULL,
      FLUID_NAME = character(NULL),
      FLUID_TYPE = character(NULL),
      GRAVITY = NULL,
      HEAT_CONDUCTIVITY = NULL,
      ISOTHERM = NULL,
      JTC = NULL,
      NON_GRAVITY = NULL,
      PHASE_DIFFUSION = NULL,
      SPECIFIC_HEAT_CAPACITY = NULL,
      SPECIFIC_HEAT_SOURCE = NULL,
      TEMPERATURE = NULL,
      VISCOSITY = character(NULL)
      
   ){
      
      mfp_name <- FLUID_NAME
      
      # validate input
      valid_ogs5(x)
      
      # look if ogs5-obj$input$mfp eximfps and valid, otherwise create 
      if (!("mfp" %in% names(x$input))) {
         x$input$mfp <- create_ogs5_mfp() 
      } else {
         
         valid_ogs5_mfp(x$input$mfp)
         
         if (mfp_name %in% names(x$input$mfp)) {
            stop("mfp_name does already exist", call. = FALSE)
         }
         
      }
      
      # create and add sublist to mfp-list
      
      x$input$mfp[[paste(mfp_name)]] <- list(
         
         "COMPONENTS" = COMPONENTS,
         "COMPRESSIBILITY" = COMPRESSIBILITY,
         "DAT_TYPE" = DAT_TYPE,
         "DECAY" = DECAY,
         "DENSITY" = DENSITY,
         "DIFFUSION" = DIFFUSION,
         "DRHO_DT_UNSATURATED" = DRHO_DT_UNSATURATED,
         "EOS_TYPE" = EOS_TYPE,
         "FLUID_NAME" = FLUID_NAME,
         "FLUID_TYPE" = FLUID_TYPE,
         "GRAVITY" = GRAVITY,
         "HEAT_CONDUCTIVITY" = HEAT_CONDUCTIVITY,
         "ISOTHERM" = ISOTHERM,
         "JTC" = JTC,
         "NON_GRAVITY" = NON_GRAVITY,
         "PHASE_DIFFUSION" = PHASE_DIFFUSION,
         "SPECIFIC_HEAT_CAPACITY" = SPECIFIC_HEAT_CAPACITY,
         "SPECIFIC_HEAT_SOURCE" = SPECIFIC_HEAT_SOURCE,
         "TEMPERATURE" = TEMPERATURE,
         "VISCOSITY" = VISCOSITY
         
      ) %>% 
         purrr::discard(is.null) %>% 
         purrr::discard(isFALSE) %>% 
         structure(class = "ogs5_mfp_bloc")
      
      valid_ogs5_mfp_bloc(x$input$mfp[[paste(mfp_name)]])
      
      return(x)
      
   }