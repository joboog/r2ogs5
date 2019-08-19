# function to add msp_component to ogs5-obj
# input: ogs5-obj
# output: updated ogs5-obj

input_add_msp_bloc <- 
   
   function(
    x = list(),
    BIOT_CONSTANT = NULL,
    CREEP = NULL,
    DENSITY = character(NULL),
    ELASTICITY = NULL,
    EXCAVATION = NULL,
    E_Function = NULL,
    GRAVITY_CONSTANT = NULL,
    MICRO_STRUCTURE_PLAS = NULL,
    NAME = character(NULL),
    NON_REACTIVE_FRACTION = NULL,
    PLASTICITY = NULL,
    REACTIVE_SYSTEM = NULL,
    SOLID_BULK_MODULUS = NULL,
    SPECIFIC_HEAT_SOURCE = NULL,
    STRESS_INTEGRATION_TOLERANCE = NULL,
    STRESS_UNIT = NULL,
    SWELLING_PRESSURE_TYPE = NULL,
    THERMAL = NULL,
    THRESHOLD_DEV_STR = NULL,
    TIME_DEPENDENT_YOUNGS_POISSON = NULL
   
   ){
      
      msp_name <- NAME
      
      # validate input
      valid_ogs5(x)
      
      # look if ogs5-objinput$msp exists and valid, otherwise create 
      if (!("msp" %in% names(x$input))) {
         x$input$msp <- create_ogs5_msp() 
      } else {
         
         valid_ogs5_msp(x$input$msp)
         
         if (msp_name %in% names(x$input$msp)) {
            stop("msp_name does already exist", call. = FALSE)
         }
         
      }
      
      # create and add sublist to msp-list
      
      x$input$msp[[paste(msp_name)]] <- list(
         
         "BIOT_CONSTANT" = BIOT_CONSTANT,
         "CREEP" = CREEP,
         "DENSITY" = DENSITY,
         "ELASTICITY" = ELASTICITY,
         "EXCAVATION" = EXCAVATION,
         "E_Function" = E_Function,
         "GRAVITY_CONSTANT" = GRAVITY_CONSTANT,
         "MICRO_STRUCTURE_PLAS" = MICRO_STRUCTURE_PLAS,
         "NAME" = NAME,
         "NON_REACTIVE_FRACTION" = NON_REACTIVE_FRACTION,
         "PLASTICITY" = PLASTICITY,
         "REACTIVE_SYSTEM" = REACTIVE_SYSTEM,
         "SOLID_BULK_MODULUS" = SOLID_BULK_MODULUS,
         "SPECIFIC_HEAT_SOURCE" = SPECIFIC_HEAT_SOURCE,
         "STRESS_INTEGRATION_TOLERANCE" = STRESS_INTEGRATION_TOLERANCE,
         "STRESS_UNIT" = STRESS_UNIT,
         "SWELLING_PRESSURE_TYPE" = SWELLING_PRESSURE_TYPE,
         "THERMAL" = THERMAL,
         "THRESHOLD_DEV_STR" = THRESHOLD_DEV_STR,
         "TIME_DEPENDENT_YOUNGS_POISSON" = TIME_DEPENDENT_YOUNGS_POISSON 
         
      ) %>% 
         purrr::discard(is.null) %>% 
         purrr::discard(isFALSE) %>% 
         structure(class = "ogs5_msp_bloc")
      
      valid_ogs5_msp_bloc(x$input$msp[[paste(msp_name)]])
      
      return(x)
      
   }