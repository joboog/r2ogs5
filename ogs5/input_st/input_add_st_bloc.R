# function to add st_condition-bloc to ogs5-obj
# input: ogs5-obj
# output: updated ogs5-obj

input_add_st_bloc <- 
   
   function(
      x = list(),
      st_name = character(NULL),
      
      AIR_BREAKING = NULL,
      CHANNEL = NULL,
      COMP_NAME = NULL,
      CONSTRAINED = NULL,
      DISTRIBUTE_VOLUME_FLUX = NULL,
      DIS_TYPE = character(NULL),
      EXPLICIT_SURFACE_WATER_PRESSURE = NULL,
      FCT_TYPE = NULL,
      GEO_TYPE = character(NULL),
      MSH_TYPE = NULL,
      NEGLECT_SURFACE_WATER_PRESSURE = NULL,
      NODE_AVERAGING = NULL,
      PCS_TYPE = character(NULL),
      PRIMARY_VARIABLE = character(NULL),
      TIME_INTERPOLATION = NULL,
      TIM_TYPE = NULL
      
   ){
      
      # validate input
      valid_ogs5(x)
      
      # look if ogs5-obj$input$st exists and valid, otherwise create 
      if (!("st" %in% names(x$input))) {
         x$input$st <- create_ogs5_st() 
      } else {
         
         valid_ogs5_st(x$input$st)
         
         if (st_name %in% names(x$input$st)) {
            stop("st_name does already exist", call. = FALSE)
         }
         
         if (PCS_TYPE!="MASS_TRANSPORT" &&
             PCS_TYPE %in% sapply(x$input$st, "[[", 1)
         ) {
            stop("PCS_TYPE does already exist", call. = FALSE)
         }
      }
      
      # create and add sublist to st-list
      
      x$input$st[[paste(st_name)]] <- list(
         
         "AIR_BREAKING" = AIR_BREAKING,
         "CHANNEL" = CHANNEL,
         "COMP_NAME" = COMP_NAME,
         "CONSTRAINED" = CONSTRAINED,
         "DISTRIBUTE_VOLUME_FLUX" = DISTRIBUTE_VOLUME_FLUX,
         "DIS_TYPE" = DIS_TYPE,
         "EXPLICIT_SURFACE_WATER_PRESSURE" = EXPLICIT_SURFACE_WATER_PRESSURE,
         "FCT_TYPE" = FCT_TYPE,
         "GEO_TYPE" = GEO_TYPE,
         "MSH_TYPE" = MSH_TYPE,
         "NEGLECT_SURFACE_WATER_PRESSURE" = NEGLECT_SURFACE_WATER_PRESSURE,
         "NODE_AVERAGING" = NODE_AVERAGING,
         "PCS_TYPE" = PCS_TYPE,
         "PRIMARY_VARIABLE" =  PRIMARY_VARIABLE,
         "TIME_INTERPOLATION" = TIME_INTERPOLATION,
         "TIM_TYPE" = TIM_TYPE
         
      ) %>% 
         purrr::discard(is.null) %>% 
         purrr::discard(isFALSE) %>% 
         structure(class = "ogs5_st_condition")
      
      valid_ogs5_st_condition(x$input$st[[paste(st_name)]])
      
      return(x)
      
   }