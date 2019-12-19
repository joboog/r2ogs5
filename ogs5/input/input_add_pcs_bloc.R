# add pcs-bloc to ogs5-obj

# steps:
# look if sub-list names pcs exists in input list
# if not, define a sub-list in input names pcs
# add pcs-blocs as additional list


input_add_pcs_bloc <- 
               
         function(
            x = list(),
            pcs_name = character(NULL),
            
            APP_TYPE = NULL,
            BOUNDARY_CONDITION_OUTPUT = FALSE,
            COUNT = NULL,
            CPL_TYPE = NULL,
            DEACTIVATED_SUBDOMAIN = NULL,
            DISSOLVED_CO2_INGAS_PCS_NAME = NULL,
            DISSOLVED_CO2_PCS_NAME = NULL,
            ELEMENT_MATRIX_OUTPUT = NULL,
            GEO_TYPE = NULL,
            MEDIUM_TYPE = NULL,
            MEMORY_TYPE = NULL,
            MSH_TYPE = NULL,
            NEGLECT_H_INI_EFFECT = NULL,
            NUM_TYPE = NULL,
            OutputMassOfGasInModel = FALSE,
            PCS_TYPE = character(NULL),
            PHASE_TRANSITION = NULL,
            PRIMARY_VARIABLE = NULL,
            PROCESSED_BC = NULL,
            RELOAD = NULL,
            SATURATION_SWITCH = FALSE,
            SAVE_ECLIPSE_DATA_FILES = FALSE,
            SIMULATOR = NULL,
            SIMULATOR_MODEL_PATH = NULL,
            SIMULATOR_PATH = NULL,
            SIMULATOR_WELL_PATH = NULL,
            ST_RHS = NULL,
            TIME_CONTROLLED_EXCAVATION = NULL,
            TIM_TYPE = NULL,
            UPDATE_INI_STATE = NULL,
            USE_PRECALCULATED_FILES = FALSE,
            USE_VELOCITIES_FOR_TRANSPORT = NULL
             ){
  
    # validate input
    valid_ogs5(x)
    
    # look if ogs5-obj$input$pcs exists and valid, otherwise create 
    if (!("pcs" %in% names(x$input))) {
      x$input$pcs <- create_ogs5_pcs() 
    } else {
       
       valid_ogs5_pcs(x$input$pcs)
       
       if (pcs_name %in% names(x$input$pcs)) {
          stop("pcs_name does already exist", call. = FALSE)
       }
       
       if (PCS_TYPE!="MASS_TRANSPORT" &&
           PCS_TYPE %in% sapply(x$input$pcs, "[[", 1)
          ) {
          stop("PCS_TYPE does already exist", call. = FALSE)
       }
    }
    
    # create and add sublist to pcs-list
    
   x$input$pcs[[paste(pcs_name)]] <- list(
       
      "APP_TYPE" = APP_TYPE,
       "BOUNDARY_CONDITION_OUTPUT" = BOUNDARY_CONDITION_OUTPUT,
       "COUNT" = COUNT,
       "CPL_TYPE" = CPL_TYPE,
       "DEACTIVATED_SUBDOMAIN" = DEACTIVATED_SUBDOMAIN,
       "DISSOLVED_CO2_INGAS_PCS_NAME" = DISSOLVED_CO2_INGAS_PCS_NAME,
       "DISSOLVED_CO2_PCS_NAME" = DISSOLVED_CO2_PCS_NAME,
       "ELEMENT_MATRIX_OUTPUT" = DISSOLVED_CO2_PCS_NAME,
       "GEO_TYPE" = GEO_TYPE,
       "MEDIUM_TYPE" = MEDIUM_TYPE,
       "MEMORY_TYPE" = MEMORY_TYPE,
       "MSH_TYPE" = MSH_TYPE,
       "NEGLECT_H_INI_EFFECT" = NEGLECT_H_INI_EFFECT,
       "NUM_TYPE" = NUM_TYPE,
       "OutputMassOfGasInModel" = OutputMassOfGasInModel,
       "PCS_TYPE" = PCS_TYPE,
       "PHASE_TRANSITION" = PHASE_TRANSITION,
       "PRIMARY_VARIABLE" =  PRIMARY_VARIABLE,
       "PROCESSED_BC" = PROCESSED_BC,
       "RELOAD" = RELOAD,
       "SATURATION_SWITCH" = SATURATION_SWITCH,
       "SAVE_ECLIPSE_DATA_FILES" = SAVE_ECLIPSE_DATA_FILES,
       "SIMULATOR" = SIMULATOR,
       "SIMULATOR_MODEL_PATH" = SIMULATOR_MODEL_PATH,
       "SIMULATOR_PATH" = SIMULATOR_PATH,
       "SIMULATOR_WELL_PATH" = SIMULATOR_WELL_PATH,
       "ST_RHS" = ST_RHS,
       "TIME_CONTROLLED_EXCAVATION" = TIME_CONTROLLED_EXCAVATION,
       "TIM_TYPE" = TIM_TYPE,
       "UPDATE_INI_STATE" = TIM_TYPE,
       "USE_PRECALCULATED_FILES" = TIM_TYPE,
       "USE_VELOCITIES_FOR_TRANSPORT" = USE_VELOCITIES_FOR_TRANSPORT
       ) %>% 
       purrr::discard(is.null) %>% 
       purrr::discard(isFALSE) %>% 
       structure(class = "ogs5_pcs_process")
    
    valid_ogs5_pcs_process(x$input$pcs[[paste(pcs_name)]])
   
    return(x)
   
}