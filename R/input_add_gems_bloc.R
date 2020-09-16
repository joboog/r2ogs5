# function to add gem-bloc to ogs5-obj
# input: ogs5-obj
# output: updated ogs5-obj

input_add_gem_bloc <- 
  
  function(
    x = list(),
    bloc_name = "GEMS_PROPERTIES",
    
    #skey
    CALCULATE_BOUNDARY_NODES = c("0","1"),
    DISABLE_GEMS = FALSE,
    FLAG_COUPLING_HYDROLOGY = c("0","1"),
    FLAG_DISABLE_GEM = NULL,
    FLAG_POROSITY_CHANGE = c("0","1"),
    GEM_CALCULATE_BOUNDARY_NODES = NULL,
    GEM_INIT_FILE = NULL,
    GEM_THREADS = 1,
    ITERATIVE_SCHEME = FALSE,
    KINETIC_GEM = NULL,
    MAX_FAILED_NODES = NULL,
    MAX_POROSITY = NULL,
    MIN_POROSITY = NULL,
    MY_SMART_GEMS = NULL,
    PRESSURE_GEM = NULL,
    TEMPERATURE_GEM = NULL,
    TRANSPORT_B = 1
    
  ){
    
    # validate input
    valid_ogs5(x)
    
    # look if ogs5-obj$input$gem exists and valid, otherwise create 
    if (!("gem" %in% names(x$input))) {
      x$input$gem <- create_ogs5_gem() 
    } else {
      
      valid_ogs5_gem(x$input$gem)
      
      if (length(names(x$input$gem) > 1)) {
        stop("only one GEMS bloc allowed", call. = FALSE)
      }
    }
    
    # match function arguments
    CALCULATE_BOUNDARY_NODES <- match.arg(CALCULATE_BOUNDARY_NODES)
    FLAG_COUPLING_HYDROLOGY <- match.arg(FLAG_COUPLING_HYDROLOGY)
    FLAG_POROSITY_CHANGE <- match.arg(FLAG_POROSITY_CHANGE)
    
    # create and add sublist to gem-list
    x$input$gem[[paste(bloc_name)]] <- list(
      
      "CALCULATE_BOUNDARY_NODES" = CALCULATE_BOUNDARY_NODES,
      "DISABLE_GEMS" = DISABLE_GEMS,
      "FLAG_COUPLING_HYDROLOGY" = FLAG_COUPLING_HYDROLOGY,
      "FLAG_DISABLE_GEM" = FLAG_DISABLE_GEM,
      "FLAG_POROSITY_CHANGE" = FLAG_POROSITY_CHANGE,
      "GEM_CALCULATE_BOUNDARY_NODES" = GEM_CALCULATE_BOUNDARY_NODES,
      "GEM_INIT_FILE" = GEM_INIT_FILE,
      "GEM_THREADS" = GEM_THREADS,
      "ITERATIVE_SCHEME" = ITERATIVE_SCHEME,
      "KINETIC_GEM" = KINETIC_GEM,
      "MAX_FAILED_NODES" = MAX_FAILED_NODES,
      "MAX_POROSITY" = MAX_POROSITY,
      "MIN_POROSITY" = MIN_POROSITY,
      "MY_SMART_GEMS" = MY_SMART_GEMS,
      "PRESSURE_GEM" = PRESSURE_GEM,
      "TEMPERATURE_GEM" = TEMPERATURE_GEM,
      "TRANSPORT_B" = TRANSPORT_B
      
    ) %>% 
      purrr::discard(is.null) %>% 
      purrr::discard(BBmisc::isFALSE) %>% 
      structure(class = "ogs5_gem_bloc")
    
    valid_ogs5_gem_bloc(x$input$gem[[paste(bloc_name)]])
    
    return(x)
    
  }