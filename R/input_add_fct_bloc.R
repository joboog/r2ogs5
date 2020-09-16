# function to add fct-bloc to ogs5-obj
# input: ogs5-obj
# output: updated ogs5-obj

input_add_fct_bloc <- 
  
  function(
    x = list(),
    data_type = c("DATA", "MATRIX"),
    data_values =tibble::tibble(NULL),
    
    #skey
    DIMENSION = NULL,
    DIS_TYPE = NULL,
    GEO_TYPE = NULL,
    TYPE = character(NULL)
    
  ){
    
    fct_name <- TYPE
    
    # validate input
    valid_ogs5(x)
    
    # look if ogs5-obj$input$fct exists and valid, otherwise create 
    if (!("fct" %in% names(x$input))) {
      x$input$fct <- create_ogs5_fct() 
    } else {
      
      valid_ogs5_fct(x$input$fct)
      
      if (fct_name %in% names(x$input$fct)) {
        stop("fct_name does already exist", call. = FALSE)
      }
    }
    
    if (data_type == "MATRIX" &
        DIMENSION %>% is.null){
      stop("DIMENSION is missing", call. = FALSE)
    }
    
    # create and add sublist to st-list
    
    x$input$fct[[paste(fct_name)]] <- list(
      
      "data_type" = data_type,
      "data_values" = data_values,
      "DIMENSION" = DIMENSION,
      "DIS_TYPE" = DIS_TYPE,
      "GEO_TYPE" = GEO_TYPE,
      "TYPE" = TYPE
      
    ) %>% 
      purrr::discard(is.null) %>% 
      structure(class = "ogs5_fct_bloc")
    
    valid_ogs5_fct_bloc(x$input$fct[[paste(fct_name)]])
    
    return(x)
    
  }