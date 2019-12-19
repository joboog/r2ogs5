# function to add cct-bloc to ogs5-obj
# input: ogs5-obj
# output: updated ogs5-obj

input_add_cct_bloc <- 
  
  function(
    x = list(),
    cct_name = character(NULL),
    
    MYRANK = NULL,
    NEIGHBOR = NULL,
    NNEIGHBORS = NULL
    
  ){
    
    # validate input
    valid_ogs5(x)
    
    # look if ogs5-obj$input$cct exists and valid, otherwise create 
    if (!("cct" %in% names(x$input))) {
      x$input$cct <- create_ogs5_cct() 
    } else {
      
      valid_ogs5_cct(x$input$cct)
      
      if (cct_name %in% names(x$input$cct)) {
        stop("cct_name does already exist", call. = FALSE)
      }
    }
    
    # create and add sublist to st-list
    
    x$input$cct[[paste(cct_name)]] <- list(
      
      "MYRANK" = MYRANK,
      "NEIGHBOR" = NEIGHBOR,
      "NNEIGHBORS" = NNEIGHBORS
      
    ) %>% 
      purrr::discard(is.null) %>% 
      structure(class = "ogs5_cct_bloc")
    
    valid_ogs5_cct_bloc(x$input$cct[[paste(cct_name)]])
    
    return(x)
    
  }