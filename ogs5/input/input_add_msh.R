input_add_msh_bloc <- 
  
  function(
    x = list(),
    msh_name = character(),
    AREA = NULL,
    AXISYMMETRY = NULL,
    CROSS_SECTION = NULL,
    ELEMENTS = tibble(), # material_id ele_type ele_1 ele2 (ele3, ele4)
    GEO_NAME = NULL,
    GEO_TYPE = NULL,
    LAYER = NULL,
    NODES = tibble(), # x y z
    PCS_TYPE = "NO_PCS"
  ){
    
    # validate input
    valid_ogs5(x)
    
    # look if ogs5-obj$input$msh exists and valid, otherwise create 
    if (!("msh" %in% names(x$input))) {
      x$input$msh <- create_ogs5_msh() 
    } else {
      
      valid_ogs5_msh(x$input$msh)
    
      if (msh_name %in% names(x$input$msh)) {
        stop("msh_name does already exist", call. = FALSE)
      }
      
      if (PCS_TYPE!="MASS_TRANSPORT" &&
          PCS_TYPE %in% sapply(x$input$msh, "[[", 1)
      ) {
        stop("PCS_TYPE does already exist", call. = FALSE)
      }
    }
    
    # check ELEMENTS
    if (!all(c("material_id", "ele_type", "node1", "node2") %in% names(ELEMENTS))){
      stop("ELEMENTS has to have headers: 'material_id', 'ele_type', 'node1', 'node2', ...",
           call. = FALSE)
    }
    
    # check NODES
    if (!all(c("x", "y", "z") %in% names(ELEMENTS))){
      stop("NODES has to have headers: 'x', 'y', 'z'",
           call. = FALSE)
    }
    
    # # check elements and node orders
    # if (rownames(ELEMENTS)[1] != 0){
    #   rownames(ELEMENTS) <- rownames(ELEMENTS) %>% as.numeric %>% -1
    # }
    # 
    # if (rownames(NODES)[1] != 0){
    #   rownames(NODES) <- rownames(NODES) %>% as.numeric %>% -1
    # }
    
    # create and add sublist to bc-list
    
    x$input$msh[[paste(msh_name)]] <- list(
      
      "AREA" = AREA,
      "AXISYMMETRY" = AXISYMMETRY,
      "CROSS_SECTION" = CROSS_SECTION,
      "ELEMENTS" = ELEMENTS,
      "GEO_NAME" = GEO_NAME,
      "GEO_TYPE" = GEO_TYPE,
      "LAYER" = LAYER,
      "NODES" = NODES,
      "PCS_TYPE" = PCS_TYPE
      
    ) %>% 
      purrr::discard(is.null) %>% 
      purrr::discard(isFALSE) %>% 
      structure(class = "ogs5_msh_bloc")
    
    valid_ogs5_msh_bloc(x$input$msh[[paste(msh_name)]])
    
    return(x)
    
  }