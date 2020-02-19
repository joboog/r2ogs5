# function to process and extract data at specific locations 
# from list that have been imported from *.vtu files 


# extract data from lists -------------------------------------------------

ogs5_read_data_at_nodes <- 
  
  function(ogs5 = list(), outbloc_name = character(),
           node_coords = NULL){
    
    # content
    # extracts data from list read from *.vtu files
    # data is extracted at given node locations
    #
    # ogs5: ogs5-object
    # outbloc_name: name of bloc at ogs5$output$out that contains the lists
    # node_coords: coordinates of nodes where data is to be extracted
    #
    # returns a tibble
    
    # validate
    valid_ogs5(ogs5)
    valid_ogs5_out(ogs5$input$out)
    valid_ogs5_out_bloc(ogs5$input$out[[paste0(outbloc_name)]])
    
    if (is.null(ogs5$output[[paste0(outbloc_name)]])) {
      stop(paste0(ogs5, "$output$", outbloc_name, " is NULL."), call. = FALSE)
    }
    
    # get node indices
    nodes_tbl <- ogs5$input$msh$base_mesh$NODES %>% 
                  mutate(node_id = as.numeric(rownames(.)))
    
    if (!(is.null(node_coords))){
      nodes_tbl <-  nodes_tbl %>% semi_join(node_coords)  
    } else {
      warning(paste0("You did not specifiy any nodes.",
                   "Data will be extracted at all nodes"))
    }
    
    # extract data at nodes_id
    # loop over timestep specific lists
    data_df <- map_dfr(ogs5$output[[paste0(outbloc_name)]], .f= function(x1){
                  # loop over paramter specific lists
                  l <- lapply(x1,
                          FUN = function(x2){
                            if (length(x2)<= 1) x3 <- x2
                            if (class(x2)=="array") x3 <- x2[nodes_tbl$node_id]
                            else { 
                              if (class(x2)=="matrix" & length(nodes_tbl$node_id)==1){
                                x3 <- x2[nodes_tbl$node_id,] %>% 
                                        matrix(ncol = dim(x2)[2])
                              } else
                                  if (class(x2)=="matrix") x3 <- x2[nodes_tbl$node_id,]
                            }
                            return(x3)
                          })
                  df <- l %>% as.data.frame()
                  df$x <- nodes_tbl$x
                  df$y <- nodes_tbl$y
                  df$z <- nodes_tbl$z
                  
                  return(df)
                }, .id = "filename")
    
    data_tbl <- as_tibble(data_df)
    return(data_tbl) 
  }


# process data and add to ogs5-object -------------------------------------

# ogs5_import_data_at_nodes <- 
#   
#   function(ogs5 = list, 
#            outbloc_name = character(),
#            node_coords = tibble()) {
#   
#     # process tibble
#     # add timestep column
#     # add time column
#     data_tbl <- ogs5_read_data_at_nodes(ogs5 = ogs5, outbloc_name = outbloc,
#                                         node_coords = node_coords)
#     
#     data_tbl <- data_tbl %>% 
#                   mutate(timestep = str_extract(str_name, 
#                                                 pattern = "\\d+(?=\\.)")
#                   )
#     
#     # add to ogs5 as list with process name
#   
# }
