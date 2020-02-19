# import vtu output files

# load requirements -------------------------------------------------------

vtk <- import("vtk")
dsa <- import("vtk.numpy_interface.dataset_adapter")



# read multiple vtu files -------------------------------------------------

ogs_read_vtu_files_point_data <- function(filepath = character(),
                                          pcs_type = character(),
                                          variable_name = NULL){
  
  # content:
  # reads specific point data arrays from a series of vtu files in a specific
  # directory, series is specified by the $PCS_TYPE given in the
  # corresponding outbloc (ogs5_obj$input$out$...)
  #
  # pcs_type: name of the process defined in the ogs5_obj$input$out$.. bloc
  # variable_name: name of the array in the *.vtu files (optional)
  #
  # returns a list
  
  
  # check filepath
  if (!(dir.exists(filepath))){
    stop("'filepath' does not exist.", call = FALSE)
  }
  
  # list *.vtu files
  files <- list.files(
              path=filepath,
              pattern=".vtu",
              full.names=TRUE
            )[which(
              stringr::str_detect(
                string=list.files(
                  path=filepath,
                  pattern=".vtu",
                  full.names=TRUE
                ),
                pattern=c(pcs_type)
              )==TRUE
            )]
  
  # loop over file list
  l <- lapply(
    files,
    # read individual files
    function(x) {
      
      if (is.null(variable_name)) {
        ll <- ogs_read_vtu_file_point_data_all(filename = x)
      } else {
        ll <- ogs_read_vtu_file_point_data_array(filename = x,
                                                array_name = variable_name)
      }  
      return(ll)
    }
  )
  # return list for all files
  names(l) <- files %>% str_remove(pattern = paste0(filepath, "/"))
  return(l)
}

# read arrays from single *.vtu file --------------------------------------

# read specific array from single file
ogs_read_vtu_file_point_data_array <- 
    function(filename = character(),
             array_name = character()){
    
    # content:
    # this function reads a point data array from a
    # *.vtu output file and returns a numeric vector
    # filename: path+name of *.vtu file
    
    require(reticulate)
    
    # load vtu
    src <- vtk$vtkXMLUnstructuredGridReader()
    src$SetFileName(filename)
    src$Update()
    
    # extract data
    src_data <- dsa$WrapDataObject(src$GetOutput())
    src_data_arr <- src_data$PointData[array_name]
    
    src_data_ll <- list(src_data_arr)
    names(src_data_ll) <- array_name
    return(src_data_ll)
}

# read all arrays from single file
ogs_read_vtu_file_point_data_all <- 
    function(filename = character()){
        
        # content:
        # this function reads all point data array from a
        # *.vtu output file and returns a list 
        # filename: path+name of *.vtu file
        
        require(reticulate)
        
        # load vtu
        src <- vtk$vtkXMLUnstructuredGridReader()
        src$SetFileName(filename)
        src$Update()
        
        # extract data
        src_data <- dsa$WrapDataObject(src$GetOutput())
        src_data_keys <- src_data$PointData$keys()
        
        src_data_list <- lapply(as.list(src_data_keys),
                                function(x){
                                    l <- src_data$PointData[x]
                                    return(l)
                                })
        
        names(src_data_list) <- src_data_keys
        #src_data_tbl <- as_tibble(src_data_list) 
        return(src_data_list)
    }
