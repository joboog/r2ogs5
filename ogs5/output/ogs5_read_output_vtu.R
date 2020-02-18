# import vtu output files

# load requirements -------------------------------------------------------

vtk <- import("vtk")
dsa <- import("vtk.numpy_interface.dataset_adapter")



# read multiple vtu files -------------------------------------------------

# implement wrapper for ogs_read_vtu_file_point_data*
# combine all vtu array data in tibble including mesh grid
# implement for specific array and for all arrays
# attach to ogs5-object

# read arrays from single *.vtu file --------------------------------------

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
    
    return(as.vector(src_data_arr))
}

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
