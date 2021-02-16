# import vtu output files

# load requirements -------------------------------------------------------

vtk <- reticulate::import("vtk")
dsa <- reticulate::import("vtk.numpy_interface.dataset_adapter")



# read multiple vtu files -------------------------------------------------

#' ogs_read_vtu_files_point_data
#' @description Read specific point data arrays from a series of ***.vtu** files
#'    in a specific directory. The series is specified by the **PCS_TYPE**
#'    given in the corresponding **out** sub bloc (ogs5_obj$input$out$...).
#' @param filepath *character* Path to directory of ***.vtu* files.
#' @param pcs_type *character* Name of the process defined in the **out** sub bloc.
#' @param variable_name *character* Name of the array in the ***.vtu** files, optional.
#' @return *list*
ogs_read_vtu_files_point_data <- function(filepath,
                                          pcs_type,
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
  if (length(files) < 1){
    stop(paste0("Corresponding '*", pcs_type, "*.vtu' files do not exist",
                call. = FALSE))
  }

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
  names(l) <- files %>%stringr::str_remove(pattern = paste0(filepath, "/"))
  return(l)
}

# read arrays from single *.vtu file --------------------------------------

#' ogs_read_vtu_file_point_data_array
#' @description Read specific point data array from single ***.vtu** file.
#' @param filename *character* Path to ***.vtu** file.
#' @param array_name *charcter* Name of the array in the ***.vtu** file to be read.
#'
#'
#' @return *numeric* vector.
ogs_read_vtu_file_point_data_array <-
    function(filename = character(),
             array_name = character()){

    # content:
    # this function reads a point data array from a
    # *.vtu output file and returns a numeric vector
    # filename: path+name of *.vtu file

    # load vtu
    src <- vtk$vtkXMLUnstructuredGridReader()
    src$SetFileName(filename)
    src$Update()

    # extract data
    src_data <- dsa$WrapDataObject(src$GetOutput())
    src_data_arr <- src_data$PointData[array_name]

    timestep_time <- ogs5_extract_time_from_vtu(filename)

    # combine
    src_data_ll <- list(src_data_arr, timestep_time[1], timestep_time[2])
    names(src_data_ll) <- c(array_name, "time_step", "time")
    return(src_data_ll)
}


#' ogs_read_vtu_file_point_data_all
#' @description Read all point data array from a ***.vtu** output file.
#' @param filename *charcter* Path to ***.vtu** file.
#' @return *list*
ogs_read_vtu_file_point_data_all <-
    function(filename){

        # content:
        # this function reads all point data array from a
        # *.vtu output file and returns a list
        # filename: path+name of *.vtu file


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

        timestep_time <- ogs5_extract_time_from_vtu(filename)

        src_data_list <- append(src_data_list,c(timestep_time[1],
                                                timestep_time[2]))
        names(src_data_list) <- c(src_data_keys, "time_step", "time")
        #src_data_tbl <- tibble::as_tibble(src_data_list)
        return(src_data_list)
    }


# read geometry from *.vtu ------------------------------------------------

ogs_read_vtu_geometry <- function(filename = character()){

  # content:
  # reads geometric informatoin (nodes, cells, material id's) from *.vtu

  points <- list()
  cells <- list()

  # load vtu
  src <- vtk$vtkXMLUnstructuredGridReader()
  src$SetFileName(filename)
  src$Update()

  # get data
  src_data <- dsa$WrapDataObject(src$GetOutput())
  #src_data$CellTypes # element types?
  #src_data$CellLocations
  #src_data$Cells
  src_data$Points

  # combine
  ll <- list(points)
  return(ll)
}


# read time from *.vtu ----------------------------------------------------

#' ogs5_extract_time_from_vtu
#' @description Extract timesteps and time from ***.vtu** file.
#' @param filename *charcter* Path to file.
#' @return *numeric* vector.
ogs5_extract_time_from_vtu <- function(filename){

  # check filepath
  if (!(file.exists(filename))){
    stop("'filename' does not exist.", call = FALSE)
  }

  line_str <- readLines(con=filename, n=2L)

  timestep <- line_str %>%
                stringr::str_extract(pattern = "(?<=step: )\\d+") %>%
                stats::na.omit() %>% as.numeric()

  time <- line_str %>%
            stringr::str_extract(pattern = "(?<=Time: )\\d+\\.\\d+e\\+\\d+") %>%
            stats::na.omit() %>% as.numeric()

  timestep_time <- c("timestep"=timestep, "time_in_sec" = time)

  return(timestep_time)
}
