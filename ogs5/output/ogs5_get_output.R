# this functoin to attach output data frames read by 
# specific ogs5_read_XXX functio to the corresponding
# ogs5_object


ogs5_get_output_all <- function(ogs5 = list(),
                                out_filepath = NULL){ 

  # content:
  # functoin to import the output df's based on the definitions
  # in the ogs5$input$out blocs
  # scans the igs5$input$out blocs whichi output to get and
  # calls the respective ogs5_read_XXX function to actually
  # read the outputted *.tec or *.vtu files

  # validate input
  valid_ogs5(ogs5)
  valid_ogs5_out(ogs5$input$out)
  
  # check filepath
  if (is.null(out_filepath)){
    out_filepath <- attributes(ogs5)$sim_path
  } else {
    if (!(dir.exists(out_filepath))) {
      stop("Given 'out_filepath' does not exist", call. = FALSE)
    }
  }
  
  # scan ogs5$input$out blocs for potential output
  outbloc_names <- names(ogs5$input$out)
  if (is.null(outbloc_names)) {
    stop(paste0("There are no ",attributes(ogs5)$sim_name,
                "$input$out blocs defined."), call. = FALSE)
  }
  
  # check if output was already imported
  outbloc_names <- outbloc_names[which(!(
                    outbloc_names %in% names(ogs5$output)))]
  if (length(outbloc_names) == 0) {
    stop("The output was already imported", call. = FALSE)
  }
  
  # read output files
  for (i in outbloc_names){
    
    # check output type
    out_dat_type <- ogs5$input$out[[paste(i)]]$DAT_TYPE
    
    out_geo_object <- ogs5$input$out[[paste(i)]]$GEO_TYPE
    if (out_geo_object == "DOMAIN") out_geo_object <- "domain"
    
    out_pcs_type <- ogs5$input$out[[paste(i)]]$PCS_TYPE
    
    # get filenames
    out_filenamepattern <- c(attributes(ogs5)$sim_name,
                             out_geo_object, out_pcs_type)
    
    out_files <- list.files(path=out_filepath, pattern=".tec", 
                           full.names=TRUE)[
                             which(
                             stringr::str_detect(string=list.files(
                               path=out_filepath, pattern=".tec",
                               full.names=TRUE),
                                pattern=out_filenamepattern)==TRUE
                              )]
    
    out_data_list <- NULL
    
    # read output files
    if (out_dat_type == "PVD") {
      warning("get_output not yet implemented for type 'PVD'")
    }
    
    if (out_dat_type == "TECPLOT") {

      out_filelist <- as.list(out_files)
      out_data_list <- lapply(out_filelist, 
                         FUN = function(x){
                           df <- ogs5_read_tecplot(filename = x,
                                    geo_object = out_geo_object)
                           return(df)
                         })
    }
    
    # attach output data as list to ogs5$output$i
    if (!(is.null(out_data_list))){
      ogs5$output[[paste(i)]] <- out_data_list
      rm(out_data_list)
    }
  }
  return(ogs5)
}


# import specific output file ---------------------------------------------

# content:
# imports data from a user-defined specific output file
ogs5_get_output_specific <- function(ogs5 = list(), outbloc_names = character(),
                                    out_filepath = NULL){
  
  # content:
  # functoin to import the output df's based on the definitions
  # in the ogs5$input$out blocs
  # scans the igs5$input$out blocs whichi output to get and
  # calls the respective ogs5_read_XXX function to actually
  # read the outputted *.tec or *.vtu files
  
  # validate input
  valid_ogs5(ogs5)
  valid_ogs5_out(ogs5$input$out)
  lapply(as.list(outbloc_names), 
         FUN = function(x){
           valid_ogs5_out_bloc(ogs5$input$out[[paste(x)]])
         })
  
  # check filepath
  if (is.null(out_filepath)){
    out_filepath <- attributes(ogs5)$sim_path
  } else {
    if (!(dir.exists(out_filepath))) {
      stop("Given 'out_filepath' does not exist", call. = FALSE)
    }
  }
  
  # check if output was already imported
  outbloc_names <- outbloc_names[which(!(
    outbloc_names %in% names(ogs5$output)))]
  if (length(outbloc_names) == 0) {
    stop("The output was already imported", call. = FALSE)
  }
  
  # read output files
  for (i in outbloc_names){
    
    # check output type
    out_dat_type <- ogs5$input$out[[paste(i)]]$DAT_TYPE
    
    out_geo_object <- ogs5$input$out[[paste(i)]]$GEO_TYPE
    if (out_geo_object == "DOMAIN") out_geo_object <- "domain"
    
    out_pcs_type <- ogs5$input$out[[paste(i)]]$PCS_TYPE
    
    # get filenames
    out_filenamepattern <- c(attributes(ogs5)$sim_name,
                             out_geo_object, out_pcs_type)
    
    out_files <- list.files(path=out_filepath, pattern=".tec", 
                            full.names=TRUE)[
                              which(
                                stringr::str_detect(string=list.files(
                                  path=out_filepath, pattern=".tec",
                                  full.names=TRUE),
                                  pattern=out_filenamepattern)==TRUE
                              )]
    
    out_data_list <- NULL
    
    # read output files
    if (out_dat_type == "PVD") {
      warning("get_output not yet implemented for type 'PVD'")
    }
    
    if (out_dat_type == "TECPLOT") {
      
      out_filelist <- as.list(out_files)
      out_data_list <- lapply(out_filelist, 
                              FUN = function(x){
                                df <- ogs5_read_tecplot(filename = x,
                                                        geo_object = out_geo_object)
                                return(df)
                              })
    }
    
    # attach output data as list to ogs5$output$i
    if (!(is.null(out_data_list))){
      ogs5$output[[paste(i)]] <- out_data_list
      rm(out_data_list)
    }
  }
  return(ogs5)
}