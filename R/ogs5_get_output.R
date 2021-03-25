
#' ogs5_get_output_all
#' @description Import all output of a **ogs5** simulation to a *ogs5* object.
#' @param ogs5 *ogs5* simulation object.
#' @param out_filepath *character* Path to output files. Default:
#'  'attributes(ogs5)$sim_path'.
#' @return Updated *ogs5* object.
#' @export
#' @examples
#' \dontrun{
#' sim1 <- ogs5_get_output_all(sim1)
#' }
ogs5_get_output_all <- function(ogs5,# = list(),
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
    out_pcs_type <- ogs5$input$out[[paste(i)]]$PCS_TYPE

    out_data_list <- NULL

    # read output files
    if (out_dat_type == "PVD") {

      out_data_list <- ogs_read_vtu_files_point_data(
                          filepath = out_filepath, pcs_type = out_pcs_type)

    }

    if (out_dat_type == "TECPLOT") {

      if (out_geo_object == "DOMAIN") out_geo_object <- "domain"

      # get filenames
      out_filenamepattern <- paste0(attributes(ogs5)$sim_name, "_",
                               out_geo_object,"_", out_pcs_type)
      # list *.tec files
      out_files <- list.files(path=out_filepath, pattern=".tec",
                              full.names=TRUE)[
                                which(
                                  stringr::str_detect(string=list.files(
                                    path=out_filepath, pattern=".tec",
                                    full.names=TRUE),
                                    pattern=out_filenamepattern)==TRUE
                                )]

      out_data_list <- lapply(out_files,
                         FUN = function(x){
                           l <- ogs5_read_tecplot(filepath = x,
                                    geo_object = out_geo_object)
                           return(l)
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


#' ogs5_get_output_specific
#' @description Import the output from an **ogs5** simulation defined in a
#' specific *ogs5* **out** sub-bloc. Add output to *ogs5* object.
#' @param ogs5 *ogs5* simulation object.
#' @param outbloc_names *Character* vector of the **out** sub bloc names.
#' @param out_filepath *character* Path to output files. Default:
#'  'attributes(ogs5)$sim_path'.
#' @return Updated *ogs5* object.
#' @export
#' @examples
#' \dontrun{
#' ex1 <- ogs5_get_output_specific(ex1, outbloc_names = "tracer")
#' }
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
    out_pcs_type <- ogs5$input$out[[paste(i)]]$PCS_TYPE

    out_data_list <- NULL

    # read output files
    if (out_dat_type == "PVD") {

      out_data_list <- ogs_read_vtu_files_point_data(
        filepath = out_filepath, pcs_type = out_pcs_type)
    }

    if (out_dat_type == "TECPLOT") {

      if (out_geo_object == "DOMAIN") out_geo_object <- "domain"

      # get filenames
      out_filenamepattern <- paste0(attributes(ogs5)$sim_name, "_",
                                    out_geo_object,"_", out_pcs_type)
      # list *.tec files
      out_files <- list.files(path=out_filepath, pattern=".tec",
                              full.names=TRUE)[
                                which(
                                  stringr::str_detect(string=list.files(
                                    path=out_filepath, pattern=".tec",
                                    full.names=TRUE),
                                    pattern=out_filenamepattern)==TRUE
                                )]

      out_data_list <- lapply(out_files,
                          FUN = function(x){
                            l <- ogs5_read_tecplot(filepath = x,
                                                   geo_object = out_geo_object)
                            return(l)
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
