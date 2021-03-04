# function to write ogs5-mkeybloc input files ----------------------------------

#' ogs5_write_inputfiles
#' @description Export *ogs5* object and write ogs5 input files.
#' @param ogs5_obj *ogs5* simulation object
#' @param type *character* Type of the *ogs5* main bloc to be written. c("all", "bc", "cct",
#'   "fct", "gem", "gli", "ic", "krc", "mcp", "mfp", "mmp", "msh", "msp", num",
#'   "out", "pcs", "pqc", "rei", "st", "tim", "rfd", "additional")
#' @param folderpath *character* Path to write the outpufile(s) to.
#' @export
#' @examples
#' tmp <- tempdir()
#' ogs5_obj <- create_ogs5(sim_name = "ex1", sim_id = 1L,
#'                         sim_path = paste0(tmp, "/ex1"))
#' ogs5_obj <- input_add_pcs_bloc(ogs5_obj, pcs_name = "PROCESS1",
#'                                PCS_TYPE = "GROUNDWATER_FLOW",
#'                                NUM_TYPE = "NEW",
#'                                ELEMENT_MATRIX_OUTPUT = "0")
#' ogs5_obj <- input_add_bc_bloc(ogs5_obj, bc_name = "BOUNDARY_CONDITION1",
#'                               PCS_TYPE = "GROUNDWATER_FLOW",
#'                               PRIMARY_VARIABLE = "HEAD",
#'                               GEO_TYPE = "POINT POINT0",
#'                               DIS_TYPE = "CONSTANT 1.0e5")
#'
#' ogs5_write_inputfiles(ogs5_obj, "all")
ogs5_write_inputfiles <-

  function(ogs5_obj = list(), type = "all", folderpath = NULL){

    # validate input
    valid_ogs5(ogs5_obj)

    if (!(any(type %in% c(names(ogs5_get_keywordlist()), "additional")) |
          any(type == "all"))) {
      stop("wrong type entered", call. = FALSE)
    }
    if (any(type == "dat")) {
      warning("This method is only tested for phreeqc.dat files")
    }

    if (is.null(folderpath)) {
      folderpath <- attributes(ogs5_obj)$sim_path
    }

    if (!(dir.exists(folderpath))) {
      dir.create(folderpath, recursive = TRUE)
    }

    if (any(length(type) == 1 & type == "all")) {

      # loop through ogs5-obj and print all sublists
      for (i in names(ogs5_obj$input)) {

       if (i == "additional") {
         # loop over ogs5$input$additional and print sub blocs as files

         ogs5_list <- ogs5_obj$input[[paste(i)]]
         ogs5_write_ogs5_additional(folderpath, ogs5_list)

       } else {
         # write file for specific ogs5_$input class
         filename <- paste0(folderpath, "/",
                               attributes(ogs5_obj)$sim_name, ".", i)

         ogs5_list <- ogs5_obj$input[[paste(i)]]
         ogs5_write_tofile(filename, ogs5_list_output(ogs5_list))
        }
      }

    } else { # if type != "all"

      for (i in type) {

      if (i == "additional") {
        # loop over ogs5$input$additional and print sub blocs as files

        ogs5_list <- ogs5_obj$input[[paste(i)]]
        ogs5_write_ogs5_additional(folderpath, ogs5_list)

      } else {
        filename <- paste0(folderpath, "/", attributes(ogs5_obj)$sim_name,
                           ".", i)

        ogs5_list <- ogs5_obj$input[[paste(i)]]
        ogs5_write_tofile(filename, ogs5_list_output(ogs5_list))
      }
        }
    }
}

#' ogs5_write_tofile
#' @description Write text output to file.
#' @param filename Path of file to be written.
#' @param text_output_fct Textoutput to be written.
ogs5_write_tofile <-

  function(filename = character(), text_output_fct){
    sink(filename, type = "output")
    text_output_fct
    sink()
  }


#' ogs5_print_mkey_bloc
#' @description Output *ogs5* main keyword bloc.
#' @param mkey_bloc *ogs5* main input bloc.
#' @param mkey Respective **ogs5** main keyword. *character*
#' @param stack If output should be on seperate lines. *logical*
ogs5_print_mkey_bloc <-

  function(mkey_bloc = list(), mkey = character(NULL), stack = FALSE) {

    skey_str <- sapply(
      c(1:length(mkey_bloc)),
      function(x) {
        if (stack) {
          paste0("\n", "$", names(mkey_bloc)[x], "\n ",
                 paste(mkey_bloc[[x]], collapse="\n")
          )
        } else {
          paste0("\n", "$", names(mkey_bloc)[x], "\n ",
                 paste(mkey_bloc[[x]], collapse=" ")
          )
        }
      }
    )
    cat(paste0("#", mkey), skey_str, "\n")
  }


# generic to output ogs5_sublist ------------------------------------------
#' ogs5_list_output
#' @description Output class specific sub bloc of *ogs5* object.
#' @param ogs5_sublist *ogs5* sub bloc.
#' @param ... Additional arguments to pass.
ogs5_list_output <- function(ogs5_sublist, ...) {
  UseMethod("ogs5_list_output")
}

## method to output simple sublist ---------------------------------------------
# ogs5_list_output.default <-
#   function(ogs5_sublist = list(), ogs5_mkey = character()){
#
#     for (i in seq_len(ogs5_sublist %>% length())){
#       ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
#                            mkey = ogs5_mkey)
#       cat("\n")
#     }
#     cat("#STOP", "\n")
#   }


# method for ogs5_ddc sublist ---------------------------------------------
ogs5_list_output.ogs5_ddc <-

  function(ogs5_sublist) {
    stopifnot(class(ogs5_sublist) == "ogs5_ddc")

    for (i in seq_len(ogs5_sublist %>% length())){

      mkey <- names(ogs5_sublist)[i] %>% stringr::str_remove(" \\d")
      mkey_nr <- names(ogs5_sublist)[i] %>% stringr::str_extract("\\d+")

      skey_str <- sapply(
        names(ogs5_sublist[[i]]),
        function(skey_name) {
            skey <- skey_name %>% stringr::str_remove(" \\d+")
            skey_nr <- skey_name %>% stringr::str_extract("\\d+")

            paste0("\n", "$", skey, " ", skey_nr, "\n ",
                   paste(ogs5_sublist[[i]][[skey_name]], collapse="\n")
            )
        }
      )
      cat(paste0("#", mkey, " ", mkey_nr), skey_str, "\n\n")
    }

    cat("#STOP\n")
    }


## method for ogs5_pcs sublist ------------------------------------------
ogs5_list_output.ogs5_pcs <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_pcs")

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = "PROCESS")
      cat("\n")
    }
    cat("#STOP", "\n")
  }

## method for ogs5_bc sublist ------------------------------------------
ogs5_list_output.ogs5_bc <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_bc")

    ogs5_mkey <- ogs5_get_keywordlist()$bc$mkey

    for (i in seq_len(ogs5_sublist %>% length())){

      if (any(names(ogs5_sublist[[i]]) == "DIS_TYPE") &
          any(stringr::str_detect(ogs5_sublist[[i]][["DIS_TYPE"]], "LINEAR"))) {

        ogs5_sublist[[i]][["DIS_TYPE"]] <- ogs5_sublist[[i]][["DIS_TYPE"]] %>%
          paste0(collapse = "  \n") # stack arguments
      }

      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

## method for ogs5_ic sublist ------------------------------------------
ogs5_list_output.ogs5_ic <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_ic")

    ogs5_mkey <- ogs5_get_keywordlist()$ic$mkey

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey,
                           stack=TRUE)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

## method for ogs5_cct sublist ------------------------------------------
ogs5_list_output.ogs5_cct <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_cct")
    # tibble to character vector
    ogs5_sublist <-
      ogs5_sublist %>%
      lapply(function(bloc){
        tibble_skeys <- which(names(bloc) == "NEIGHBOR")
        for(i in tibble_skeys) {
            bloc[[i]][[2]] <- bloc[[i]][[2]] %>%
            apply(1, function(row) {
              paste0(paste(row, collapse = " "), "\n")
            })
            bloc[[i]] <- c(paste0(bloc[[i]][[1]], "\n"),
                                    bloc[[i]][[2]])
        }
        return(bloc)
    })

    ogs5_mkey <- ogs5_get_keywordlist()$cct$mkey

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

## method for ogs5_fct sublist ------------------------------------------
ogs5_list_output.ogs5_fct <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_fct")

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_fct_bloc(mkey_bloc = ogs5_sublist[[i]],
                          mkey = "FUNCTION")
      cat("\n")
    }
    cat("#STOP", "\n")
  }

# output ogs5_fct bloc ----------------------------------------------------

#' ogs5_print_fct_bloc
#' @description Output *ogs5* **FCT** bloc.
#' @param mkey_bloc *ogs5* main input bloc.
#' @param mkey Respective **ogs5** main keyword. *character*
ogs5_print_fct_bloc <- function(mkey_bloc, mkey) {

  other_skeys <- which(!(names(mkey_bloc) %in% c("DATA", "MATRIX")))
  skey_str <- sapply(
    names(mkey_bloc)[other_skeys],
    function(x) {
      paste0("\n", "$", x, "\n ",
             paste(mkey_bloc[[x]], collapse=" ")
      )
    }
  )
  cat(paste0("#", mkey), skey_str, "\n")

  # paste DATA
  if (!(is.null(mkey_bloc[["DATA"]]))){
    cat(" $DATA\n")
    cat(apply(mkey_bloc[["DATA"]], 1, paste0, collapse=" "), sep = "\n")
  }
  # paste MATRIX
  if (!(is.null(mkey_bloc[["MATRIX"]]))){
    cat(" $MATRIX\n")
    cat(apply(mkey_bloc[["MATRIX"]], 1, paste0, collapse=" "), sep = "\n")
  }


}


ogs5_fct_bloc_output <-

  function(ogs5_fct_bloc){

    # check ogs5_sublist
    stopifnot(class(ogs5_fct_bloc) == "ogs5_fct_bloc")

    nn <- names(ogs5_fct_bloc)

    for (i in seq_len(length(ogs5_fct_bloc))){

      if (nn == "data_type") cat(ogs5_fct_bloc[[i]], "\n")

      if (nn == "data_values") {
        ogs5_print_fct_bloc(mkey_bloc = ogs5_fct_bloc[[i]],
                            mkey = "FUNCTION")
        cat("\n")
      }
    }
    cat("#STOP", "\n")
  }



# output ogs5_gem sublist ------------------------------------------
ogs5_list_output.ogs5_gem <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_gem")

    ogs5_mkey <- ogs5_get_keywordlist()$gem$mkey

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

# method for ogs5_gli sublist ------------------------------------------
ogs5_list_output.ogs5_gli <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_gli")

    for (i in seq_len(ogs5_sublist %>% length())){

      # print points
      if (stringr::str_detect(tolower(names(ogs5_sublist[i])), "points")) {

        cat("#POINTS\n")

        df <- ogs5_sublist[[i]] %>%
              tibble::rownames_to_column() %>%
              dplyr::mutate(rowname = as.numeric(.data$rowname)) %>%
              as.data.frame()

        if (any(colnames(df) == "name")) {
          noname <- which(df$name == "")
          df$name <- stringr::str_c("$NAME ", df$name)
          df$name[noname] <- ""
        }

        if (any(colnames(df) == "md")) {
          df$md <-stringr::str_c("$MD ", df$md)
        }
        names(df) <- NULL
        #df %>% print(row.names = TRUE)

        cat(" ", paste(colnames(df)), "\n")
        cat(apply(df, 1, paste0, collapse=" "), sep = "\n")

        cat("\n")
      }

      # print ply and srf
      if (stringr::str_detect(names(ogs5_sublist)[i], "POLYLINE") |
          stringr::str_detect(names(ogs5_sublist)[i], "SURFACE")) {

        ogs5_mkey = names(ogs5_sublist)[i] %>%
                    stringr::str_extract("[:alpha:]+")

        skey_str <- sapply(
          names(ogs5_sublist[[i]]),
          function(x) {
            if (x == "POINTS") {    # line breaks after each point
              skey <- paste0("\n", "$", x, "\n ",
                      paste(ogs5_sublist[[i]][[x]], collapse="\n "))
            } else {               # normal procedure, no line breaks
            skey <- paste0("\n", "$", x, "\n ",
                    paste(ogs5_sublist[[i]][[x]], collapse=" "))
            }
            return(skey)
        })

        cat(paste0("#", ogs5_mkey), skey_str, "\n")
          cat("\n")
        }
    }
    cat("#STOP", "\n")
  }

# output ogs5_krc sublist ------------------------------------------
ogs5_list_output.ogs5_krc <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_krc")

    mkeys <- names(ogs5_sublist) %>% stringr::str_extract("[:alpha:]+")
    # loop over sublists
    for (i in seq_along(mkeys)) {
        # check mkey
        if (!(mkeys[i] %in% ogs5_get_keywordlist()$krc$mkey)) {
          stop("undefined ogs5 mkey in krc list")
        }

        # print
        ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                             mkey = mkeys[i], stack = TRUE)
        cat("\n")
      }
    cat("#STOP", "\n")
}


## method for ogs5_mcp sublist ------------------------------------------
ogs5_list_output.ogs5_mcp <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_mcp")

    ogs5_mkey <- ogs5_get_keywordlist()$mcp$mkey

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

## method for ogs5_mfp sublist ------------------------------------------
ogs5_list_output.ogs5_mfp <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_mfp")

    ogs5_mkey <- ogs5_get_keywordlist()$mfp$mkey

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

# method for ogs5_mmp sublist ------------------------------------------
ogs5_list_output.ogs5_mmp <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_mmp")

    ogs5_mkey <- ogs5_get_keywordlist()$mmp$mkey

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey, stack = TRUE)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

# method for ogs5_msh sublist ------------------------------------------
ogs5_list_output.ogs5_msh <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_msh")

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_msh_mkey_bloc(mkey_bloc = ogs5_sublist[[i]])
      cat("\n")
    }
    cat("#STOP", "\n")
  }


#' ogs5_print_msh_mkey_bloc
#' @description Output *ogs5* **MSH** bloc.
#' @param mkey_bloc *ogs5* **MSH** input bloc.
ogs5_print_msh_mkey_bloc <-

  function(mkey_bloc = list()) {

    mkey <- "FEM_MSH"
    skeys <- names(mkey_bloc)[names(mkey_bloc) != "ELEMENTS" &
                                names(mkey_bloc) != "NODES"]
    skey_str <- sapply(
      skeys,
      function(x) {
        paste0("\n", "$", x, "\n ",
               paste(mkey_bloc[[x]], collapse=" ")
        )
      }
    )
    cat(paste0("#", mkey), skey_str, "\n")

    # print NODES
    df <- mkey_bloc$NODES %>%
          tibble::rownames_to_column() %>%
          dplyr::mutate(rowname = as.numeric(.data$rowname) - 1) %>%
          as.data.frame()
    rownames(df) <- rownames(df) %>% as.numeric() %>% -1
    names(df) <- NULL
    cat("$NODES\n", length(df[[1]]), "\n")
    #df %>% print(row.names = TRUE)
    #cat(" ", paste(colnames(df)), "\n")
    cat(apply(df, 1, paste0, collapse=" "), sep = "\n")

    # print ELEMENTS
    n_ele <- lapply(mkey_bloc$ELEMENTS, nrow) %>% unlist %>% sum
    cat("$ELEMENTS\n", n_ele, "\n")
    # loop over geometries
    for (geometry in names(mkey_bloc$ELEMENTS)) {

      df <- mkey_bloc$ELEMENTS[[paste0(geometry)]] %>%
        tidyr::replace_na(list(node3 = "", node4 = "", node5 = "",
                               node6 = "", node7 = "", node8 = "")) %>%
        as.data.frame()
      # rownames(df) <- rownames(df) %>% as.numeric() %>% -1
      names(df) <- NULL
      cat(apply(df, 1, paste0, collapse=" "), sep = "\n")
    }

  }

# method for ogs5_msp sublist ------------------------------------------
ogs5_list_output.ogs5_msp <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_msp")

    ogs5_mkey <- ogs5_get_keywordlist()$msp$mkey

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey,
                           stack = TRUE)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

# method for ogs5_num sublist ------------------------------------------
ogs5_list_output.ogs5_num <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_num")

    ogs5_mkey <- ogs5_get_keywordlist()$num$mkey

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

# method for ogs5_out sublist ------------------------------------------
ogs5_list_output.ogs5_out <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_out")

    ogs5_mkey <- ogs5_get_keywordlist()$out$mkey

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey,
                           stack = TRUE)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

# output ogs5_pqc sublist ------------------------------------------
ogs5_list_output.ogs5_pqc <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_pqc")

    for (i in seq_len(ogs5_sublist %>% length())) {

        mkey <- names(ogs5_sublist)[i]
        skey_str <- sapply(
            ogs5_sublist[[i]],
          function(x) {paste0("\n", x)}
        )

        cat(mkey, skey_str, "\n\n")
      }
    cat("END", "\n")
  }


# output ogs5_pqc_filebloc sublist ------------------------------------------
ogs5_list_output.ogs5_pqc_filebloc <-
  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_pqc_filebloc")

    for (i in seq_len(ogs5_sublist %>% length())) {

      mkey <- names(ogs5_sublist)[i]
      skey_str <- sapply(
        ogs5_sublist[[i]],
        function(x) {paste0("\n ", x)}
      )

      cat(mkey, skey_str, "\n\n\n")
    }
    cat("END", "\n")
  }

# output ogs5_rei sublist ------------------------------------------
ogs5_list_output.ogs5_rei <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_rei")

    ogs5_mkey <- ogs5_get_keywordlist()$rei$mkey

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("#STOP", "\n")
  }


# method for ogs5_rfd sublist ------------------------------------------
ogs5_list_output.ogs5_rfd <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_rfd")

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_rfd_mkey_bloc(mkey_bloc = ogs5_sublist[[i]])
      cat("\n")
    }
    cat("#STOP", "\n")
  }


ogs5_print_rfd_mkey_bloc <-

  function(mkey_bloc = list()) {

    mkey <- mkey_bloc$mkey

    if ("INTERPOLATION" %in% names(mkey_bloc)){
      int_str <- paste0("\n", "$INTERPOLATION", "\n",
                        paste(mkey_bloc$INTERPOLATION, collapse=" "))
    }else int_str <- ""

    if ("MSH_TYPE" %in% names(mkey_bloc)){
      msh_str <- paste0("\n", "$MSH_TYPE", "\n",
                        paste(mkey_bloc$MSH_TYPE, collapse=" "))
    }else msh_str <- ""

    cat(paste0("#", mkey), int_str, msh_str, "\n", "$DATA")

    df <- mkey_bloc$data %>%
            as.data.frame()

    cat(" ", paste(colnames(df)), "\n")
    cat(apply(df, 1, paste0, collapse=" "), sep = "\n")
    cat("\n")
  }


# method for ogs5_st sublist ------------------------------------------
ogs5_list_output.ogs5_st <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_st")

    ogs5_mkey <- ogs5_get_keywordlist()$st$mkey

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

# method for ogs5_tim sublist ------------------------------------------
ogs5_list_output.ogs5_tim <-

  function(ogs5_sublist){

    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_tim")

    ogs5_mkey <- ogs5_get_keywordlist()$tim$mkey

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey, stack = TRUE)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

# output ogs5_additional_bloc ------------------------------------------

#' ogs5_write_ogs5_additional
#' @description Output *ogs5* **additional** bloc.
#' @param folderpath Path to directory where to write out the file.
#' @param ogs5_list *ogs5* **additional** bloc.
ogs5_write_ogs5_additional <-

  function(folderpath, ogs5_list){

    # check ogs5_sublist
    stopifnot(class(ogs5_list) == "ogs5_additional")

    for (i in seq_len(ogs5_list %>% length())){

      ogs5_sublist <- ogs5_list[[i]]
      filename = paste0(folderpath, "/", names(ogs5_list)[i])

      if (class(ogs5_sublist) == "character") {
        ogs5_write_tofile(filename,
                          cat(paste0(ogs5_sublist, collapse = "\n")))
      }

      if (class(ogs5_sublist) == "ogs5_pqc_filebloc") {
        ogs5_write_tofile(filename, ogs5_list_output(ogs5_sublist))

      }
    }
  }
