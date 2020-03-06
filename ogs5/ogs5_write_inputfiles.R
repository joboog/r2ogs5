# function to write ogs5-mkeybloc input files -------------------------------------


#  write input file -------------------------------------------------------
ogs5_write_inputfiles <-

  function(ogs5_obj = list(), type = "all", folderpath = NULL){

    # validate input
    valid_ogs5(ogs5_obj)
    if (!(type %in% names(ogs5_keywordlist) | type == "all")) {
      stop("wrong type entered", call. = FALSE)
    }
    
    if (is.null(folderpath)){
      folderpath <- paste0(attributes(ogs5_obj)$sim_path, "/")
    }
    
    if (type == "all") {
    
    # loop through ogs5-obj and print all sublists
     for (i in names(ogs5_obj$input)){
       filename <- paste0(folderpath, attributes(ogs5_obj)$sim_name, ".", i)
       ogs5_list <- ogs5_obj$input[[paste(i)]]
       ogs5_write_tofile(filename, ogs5_list_output(ogs5_list))   
     }
    }
    else {
      
      filename <- paste0(folderpath, attributes(ogs5_obj)$sim_name, ".", type)
      ogs5_list <- ogs5_obj$input[[paste(type)]]
      ogs5_write_tofile(filename, ogs5_list_output(ogs5_list))  
    }
  }

ogs5_write_tofile <- 
  
  function(filename = character(), text_output_fct){
    sink(filename, type = "output")
    text_output_fct
    sink()
  }

# output an individual mkey bloc out of a ogs5_sublist -------------
# input: mkey_bloc = main keywword bloc; mkey = main (#) key word
# define as class specific method?
ogs5_print_mkey_bloc <- 
  
  function(mkey_bloc = list(), mkey = character(NULL)){
    
    skey_str <- sapply(
      names(mkey_bloc),
      function(x) {
        paste0("\n", "$", x, "\n ",
               paste(mkey_bloc[[x]], collapse=" ")
        )
      }
    )
    
    cat(paste0("#", mkey), skey_str, "\n")
  }


# generic to output ogs5_sublist ------------------------------------------
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
    
    ogs5_mkey <- ogs5_keywordlist$bc$mkey
    
    for (i in seq_len(ogs5_sublist %>% length())){
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
    
    ogs5_mkey <- ogs5_keywordlist$ic$mkey
    
    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

## method for ogs5_cct sublist ------------------------------------------
ogs5_list_output.ogs5_cct <- 
  
  function(ogs5_sublist){
    
    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_cct")
    
    ogs5_mkey <- ogs5_keywordlist$cct$mkey
    
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
      ogs5_print_fct_bloc(ogs5_fct_bloc = ogs5_sublist[[i]],
                          mkey = "FUNCTION")
      cat("\n")
    }
    cat("#STOP", "\n")
  }


# output ogs5_fct bloc ----------------------------------------------------
ogs5_fct_bloc_output <- 
  
  function(ogs5_fct_bloc){
    
    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_fct_bloc")
    
    nn <- names(ogs5_fct_bloc)
    
    for (i in seq_len(length(ogs5_fct_bloc))){
      
      if (nn == "data_type") cat(ogs5_fct_bloc[[i]], "\n")
      
      if (nn == "data_values") 
      ogs5_print_fct_bloc(fct_bloc = ogs5_sublist[[i]],
                          mkey = "FUNCTION")
      cat("\n")
    }
    cat("#STOP", "\n")
  }


# output ogs5_gem sublist ------------------------------------------

# method for ogs5_gli sublist ------------------------------------------
ogs5_list_output.ogs5_gli <- 
  
  function(ogs5_sublist){
    
    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_gli")
    
    for (i in seq_len(ogs5_sublist %>% length())){
      
      # print points
      if ("points" == names(ogs5_sublist[i])){
        
        cat("#POINTS\n")
        
        df <- ogs5_sublist[[i]] %>% 
              as.data.frame() 
        df$name <- str_c("$NAME ", df$name)
        names(df) <- NULL
        #df %>% print(row.names = TRUE)
        
        cat(" ", paste(colnames(df)), "\n")
        cat(apply(df, 1, paste0, collapse=" "), sep = "\n")
        
        cat("\n")
      }
      
      # print ply and srf
      if ("polylines" == names(ogs5_sublist[i])){
        
        ogs5_mkey = "POLYLINE"
        for (j in seq_len(ogs5_sublist[[i]] %>% length())){
          ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]][[j]],
                               mkey = ogs5_mkey)
          cat("\n")
        }
      }
      
      if ("surfaces" == names(ogs5_sublist[i])){
        
        ogs5_mkey = "SURFACE"
        for (j in seq_len(ogs5_sublist[[i]] %>% length())){
          ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]][[j]],
                               mkey = ogs5_mkey)
          cat("\n")
        }
      }
    }
    cat("#STOP", "\n")
  }


# output ogs5_ic sublist ------------------------------------------

# output ogs5_krc sublist ------------------------------------------

## method for ogs5_mcp sublist ------------------------------------------
ogs5_list_output.ogs5_mcp <- 
  
  function(ogs5_sublist){
    
    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_mcp")
    
    ogs5_mkey <- ogs5_keywordlist$mcp$mkey
    
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
    
    ogs5_mkey <- ogs5_keywordlist$mfp$mkey
    
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
    
    ogs5_mkey <- ogs5_keywordlist$mmp$mkey
    
    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
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
          rownames_to_column() %>% 
          mutate(rowname = as.numeric(rowname) - 1) %>% 
          as.data.frame()
    rownames(df) <- rownames(df) %>% as.numeric() %>% -1
    names(df) <- NULL
    cat("$NODES\n", length(df[[1]]), "\n")
    #df %>% print(row.names = TRUE)
    #cat(" ", paste(colnames(df)), "\n")
    cat(apply(df, 1, paste0, collapse=" "), sep = "\n")
    
    # print ELEMENTS
    df <- mkey_bloc$ELEMENTS %>%
          rownames_to_column() %>% 
          mutate(rowname = as.numeric(rowname) - 1) %>% 
          replace_na(list(node3 = "", node4 = "", node5 = "",
                          node6 = "", node7 = "", node8 = "")) %>% 
          as.data.frame() 
    rownames(df) <- rownames(df) %>% as.numeric() %>% -1
    names(df) <- NULL
    cat("$ELEMENTS\n", length(df[[1]]), "\n")
    #df %>% print(row.names = TRUE)
    #cat(" ", paste(colnames(df)), "\n")
    cat(apply(df, 1, paste0, collapse=" "), sep = "\n")
  }

# method for ogs5_msp sublist ------------------------------------------
ogs5_list_output.ogs5_msp <- 
  
  function(ogs5_sublist){
    
    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_msp")
    
    ogs5_mkey <- ogs5_keywordlist$msp$mkey
    
    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

# method for ogs5_num sublist ------------------------------------------
ogs5_list_output.ogs5_num <- 
  
  function(ogs5_sublist){
    
    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_num")
    
    ogs5_mkey <- ogs5_keywordlist$num$mkey
    
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
    
    ogs5_mkey <- ogs5_keywordlist$out$mkey
    
    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

# output ogs5_pqc sublist ------------------------------------------

# output ogs5_rei sublist ------------------------------------------

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
    
    ogs5_mkey <- ogs5_keywordlist$st$mkey
    
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
    
    ogs5_mkey <- ogs5_keywordlist$tim$mkey
    
    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("#STOP", "\n")
  }

