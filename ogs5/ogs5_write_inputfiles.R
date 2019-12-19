# function to write ogs5-mkeybloc input files -------------------------------------


#  write input file -------------------------------------------------------
ogs5_write_inputfiles <-

  function(ogs5_obj = list(), type = "all"){

    # validate input
    valid_ogs5(ogs5_obj)
    if (!(type %in% names(ogs5_keywordlist) | type == "all")) {
      stop("wrong type entered", call. = FALSE)
    }
    
    if (type == "all") {
    
    # loop through ogs5-obj and print all sublists
     for (i in names(ogs5_obj$input)){
       filename <- paste0(attributes(ogs5_obj)$sim_name, ".", i)
       ogs5_list <- ogs5_obj$input[[paste(i)]]
       ogs5_write_tofile(filename, ogs5_list_output(ogs5_list))   
     }
    }
    else {
      
      filename <- paste0(attributes(ogs5_obj)$sim_name, ".", type)
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
#     cat("STOP", "\n")
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
    cat("STOP", "\n")
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
    cat("STOP", "\n")
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
    cat("STOP", "\n")
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
    cat("STOP", "\n")
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
    cat("STOP", "\n")
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
    cat("STOP", "\n")
  }


# output ogs5_gem sublist ------------------------------------------

# output ogs5_gli sublist ------------------------------------------

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
    cat("STOP", "\n")
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
    cat("STOP", "\n")
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
    cat("STOP", "\n")
  }
# output ogs5_msh sublist ------------------------------------------

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
    cat("STOP", "\n")
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
    cat("STOP", "\n")
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
    cat("STOP", "\n")
  }

# output ogs5_pqc sublist ------------------------------------------

# output ogs5_rei sublist ------------------------------------------

# output ogs5_rfd sublist ------------------------------------------

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
    cat("STOP", "\n")
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
    cat("STOP", "\n")
  }

