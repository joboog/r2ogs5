# function to write ogs5-mkeybloc input files -------------------------------------

#  write input file -------------------------------------------------------
ogs5_write_inputfile <-

  function(ogs5_obj = list(), type = character()){

    # validate input
    valid_ogs5(ogs5_obj)
    stopifnot(type %in% names(ogs5_keywordlist))
    
    # define input file name and list
    filename <- paste0(attributes(ogs5_obj)$sim_name, ".", type)
    ogs5_list <- ogs5_obj$input[[paste(type)]]
    
    ogs5_write_tofile(filename, ogs5_list_output(ogs5_list))
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


# output ogs5_fct sublist ------------------------------------------

# output ogs5_gem sublist ------------------------------------------

# output ogs5_gli sublist ------------------------------------------

# output ogs5_ic sublist ------------------------------------------

# output ogs5_krc sublist ------------------------------------------

# output ogs5_mcp sublist ------------------------------------------

# output ogs5_mfp sublist ------------------------------------------

# output ogs5_mmp sublist ------------------------------------------

# output ogs5_msh sublist ------------------------------------------

# output ogs5_msp sublist ------------------------------------------

# output ogs5_num sublist ------------------------------------------

# output ogs5_out sublist ------------------------------------------

# output ogs5_pqc sublist ------------------------------------------

# output ogs5_rei sublist ------------------------------------------

# output ogs5_rfd sublist ------------------------------------------

# output ogs5_st sublist ------------------------------------------

# output ogs5_tim sublist ------------------------------------------


