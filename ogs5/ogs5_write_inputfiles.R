# function to write ogs5-mkeybloc input files -------------------------------------

#todo
# input: ogs5-mkey_bloc
# output: 

# write_ogs5_inputfile_bloc <- 
#   
#   function(x = list(), bloc_class = character()){
#     
#     
#     # check x
#     
#     # check bloc_class
#     
#     # write bloc
#     mkey = "PROCESS"
#     sapply(
#       names(a),
#       function(x) paste0("#", mkey, "\n", "$", x, "\n ", paste(a[[x]], collapse=""), "\n")
#       ) %>%
#       cat
#   }


#  write input file -------------------------------------------------------
# ogs5_write_inputfile <- 
#   
#   function(type = character()){
#     
#     # check type if of: pcs, tim ...
#     # define input file name
#     # grab input text_objectfct
#     # write file: ogs5_write_tofile
#     
#   }

ogs5_write_tofile <- 
  
  function(filename = character(), text_output_fct){
    #cat(text_output_fct)
    sink(filename, type = "output")
    text_output_fct
    sink()
  }


# output simple sublist ---------------------------------------------

ogs5_sublist_output <- 
  function(ogs5_sublist = list(), ogs5_mkey = character()){
    
    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("STOP", "\n")
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

  
# output ogs5_pcs sublist ------------------------------------------
ogs5_pcs_list_output <- 
  
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

# output ogs5_bc sublist ------------------------------------------
ogs5_bc_list_output <- 
  
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
  

# output ogs5_cct sublist ------------------------------------------
ogs5_cct_list_output <- 
  
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

# output ogs5_fct sublist ------------------------------------------
ogs5_fct_list_output <- 
  
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


