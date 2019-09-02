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


# function to print ogs5_pcs sublist ------------------------------------------
ogs5_pcs_print_sublist <- 
  
  function(ogs5_sublist){
    
    # check ogs5_sublist
    stopifnot(class(ogs5_sublist) == "ogs5_pcs")
    
    ogs5_mkey <- "PROCESS"

    for (i in seq_len(ogs5_sublist %>% length())){
      ogs5_print_mkey_bloc(mkey_bloc = ogs5_sublist[[i]],
                           mkey = ogs5_mkey)
      cat("\n")
    }
    cat("STOP", "\n")
  }


  
# function to print an individual mkey bloc out of a ogs5_sublist --------
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
