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

print_mkey_bloc <- function(bloc = list(), mkey = character(NULL)){
  
  skey_str <- sapply(
                names(bloc),
                function(x) {
                  paste0("\n", "$", x, "\n ",
                         paste(bloc[[x]], collapse="")
                  )
                }
              )
  
  cat(paste0("#", mkey), skey_str)
}
