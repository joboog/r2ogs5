# validates list for bc and bc_initialcond class

create_ogs5_msh <- function(){
  
  list() %>% structure(class = "ogs5_msh")
}

valid_ogs5_msh <- function(x){
  
  if (!class(x)=="ogs5_msh"){
    stop(paste(x, " is not of class 'ogs5_msh' "), call. = FALSE)
  }
}

valid_ogs5_msh_bloc <- function(x){
  
  if (!class(x)=="ogs5_msh_bloc"){
    stop(paste(x, " is not of class 'ogs5_msh_bloc' "), call. = FALSE)
  }
}

