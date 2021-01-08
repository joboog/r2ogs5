# validates list for bc and bc_initialcond class

#' create_ogs5_msh
#' @description Constructor for the *ogs5_msh* class
#' @return Object of class *ogs5_msh*
create_ogs5_msh <- function(){

  list() %>% structure(class = "ogs5_msh")
}

#' valid_ogs5_msh
#' @description  Validator for *ogs5_msh* base class
#' @param x Object of class *ogs5_msh* to validate.
valid_ogs5_msh <- function(x){

  if (!class(x)=="ogs5_msh"){
    stop(paste(x, " is not of class 'ogs5_msh' "), call. = FALSE)
  }
}

#' valid_ogs5_msh_bloc
#' @description  Validator for *ogs5_msh_bloc* base class
#' @param x Object of class *ogs5_msh_bloc* to validate.
valid_ogs5_msh_bloc <- function(x){

  if (!class(x)=="ogs5_msh_bloc"){
    stop(paste(x, " is not of class 'ogs5_msh_bloc' "), call. = FALSE)
  }
}

