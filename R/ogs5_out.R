# validates list for st and st_initialcond class

#' create_ogs5_out
#' @description Constructor for the *ogs5_out* class
#' @return Object of class *ogs5_out*
create_ogs5_out <- function(){

  list() %>% structure(class = "ogs5_out")

}

#' valid_ogs5_out
#' @description  Validator for *ogs5_out* base class
#' @param x Object of class *ogs5_out* to validate.
valid_ogs5_out <- function(x){

  if (!class(x)=="ogs5_out"){
    stop(paste(x, " is not of class 'ogs5_out' "), call. = FALSE)
  }

}

#' valid_ogs5_out_bloc
#' @description  Validator for *ogs5_out_bloc* base class
#' @param x Object of class *ogs5_out_bloc* to validate.
valid_ogs5_out_bloc <- function(x){

  if (!class(x)=="ogs5_out_bloc"){
    stop(paste(x, " is not of class 'ogs5_out_bloc' "), call. = FALSE)
  }

}
