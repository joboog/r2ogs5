# validates list for krc and krc_bloc class

#' create_ogs5_krc
#' @description Constructor for the *ogs5_krc* class
#' @return Object of class *ogs5_krc*
create_ogs5_krc <- function(){

  list() %>% structure(class = "ogs5_krc")

}

#' valid_ogs5_krc
#' @description  Validator for *ogs5_krc* base class
#' @param x Object of class *ogs5_krc* to validate.
valid_ogs5_krc <- function(x){

  if (!class(x)=="ogs5_krc"){
    stop(paste(x, " is not of class 'ogs5_krc' "), call. = FALSE)
  }

}

#' valid_ogs5_krc_bloc
#' @description  Validator for *ogs5_krc_bloc* base class
#' @param x Object of class *ogs5_krc_bloc* to validate.
valid_ogs5_krc_bloc <- function(x){

  if (!class(x)=="ogs5_krc_bloc"){
    stop(paste(x, " is not of class 'ogs5_krc_bloc' "), call. = FALSE)
  }

}
