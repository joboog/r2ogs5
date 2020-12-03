# validates list for cct and cct_bloc class

#' create_ogs5_cct
#' @description Constructor for the *ogs5_cct* class
#' @return Object of class *ogs5_cct*
create_ogs5_cct <- function(){

  list() %>% structure(class = "ogs5_cct")

}

#' valid_ogs5_cct
#' @description  Validator for *ogs5_cct* base class
#' @param x Object of class *ogs5_cct* to validate.
valid_ogs5_cct <- function(x){

  if (!class(x)=="ogs5_cct"){
    stop(paste(x, " is not of class 'ogs5_cct' "), call. = FALSE)
  }

}

#' valid_ogs5_cct_bloc
#' @description  Validator for *ogs5_cct_bloc* base class
#' @param x Object of class *ogs5_cct_bloc* to validate.
valid_ogs5_cct_bloc <- function(x){

  if (!class(x)=="ogs5_cct_bloc"){
    stop(paste(x, " is not of class 'ogs5_cct_bloc' "), call. = FALSE)
  }

}
