# validates list for pqc and pqc_bloc class

#' create_ogs5_bc
#' @description Constructor for the *ogs5_pqc* class
#' @return Object of class *ogs5_pqc*
create_ogs5_pqc <- function(){

  list() %>% structure(class = "ogs5_pqc")

}

#' valid_ogs5_pqc
#' @description  Validator for *ogs5_pqc* base class
#' @param x Object of class *ogs5_pqc* to validate.
valid_ogs5_pqc <- function(x){

  if (!class(x)=="ogs5_pqc"){
    stop(paste(x, " is not of class 'ogs5_pqc' "), call. = FALSE)
  }

}

#' valid_ogs5_pqc_skeybloc
#' @description  Validator for *ogs5_pqc_skeybloc* base class
#' @param x Object of class *ogs5_pqc_skeybloc* to validate.
valid_ogs5_pqc_skeybloc <- function(x){

  if (!class(x)=="ogs5_pqc_skeybloc"){
    stop(paste(x, " is not of class 'ogs5_pqc_skeybloc' "), call. = FALSE)
  }

}

#' valid_ogs5_pqc_filebloc
#' @description  Validator for *ogs5_pqc_filebloc* base class
#' @param x Object of class *ogs5_pqc_filebloc* to validate.
valid_ogs5_pqc_filebloc <- function(x){

  if (!class(x)=="ogs5_pqc_filebloc"){
    stop(paste(x, " is not of class 'ogs5_pqc_filebloc' "), call. = FALSE)
  }

}
