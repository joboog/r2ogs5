# validates list for msp and msp_bloc class

#' create_ogs5_msp
#' @description Constructor for the *ogs5_msp* class
#' @return Object of class *ogs5_msp*
create_ogs5_msp <- function(){

   list() %>% structure(class = "ogs5_msp")

}

#' valid_ogs5_msp
#' @description  Validator for *ogs5_msp* base class
#' @param x Object of class *ogs5_msp* to validate.
valid_ogs5_msp <- function(x){

   if (!class(x)=="ogs5_msp"){
      stop(paste(x, " is not of class 'ogs5_msp' "), call. = FALSE)
   }

}

#' valid_ogs5_msp_bloc
#' @description  Validator for *ogs5_msp_bloc* base class
#' @param x Object of class *ogs5_msp_bloc* to validate.
valid_ogs5_msp_bloc <- function(x){

   if (!class(x)=="ogs5_msp_bloc"){
      stop(paste(x, " is not of class 'ogs5_msp_bloc' "), call. = FALSE)
   }

}
