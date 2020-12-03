# validates list for mmp and mmp_bloc class

#' create_ogs5_mmp
#' @description Constructor for the *ogs5_mmp* class
#' @return Object of class *ogs5_mmp*
create_ogs5_mmp <- function(){

   list() %>% structure(class = "ogs5_mmp")

}

#' valid_ogs5_mmp
#' @description  Validator for *ogs5_mmp* base class
#' @param x Object of class *ogs5_mmp* to validate.
valid_ogs5_mmp <- function(x){

   if (!class(x)=="ogs5_mmp"){
      stop(paste(x, " is not of class 'ogs5_mmp' "), call. = FALSE)
   }

}

#' valid_ogs5_mmp_bloc
#' @description  Validator for *ogs5_mmp_bloc* base class
#' @param x Object of class *ogs5_mmp_bloc* to validate.
valid_ogs5_mmp_bloc <- function(x){

   if (!class(x)=="ogs5_mmp_bloc"){
      stop(paste(x, " is not of class 'ogs5_mmp_bloc' "), call. = FALSE)
   }

}
