# validates list for mfp and mfp_bloc class

#' create_ogs5_mfp
#' @description Constructor for the *ogs5_mfp* class
#' @return Object of class *ogs5_mfp*
create_ogs5_mfp <- function(){

   list() %>% structure(class = "ogs5_mfp")

}

#' valid_ogs5_mfp
#' @description  Validator for *ogs5_mfp* base class
#' @param x Object of class *ogs5_mfp* to validate.
valid_ogs5_mfp <- function(x){

   if (!class(x)=="ogs5_mfp"){
      stop(paste(x, " is not of class 'ogs5_mfp' "), call. = FALSE)
   }

}

#' valid_ogs5_mfp_bloc
#' @description  Validator for *ogs5_mfp_bloc* base class
#' @param x Object of class *ogs5_mfp_bloc* to validate.
valid_ogs5_mfp_bloc <- function(x){

   if (!class(x)=="ogs5_mfp_bloc"){
      stop(paste(x, " is not of class 'ogs5_mfp_bloc' "), call. = FALSE)
   }

}
