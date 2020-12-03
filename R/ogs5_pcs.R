# validates list for pcs and pcs_process

#' create_ogs5_pcs
#' @description Constructor for the *ogs5_pcs* class
#' @return Object of class *ogs5_pcs*
create_ogs5_pcs <- function(){

   list() %>% structure(class = "ogs5_pcs")

}

#' valid_ogs5_pcs
#' @description  Validator for *ogs5_pcs* base class
#' @param x Object of class *ogs5_pcs* to validate.
valid_ogs5_pcs <- function(x){

   if (!class(x)=="ogs5_pcs"){
      stop(paste(x, " is not of class 'ogs5_pcs' "), call. = FALSE)
   }

}

#' valid_ogs5_pcs_process
#' @description  Validator for *ogs5_pcs_process* base class
#' @param x Object of class *ogs5_pcs_process* to validate.
valid_ogs5_pcs_process <- function(x){

   if (!class(x)=="ogs5_pcs_process"){
      stop(paste(x, " is not of class 'ogs5_pcs_process' "), call. = FALSE)
   }

}
