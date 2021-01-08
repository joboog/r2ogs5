
#' create_ogs5_bc
#' @description Constructor for the *ogs5_bc* class
#' @return Object of class *ogs5_bc*
create_ogs5_bc <- function(){

   list() %>% structure(class = "ogs5_bc")

}

#' valid_ogs5_bc
#' @description  Validator for *ogs5_bc* base class
#' @param x Object of class *ogs5_bc* to validate.
valid_ogs5_bc <- function(x){

   if (!class(x)=="ogs5_bc"){
      stop(paste(x, " is not of class 'ogs5_bc' "), call. = FALSE)
   }

}

#' valid_ogs5_bc_condition
#' @description  Validator for *ogs5_bc_condition* base class
#' @param x Object of class *ogs5_bc_condition* to validate.
valid_ogs5_bc_condition <- function(x){

   if (!class(x)=="ogs5_bc_condition"){
      stop(paste(x, " is not of class 'ogs5_bc_condition' "), call. = FALSE)
   }

}
