# validates list for ic and ic_initialcond class

#' create_ogs5_ic
#' @description Constructor for the *ogs5_ic* class
#' @return Object of class *ogs5_ic*
create_ogs5_ic <- function(){

   list() %>% structure(class = "ogs5_ic")

}

#' valid_ogs5_ic
#' @description  Validator for *ogs5_ic* base class
#' @param x Object of class *ogs5_ic* to validate.
valid_ogs5_ic <- function(x){

   if (!class(x)=="ogs5_ic"){
      stop(paste(x, " is not of class 'ogs5_ic' "), call. = FALSE)
   }

}

#' valid_ogs5_ic_condition
#' @description  Validator for *ogs5_ic_condition* base class
#' @param x Object of class *ogs5_ic_condition* to validate.
valid_ogs5_ic_condition <- function(x){

   if (!class(x)=="ogs5_ic_condition"){
      stop(paste(x, " is not of class 'ogs5_ic_condition' "), call. = FALSE)
   }

}
