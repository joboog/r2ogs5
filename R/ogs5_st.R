# validates list for st and st_initialcond class

#' create_ogs5_st
#' @description Constructor for the *ogs5_st* class
#' @return Object of class *ogs5_st*
create_ogs5_st <- function(){

   list() %>% structure(class = "ogs5_st")

}

#' valid_ogs5_st
#' @description  Validator for *ogs5_st* base class
#' @param x Object of class *ogs5_st* to validate.
valid_ogs5_st <- function(x){

   if (!class(x)=="ogs5_st"){
      stop(paste(x, " is not of class 'ogs5_st' "), call. = FALSE)
   }

}

#' valid_ogs5_st_condition
#' @description  Validator for *ogs5_st_condition* base class
#' @param x Object of class *ogs5_st_condition* to validate.
valid_ogs5_st_condition <- function(x){

   if (!class(x)=="ogs5_st_condition"){
      stop(paste(x, " is not of class 'ogs5_st_condition' "), call. = FALSE)
   }

}
