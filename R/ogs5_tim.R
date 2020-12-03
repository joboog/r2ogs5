# validates list for tim and tim_bloc class

#' create_ogs5_tim
#' @description Constructor for the *ogs5_tim* class
#' @return Object of class *ogs5_tim*
create_ogs5_tim <- function(){

  list() %>% structure(class = "ogs5_tim")

}

#' valid_ogs5_tim
#' @description  Validator for *ogs5_tim* base class
#' @param x Object of class *ogs5_tim* to validate.
valid_ogs5_tim <- function(x){

  if (!class(x)=="ogs5_tim"){
    stop(paste(x, " is not of class 'ogs5_tim' "), call. = FALSE)
  }

}

#' valid_ogs5_tim_bloc
#' @description  Validator for *ogs5_tim_bloc* base class
#' @param x Object of class *ogs5_tim_bloc* to validate.
valid_ogs5_tim_bloc <- function(x){

  if (!class(x)=="ogs5_tim_bloc"){
    stop(paste(x, " is not of class 'ogs5_tim_bloc' "), call. = FALSE)
  }

}
