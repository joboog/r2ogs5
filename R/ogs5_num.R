# validates list for num and num_bloc class

#' create_ogs5_num
#' @description Constructor for the *ogs5_num* class
#' @return Object of class *ogs5_num*
create_ogs5_num <- function(){

   list() %>% structure(class = "ogs5_num")

}

#' valid_ogs5_num
#' @description  Validator for *ogs5_num* base class
#' @param x Object of class *ogs5_num* to validate.
valid_ogs5_num <- function(x){

   if (!class(x)=="ogs5_num"){
      stop(paste(x, " is not of class 'ogs5_num' "), call. = FALSE)
   }

}

#' valid_ogs5_num_bloc
#' @description  Validator for *ogs5_num_bloc* base class
#' @param x Object of class *ogs5_num_bloc* to validate.
valid_ogs5_num_bloc <- function(x){

   if (!class(x)=="ogs5_num_bloc"){
      stop(paste(x, " is not of class 'ogs5_num_bloc' "), call. = FALSE)
   }

}
