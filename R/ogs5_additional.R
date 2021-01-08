
#' create_ogs5_additional
#' @description Constructor for the *ogs5_additional* class
#' @return Object of class *ogs5_additional*
create_ogs5_additional <- function(){

    list() %>% structure(class = "ogs5_additional")

}

#' valid_ogs5_additional
#' @description  Validator for *ogs5_additional* base class
#' @param x Object of class *ogs5_additional* to validate.
valid_ogs5_additional <- function(x){

    if (!class(x)=="ogs5_additional"){
        stop(paste(x, " is not of class 'ogs5_additional' "), call. = FALSE)
    }

}

#' valid_ogs5_additional_bloc
#' @description  Validator for *ogs5_additional_bloc* base class
#' @param x Object of class *ogs5_additional_bloc* to validate.
valid_ogs5_additional_bloc <- function(x){

    if (!class(x)=="ogs5_additional_bloc"){
        stop(paste(x, " is not of class 'ogs5_additional_bloc' "), call. = FALSE)
    }

}
