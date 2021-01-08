# validates list for gem and gem_bloc class

#' create_ogs5_gem
#' @description Constructor for the *ogs5_gem* class
#' @return Object of class *ogs5_gem*
create_ogs5_gem <- function(){

  list() %>% structure(class = "ogs5_gem")

}

#' valid_ogs5_gem
#' @description  Validator for *ogs5_gem* base class
#' @param x Object of class *ogs5_gem* to validate.
valid_ogs5_gem <- function(x){

  if (!class(x)=="ogs5_gem"){
    stop(paste(x, " is not of class 'ogs5_gem' "), call. = FALSE)
  }

}

#' valid_ogs5_gem_bloc
#' @description  Validator for *ogs5_gem_bloc* base class
#' @param x Object of class *ogs5_gem_bloc* to validate.
valid_ogs5_gem_bloc <- function(x){

  if (!class(x)=="ogs5_gem_bloc"){
    stop(paste(x, " is not of class 'ogs5_gem_bloc' "), call. = FALSE)
  }

}
