# validates list for fct and fct_bloc class

create_ogs5_fct <- function(){
  
  list() %>% structure(class = "ogs5_fct")
  
}

valid_ogs5_fct <- function(x){
  
  if (!class(x)=="ogs5_fct"){
    stop(paste(x, " is not of class 'ogs5_fct' "), call. = FALSE)
  }
  
}

valid_ogs5_fct_bloc <- function(x){
  
  if (!class(x)=="ogs5_fct_bloc"){
    stop(paste(x, " is not of class 'ogs5_fct_bloc' "), call. = FALSE)
  }
  
}
