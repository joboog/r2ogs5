# validates list for cct and cct_bloc class

create_ogs5_cct <- function(){
  
  list() %>% structure(class = "ogs5_cct")
  
}

valid_ogs5_cct <- function(x){
  
  if (!class(x)=="ogs5_cct"){
    stop(paste(x, " is not of class 'ogs5_cct' "), call. = FALSE)
  }
  
}

valid_ogs5_cct_bloc <- function(x){
  
  if (!class(x)=="ogs5_cct_bloc"){
    stop(paste(x, " is not of class 'ogs5_cct_bloc' "), call. = FALSE)
  }
  
}
