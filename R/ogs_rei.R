# validates list for st and st_initialcond class

create_ogs5_rei <- function(){
  
  list() %>% structure(class = "ogs5_rei")
  
}

valid_ogs5_rei <- function(x){
  
  if (!class(x)=="ogs5_rei"){
    stop(paste(x, " is not of class 'ogs5_rei' "), call. = FALSE)
  }
  
}

valid_ogs5_rei_bloc <- function(x){
  
  if (!class(x)=="ogs5_rei_bloc"){
    stop(paste(x, " is not of class 'ogs5_rei_bloc' "), call. = FALSE)
  }
  
}
