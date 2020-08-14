# validates list for tim and tim_bloc class

create_ogs5_tim <- function(){
  
  list() %>% structure(class = "ogs5_tim")
  
}

valid_ogs5_tim <- function(x){
  
  if (!class(x)=="ogs5_tim"){
    stop(paste(x, " is not of class 'ogs5_tim' "), call. = FALSE)
  }
  
}

valid_ogs5_tim_bloc <- function(x){
  
  if (!class(x)=="ogs5_tim_bloc"){
    stop(paste(x, " is not of class 'ogs5_tim_bloc' "), call. = FALSE)
  }
  
}
