# validates list for rfd and rfd_bloc class

create_ogs5_rfd <- function(){
  
  list() %>% structure(class = "ogs5_rfd")
  
}

valid_ogs5_rfd <- function(x){
  
  if (!class(x)=="ogs5_rfd"){
    stop(paste(x, " is not of class 'ogs5_rfd' "), call. = FALSE)
  }
  
}

valid_ogs5_rfd_bloc <- function(x){
  
  if (!class(x)=="ogs5_rfd_bloc"){
    stop(paste(x, " is not of class 'ogs5_rfd_bloc' "), call. = FALSE)
  }
  
}
