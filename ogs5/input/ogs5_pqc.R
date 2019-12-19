# validates list for pqc and pqc_bloc class

create_ogs5_pqc <- function(){
  
  list() %>% structure(class = "ogs5_pqc")
  
}

valid_ogs5_pqc <- function(x){
  
  if (!class(x)=="ogs5_pqc"){
    stop(paste(x, " is not of class 'ogs5_pqc' "), call. = FALSE)
  }
  
}

valid_ogs5_pqc_skeybloc <- function(x){
  
  if (!class(x)=="ogs5_pqc_skeybloc"){
    stop(paste(x, " is not of class 'ogs5_pqc_skeybloc' "), call. = FALSE)
  }
  
}

valid_ogs5_pqc <- function(x){
  
  if (!class(x)=="ogs5_pqc_filebloc"){
    stop(paste(x, " is not of class 'ogs5_pqc_filebloc' "), call. = FALSE)
  }
  
}