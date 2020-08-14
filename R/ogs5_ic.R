# validates list for ic and ic_initialcond class

create_ogs5_ic <- function(){
   
   list() %>% structure(class = "ogs5_ic")
   
}

valid_ogs5_ic <- function(x){
   
   if (!class(x)=="ogs5_ic"){
      stop(paste(x, " is not of class 'ogs5_ic' "), call. = FALSE)
   }
   
}

valid_ogs5_ic_condition <- function(x){
   
   if (!class(x)=="ogs5_ic_condition"){
      stop(paste(x, " is not of class 'ogs5_ic_condition' "), call. = FALSE)
   }
   
}
