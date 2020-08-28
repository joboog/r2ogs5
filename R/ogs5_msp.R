# validates list for msp and msp_bloc class

create_ogs5_msp <- function(){
   
   list() %>% structure(class = "ogs5_msp")
   
}

valid_ogs5_msp <- function(x){
   
   if (!class(x)=="ogs5_msp"){
      stop(paste(x, " is not of class 'ogs5_msp' "), call. = FALSE)
   }
   
}

valid_ogs5_msp_bloc <- function(x){
   
   if (!class(x)=="ogs5_msp_bloc"){
      stop(paste(x, " is not of class 'ogs5_msp_bloc' "), call. = FALSE)
   }
   
}
