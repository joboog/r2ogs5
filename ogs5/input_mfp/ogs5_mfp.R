# validates list for mfp and mfp_bloc class

create_ogs5_mfp <- function(){
   
   list() %>% structure(class = "ogs5_mfp")
   
}

valid_ogs5_mfp <- function(x){
   
   if (!class(x)=="ogs5_mfp"){
      stop(paste(x, " is not of class 'ogs5_mfp' "), call. = FALSE)
   }
   
}

valid_ogs5_mfp_bloc <- function(x){
   
   if (!class(x)=="ogs5_mfp_bloc"){
      stop(paste(x, " is not of class 'ogs5_mfp_bloc' "), call. = FALSE)
   }
   
}
