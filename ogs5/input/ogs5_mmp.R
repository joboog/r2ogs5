# validates list for mmp and mmp_bloc class

create_ogs5_mmp <- function(){
   
   list() %>% structure(class = "ogs5_mmp")
   
}

valid_ogs5_mmp <- function(x){
   
   if (!class(x)=="ogs5_mmp"){
      stop(paste(x, " is not of class 'ogs5_mmp' "), call. = FALSE)
   }
   
}

valid_ogs5_mmp_bloc <- function(x){
   
   if (!class(x)=="ogs5_mmp_bloc"){
      stop(paste(x, " is not of class 'ogs5_mmp_bloc' "), call. = FALSE)
   }
   
}
