# validates list for pcs and pcs_process

create_ogs5_pcs <- function(){
   
   list() %>% structure(class = "ogs5_pcs")
   
}

valid_ogs5_pcs <- function(x){
                     
   if (!class(x)=="ogs5_pcs"){
      stop(paste(x, " is not of class 'ogs5_pcs' "), call. = FALSE)
   }
   
}

valid_ogs5_pcs_process <- function(x){
   
   if (!class(x)=="ogs5_pcs_process"){
      stop(paste(x, " is not of class 'ogs5_pcs_process' "), call. = FALSE)
   }
   
}
