# validates list for bc and bc_initialcond class

create_ogs5_bc <- function(){
   
   list() %>% structure(class = "ogs5_bc")
   
}

valid_ogs5_bc <- function(x){
   
   if (!class(x)=="ogs5_bc"){
      stop(paste(x, " is not of class 'ogs5_bc' "), call. = FALSE)
   }
   
}

valid_ogs5_bc_condition <- function(x){
   
   if (!class(x)=="ogs5_bc_condition"){
      stop(paste(x, " is not of class 'ogs5_bc_condition' "), call. = FALSE)
   }
   
}
