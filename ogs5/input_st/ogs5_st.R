# validates list for st and st_initialcond class

create_ogs5_st <- function(){
   
   list() %>% structure(class = "ogs5_st")
   
}

valid_ogs5_st <- function(x){
   
   if (!class(x)=="ogs5_st"){
      stop(paste(x, " is not of class 'ogs5_st' "), call. = FALSE)
   }
   
}

valid_ogs5_st_condition <- function(x){
   
   if (!class(x)=="ogs5_st_condition"){
      stop(paste(x, " is not of class 'ogs5_st_condition' "), call. = FALSE)
   }
   
}
