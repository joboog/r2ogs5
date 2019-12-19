# validates list for gli and gli_geometry class

create_ogs5_gli <- function(){
   
   list() %>% structure(class = "ogs5_gli")
   
}

valid_ogs5_gli <- function(x){
   
   if (!class(x)=="ogs5_gli"){
      stop(paste(x, " is not of class 'ogs5_gli' "), call. = FALSE)
   }
   
}

valid_ogs5_gli_points <- function(x){
   
   if (!(class(x) %in% c("tbl", "tbl_df", "data.frame") %>% all())){
      stop(paste(x, " is not of class 'tbl' "), call. = FALSE)
   } %>% any(
      
   )
   
   if (!(length(x[1,])<=4 && 
       (x %>% names %in% c("x", "y", "z", "name") %>% all()))
       ){
      stop(paste(x, " column names do not fit to 'x, y, z, name' "))
   }
   
}

valid_ogs5_gli_ply <- function(x){
   
   if (!class(x)=="ogs5_gli_polyline"){
      stop(paste(x, " is not of class 'ogs5_gli_polyline' "), call. = FALSE)
   }
   
}

valid_ogs5_gli_point <- function(x){
   
   if (!class(x)=="ogs5_gli_surface"){
      stop(paste(x, " is not of class 'ogs5_gli_surface' "), call. = FALSE)
   }
   
}
