# validates list for gli and gli_geometry class

#' create_ogs5_gli
#' @description Constructor for the *ogs5_gli* class
#' @return Object of class *ogs5_gli*
create_ogs5_gli <- function(){

   list() %>% structure(class = "ogs5_gli")

}

#' valid_ogs5_gli
#' @description  Validator for *ogs5_gli* base class
#' @param x Object of class *ogs5_gli* to validate.
valid_ogs5_gli <- function(x){

   if (!class(x)=="ogs5_gli"){
      stop(paste(x, " is not of class 'ogs5_gli' "), call. = FALSE)
   }
}

#' valid_ogs5_gli_points
#' @description  Validator for ogs5_gli_points
#' @param x Object of class c("tbl", "tbl_df", "data.frame").
valid_ogs5_gli_points <- function(x){

   if (!(class(x) %in% c("tbl", "tbl_df", "data.frame") %>% all())){
      stop(paste(x, " is not of class 'tbl' "), call. = FALSE)
   } %>% any(

   )

   if (!(length(x[1,])<=5 &&
       (x %>% names %in% c("pnt_id", "x", "y", "z", "name") %>% all()))
       ){
      stop(paste(x, " column names do not fit to 'x, y, z, name' "))
   }
}

#' create_ogs5_gli_polylines
#' @description Constructor for the *ogs5_gli_polylines* class
#' @return Object of class *ogs5_gli_polylines*
create_ogs5_gli_polylines <- function(){

   list() %>% structure(class = "ogs5_gli_polylines")

}

#' valid_ogs5_gli_polylines
#' @description  Validator for *ogs5_gli_polylines* base class
#' @param x Object of class *ogs5_gli_polylines* to validate.
valid_ogs5_gli_polylines <- function(x){

   if (!class(x)=="ogs5_gli_polylines"){
      stop(paste(x, " is not of class 'ogs5_gli_polylines' "), call. = FALSE)
   }
}

#' valid_ogs5_gli_polyline
#' @description  Validator for *ogs5_gli_polyline* base class
#' @param x Object of class *ogs5_gli_polyline* to validate.
valid_ogs5_gli_polyline <- function(x){

   if (!class(x)=="ogs5_gli_polyline"){
      stop(paste(x, " is not of class 'ogs5_gli_polyline' "), call. = FALSE)
   }
}

#' create_ogs5_gli_surfaces
#' @description Constructor for the *ogs5_gli_surfaces* class
#' @return Object of class *ogs5_gli_surfaces*
create_ogs5_gli_surfaces <- function(){

   list() %>% structure(class = "ogs5_gli_surfaces")

}

#' valid_ogs5_gli_surfaces
#' @description  Validator for *ogs5_gli_surfaces* base class
#' @param x Object of class *ogs5_gli_surfaces* to validate.
valid_ogs5_gli_surfaces <- function(x){

   if (!class(x)=="ogs5_gli_surfaces"){
      stop(paste(x, " is not of class 'ogs5_gli_surfaces' "), call. = FALSE)
   }}

#' valid_ogs5_gli_surface
#' @description  Validator for *ogs5_gli_surface* base class
#' @param x Object of class *ogs5_gli_surface* to validate.
valid_ogs5_gli_surface <- function(x){

   if (!class(x)=="ogs5_gli_surface"){
      stop(paste(x, " is not of class 'ogs5_gli_surface' "), call. = FALSE)
   }
}
