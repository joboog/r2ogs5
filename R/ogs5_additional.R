# validates list for pqc and pqc_bloc class

create_ogs5_additional <- function(){

    list() %>% structure(class = "ogs5_additional")

}

valid_ogs5_additional <- function(x){

    if (!class(x)=="ogs5_additional"){
        stop(paste(x, " is not of class 'ogs5_additional' "), call. = FALSE)
    }

}

valid_ogs5_additional_bloc <- function(x){

    if (!class(x)=="ogs5_additional_bloc"){
        stop(paste(x, " is not of class 'ogs5_additional_bloc' "), call. = FALSE)
    }

}
