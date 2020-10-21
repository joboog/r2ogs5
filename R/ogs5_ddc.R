# validates list for bc and bc_initialcond class

create_ogs5_ddc <- function(){

    list() %>% structure(class = "ogs5_ddc")

}

valid_ogs5_ddc <- function(x){

    if (!class(x)=="ogs5_ddc"){
        stop(paste(x, " is not of class 'ogs5_ddc' "), call. = FALSE)
    }

}

valid_ogs5_ddc_bloc <- function(x){

    if (!class(x)=="ogs5_ddc_bloc"){
        stop(paste(x, " is not of class 'ogs5_ddc_bloc' "), call. = FALSE)
    }

}
