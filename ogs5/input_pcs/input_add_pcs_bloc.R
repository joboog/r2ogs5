# add pcs-bloc to ogs5-obj

# steps:
# look if sub-list names pcs exists in input list
# if not, define a sub-list in input names pcs
# add pcs-blocs as additional list


input_add_pcs_bloc <- function(
                        x = list(),
                        PROCESS = character(),
                        PRIMARY_VARIABLE = character(),
                        APP_TYPE = NULL,
                        BOUNDARY_CONDITION_OUTPUT = FALSE
                      ){
  
    # validate input
    if (!class(x)=="ogs5") {
      stop("x is not of class 'ogs5' ", call. = FALSE)
    }
  
    # look if ogs5-obj$input$pcs exists, otherwise create 
    if (!("pcs" %in% names(mod3$input))) {
        ogs5_obj$input$pcs <- lists() 
    }
    
    # now validate eac input argutment and add to pcs-list
}