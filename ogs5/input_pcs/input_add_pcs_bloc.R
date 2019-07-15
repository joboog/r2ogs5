# add pcs-bloc to ogs5-obj

# steps:
# look if sub-list names pcs exists in input list
# if not, define a sub-list in input names pcs
# add pcs-blocs as additional list


input_add_pcs_bloc <- function(
                        x = list(),
                        pcs_name = character(),
                        PCS_TYPE = character(),
                        PRIMARY_VARIABLE = NULL,
                        APP_TYPE = NULL,
                        BOUNDARY_CONDITION_OUTPUT = FALSE
                      ){
  
    # validate input
    if (!class(x)=="ogs5") {
      stop("x is not of class 'ogs5' ", call. = FALSE)
    }
  
    # look if ogs5-obj$input$pcs exists, otherwise create 
    if (!("pcs" %in% names(x$input))) {
       x$input$pcs <- list() 
    } else {
       
       if (pcs_name %in% names(x$input$pcs)) {
          stop("pcs_name does already exist", call. = FALSE)
       }
       
       if (PCS_TYPE!="MASS_TRANSPORT" &&
           PCS_TYPE %in% sapply(x$input$pcs, "[[", 1)
          ) {
          stop("PCS_TYPE does already exist", call. = FALSE)
       }
    }
    
    # add sublist to pcs-list
    x$input$pcs[[paste(pcs_name)]] <- list(
    
       "PCS_TYPE" = PCS_TYPE,
       "PRIMARY_VARIABLE" =  PRIMARY_VARIABLE,
       "APP_TYPE" = APP_TYPE,
       "BOUNDARY_CONDITION_OUTPUT" = BOUNDARY_CONDITION_OUTPUT
    
       )

    return(x)
   
}