# function to add a complete subkeyword bloc for the IPQC interface
# input: ogs5-obj
# output: updated ogs5-obj

input_add_pqc_skeybloc <- 
  
  function(
    x = list(),
    skey = c("EQUILIBRIUM_PHASES",
             "EXCHANGE",
             "GAS_PHASE",
             "ISOTOPES",
             "KINETICS",
             "KNOBS",
             "RATES",
             "REACTION",
             "SOLID_SOLUTION",
             "SOLUTION",
             "SURFACE"
            ),
    bloc_text = character(NULL)
    
  ){
    
    bloc_name <- skey
    
    # match function arguments
    skey <- match.arg(skey)
    
    # validate input
    valid_ogs5(x)
    
    # look if ogs5-obj$input$pqc exists and valid, otherwise create 
    if (!("pqc" %in% names(x$input))) {
      x$input$pqc <- create_ogs5_pqc() 
    } else {
      
      valid_ogs5_pqc(x$input$pqc)
      
      if (bloc_name %in% names(x$input$pqc)) {
        stop("similar keyword bloc already defined", call. = FALSE)
      }
    }
    
    # create and add sublist to pqc-list
    x$input$pqc[[paste(bloc_name)]] <- bloc_text %>% structure(class = "ogs5_pqc_skeybloc")
    
    valid_ogs5_pqc_skeybloc(x$input$pqc[[paste(bloc_name)]])
    
    return(x)
    
  }