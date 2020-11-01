
input_add_ic_bloc <-

   function(
      x = list(),
      ic_name = character(NULL),
      COMP_NAME = NULL,
      DIS_TYPE = character(NULL),
      GEO_TYPE = character(NULL),
      PCS_TYPE = character(NULL),
      PRIMARY_VARIABLE = character(NULL)

   ){

      # validate input
      valid_ogs5(x)

      # look if ogs5-obj$input$ic exists and valid, otherwise create
      if (!("ic" %in% names(x$input))) {
         x$input$ic <- create_ogs5_ic()
      } else {

         valid_ogs5_ic(x$input$ic)

         if (ic_name %in% names(x$input$ic)) {
            stop("ic_name does already exist", call. = FALSE)
         }

         if (PCS_TYPE!="MASS_TRANSPORT" &&
             PCS_TYPE %in% sapply(x$input$ic, "[[", 1)
         ) {
            stop("PCS_TYPE does already exist", call. = FALSE)
         }
      }

      # create and add sublist to ic-list

      x$input$ic[[paste(ic_name)]] <- list(

         "COMP_NAME" = COMP_NAME,
         "DIS_TYPE" = DIS_TYPE,
         "GEO_TYPE" = GEO_TYPE,
         "PCS_TYPE" = PCS_TYPE,
         "PRIMARY_VARIABLE" =  PRIMARY_VARIABLE

      ) %>%
         purrr::discard(is.null) %>%
         purrr::discard(BBmisc::isFALSE) %>%
         structure(class = "ogs5_ic_condition")

      valid_ogs5_ic_condition(x$input$ic[[paste(ic_name)]])

      return(x)

   }
