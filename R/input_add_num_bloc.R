
input_add_num_bloc <-

   function(
      x = list(),
      num_name = character(NULL),
      COUPLED_PROCESS = NULL,
      COUPLING_CONTROL = NULL,
      COUPLING_ITERATIONS = NULL,
      DYNAMIC_DAMPING = NULL,
      ELE_GAUSS_POINTS = NULL,
      ELE_MASS_LUMPING = NULL,
      ELE_SUPG = NULL,
      ELE_UPWINDING = NULL,
      EXTERNAL_SOLVER_OPTION = NULL,
      FEM_FCT = NULL,
      GRAVITY_PROFILE = NULL,
      LINEAR_SOLVER = character(NULL),
      LOCAL_PICARD1 = NULL,
      NON_LINEAR_ITERATION = NULL,
      NON_LINEAR_SOLVER = NULL,
      NON_LINEAR_UPDATE_VELOCITY = NULL,
      OVERALL_COUPLING = NULL,
      PCS_TYPE = NUcharacter(NULL),
      PLASTICITY_TOLERANCE = NULL,
      RENUMBER = NULL,
      TIME_STEPS  = NULL

   ){
      # validate input
      valid_ogs5(x)

      # look if ogs5-objinput$num exists and valid, otherwise create
      if (!("num" %in% names(x$input))) {
         x$input$num <- create_ogs5_num()
      } else {

         valid_ogs5_num(x$input$num)

         if (num_name %in% names(x$input$num)) {
            stop("num_name does already exist", call. = FALSE)
         }

         if (PCS_TYPE %in% sapply(x$input$bc, "[[", 1)) {
            stop("PCS_TYPE does already exist", call. = FALSE)
         }

      }

      # create and add sublist to num-list

      x$input$num[[paste(num_name)]] <- list(

         "COUPLED_PROCESS" = COUPLED_PROCESS,
         "COUPLING_CONTROL" = COUPLING_CONTROL,
         "COUPLING_ITERATIONS" = COUPLING_ITERATIONS,
         "DYNAMIC_DAMPING" = DYNAMIC_DAMPING,
         "ELE_GAUSS_POINTS" = ELE_GAUSS_POINTS,
         "ELE_MASS_LUMPING" = ELE_MASS_LUMPING,
         "ELE_SUPG" = ELE_SUPG,
         "ELE_UPWINDING" = ELE_UPWINDING,
         "EXTERNAL_SOLVER_OPTION" = EXTERNAL_SOLVER_OPTION,
         "FEM_FCT" = FEM_FCT,
         "GRAVITY_PROFILE" = GRAVITY_PROFILE,
         "LINEAR_SOLVER"= LINEAR_SOLVER ,
         "LOCAL_PICARD1" = LOCAL_PICARD1,
         "NON_LINEAR_ITERATION" = NON_LINEAR_ITERATION,
         "NON_LINEAR_SOLVER" = NON_LINEAR_SOLVER,
         "NON_LINEAR_UPDATE_VELOCITY" = NON_LINEAR_UPDATE_VELOCITY,
         "OVERALL_COUPLING" = OVERALL_COUPLING,
         "PCS_TYPE" = PCS_TYPE,
         "PLASTICITY_TOLERANCE" = PLASTICITY_TOLERANCE,
         "RENUMBER" = RENUMBER,
         "TIME_STEPS"  = TIME_STEPS

      ) %>%
         purrr::discard(is.null) %>%
         purrr::discard(BBmisc::isFALSE) %>%
         structure(class = "ogs5_num_bloc")

      valid_ogs5_num_bloc(x$input$num[[paste(num_name)]])

      return(x)

   }
