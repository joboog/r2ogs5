# functions to add gli geometric entities
# input: ogs5-obj
# output: updated ogs5-obj

input_add_gli_points <- 
   
   function(
      x = list(),
      ogs5_points = tibble() # x y z name
   ){
      
      # validate input
      valid_ogs5(x)
      
      # look if ogs5-obj$input$gli exists and valid, otherwise create 
      if (!("gli" %in% names(x$input))) {
         x$input$gli <- create_ogs5_gli() 
      } else {
         
         valid_ogs5_gli(x$input$gli)
      }
      
      if ("points" %in% names(x$input$gli)) {
         stop("points do already exist", call. = FALSE)
      }
      
      if (!(ogs5_points$x %>% is.numeric())){
         stop("ogs5_points$x is not of type 'numeric' ", call. = FALSE)
      }
      
      if (!(ogs5_points$y %>% is.numeric())){
         stop("ogs5_points$y is not of type 'numeric' ", call. = FALSE)
      }
      
      if (!(ogs5_points$z %>% is.numeric())){
         stop("ogs5_points$z is not of type 'numeric' ", call. = FALSE)
      }
      
      if (!(ogs5_points$name %>% is.character())){
         stop("ogs5_points$name is not of type 'character' ", call. = FALSE)
      }
      
      if (ogs5_points %>% select(x,y,z) %>% is.na() %>%  any()){
         stop("ogs5_points coordinates contain NA ", call. = FALSE)
      }
      
      # create and add sublist to gli-list
      df <- ogs5_points %>% 
               select(x, y, z, name) %>% 
               as_tibble()
      rownames(df) <- df %>% rownames() %>% as.numeric() %>% -1 
      
      x$input$gli$points <- df
      
      valid_ogs5_gli_points(x$input$gli$points)
      
      return(x)
      
   }


input_add_gli_polyline <- 
   
   function(
      x = list(),
      EPSILON = character(),
      ID = NULL,
      MAT_GROUP = NULL,
      ply_name = character(),
      POINTS = character(),
      POINT_VECTOR = NULL,
      TYPE = NULL
   ){
      
      # validate input
      valid_ogs5(x)
      
      # look if ogs5-obj$input$gli exists and valid, otherwise break 
      if (!("gli" %in% names(x$input))) {
         stop("you have to add points first", call. = FALSE)
      } else {
         valid_ogs5_gli(x$input$gli)
      }
      
      if (!("points" %in% names(x$input$gli))) {
         stop("you have to add points first", call. = FALSE)
      }
      
      # check if POINTS exist
      ply_points <- str_split(POINTS, " ") %>% .[[1]] %>% as.numeric()
      if (max(ply_points) > length(x$input$gli$points[,1])) {
         stop("required POINTS missing", call. = FALSE)
      }
      
      # check if ogs5-obj$input$gli$polylines exists and validate, otherwise create 
      if (!("polylines" %in% names(x$input$gli))) {
         x$input$gli$polylines <- create_ogs5_gli_polylines()
      } else {
         valid_ogs5_gli_polylines(x$input$gli$polylines)
      }
      
      if (ply_name %in% names(x$input$gli$polylines)) {
         stop("Name alreaedy exists", call. = FALSE)
      }
      
      # create and add sublist to polylines-list
      x$input$gli$polylines[[paste(ply_name)]] <- list(
         
         "EPSILON" = EPSILON,
         "ID" = ID,
         "MAT_GROUP" = MAT_GROUP,
         "NAME" = ply_name,
         "POINTS" = POINTS,
         "POINT_VECTOR" = POINT_VECTOR,
         "TYPE" = TYPE
         )%>% 
         purrr::discard(is.null) %>% 
         purrr::discard(isFALSE) %>% 
         structure(class = "ogs5_gli_polyline")
      
      valid_ogs5_gli_polyline(x$input$gli$polylines[[paste(ply_name)]])
      
      return(x)
      
   }


input_add_gli_surface <- 
   
   function(
      x = list(),
      EPSILON = character(),
      ID = NULL,
      MAT_GROUP = NULL,
      srf_name = character(),
      POLYLINES = character(),
      TYPE = NULL,
      TIN = NULL
      
   ){
      
      # validate input
      valid_ogs5(x)
      
      # look if ogs5-obj$input$gli exists and valid, otherwise break 
      if (!("gli" %in% names(x$input))) {
         stop("you have to add points first", call. = FALSE)
      } else {
         valid_ogs5_gli(x$input$gli)
      }
      
      if (!("points" %in% names(x$input$gli))) {
         stop("you have to add points first", call. = FALSE)
      }
      
      # check if ogs5_gli_polylines exist
      if (!("polylines" %in% names(x$input$gli))) {
         stop("you have to add polylines first", call. = FALSE)
      }
      
      # check if the required polylines exist
      plys <- str_split(POLYLINES, " ") %>% .[[1]]
      if (!(plys %in% names(x$input$gli$polylines) %>% all())){
         stop("required POLYLINES missing", call. = FALSE)
      }
      
      # check if ogs5-obj$input$gli$surfaces exists and validate, otherwise create 
      if (!("surfaces" %in% names(x$input$gli))) {
         x$input$gli$surfaces <- create_ogs5_gli_surfaces()
      } else {
         valid_ogs5_gli_surfaces(x$input$gli$surfaces)
      }
      
      if (srf_name %in% names(x$input$gli$surfaces)) {
         stop("Name alreaedy exists", call. = FALSE)
      }
      
      # create and add sublist to surfaces-list
      x$input$gli$surfaces[[paste(srf_name)]] <- list(
         
         "EPSILON" = EPSILON,
         "ID" = ID,
         "MAT_GROUP" = MAT_GROUP,
         "NAME" = srf_name,
         "POLYLINES" = POLYLINES,
         "TYPE" = TYPE,
         "TIN" = TIN
      )%>% 
         purrr::discard(is.null) %>% 
         purrr::discard(isFALSE) %>% 
         structure(class = "ogs5_gli_surface")
      
      valid_ogs5_gli_surface(x$input$gli$surfaces[[paste(srf_name)]])
      
      return(x)
      
   }
