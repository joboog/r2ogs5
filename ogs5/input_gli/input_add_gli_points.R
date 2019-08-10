# functions to add gli geometric entities
# input: ogs5-obj
# output: updated ogs5-obj

input_add_gli_points <- 
   
   function(
      x = list(),
      ogs5_points = tibble()
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
      
      # create and add sublist to bc-list
      
      x$input$gli$points <- 
         
         ogs5_points %>% 
         select(x, y, z, name) %>% 
         as_tibble()
         #structure(class = "ogs5_gli_points")
      
      valid_ogs5_gli_points(x$input$gli$points)
      
      return(x)
      
   }